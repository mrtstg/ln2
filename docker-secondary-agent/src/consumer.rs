use crate::deploy::docker::*;
use crate::structs::app_env::AppEnvironment;
use crate::structs::check_result::{StandCheckEnum, StandCheckResult};
use crate::structs::queue_response::QueueTaskResponse;
use crate::structs::queue_task::QueueTask;
use async_std::future::timeout;
use docker_api::docker::Docker;
use lapin::message::Delivery;
use lapin::options::{BasicAckOptions, BasicPublishOptions};
use lapin::{BasicProperties, Connection};
use log::*;
use serde_json::from_str;
use std::time::Duration;

pub struct RabbitConsumer {
    pub docker_client: Docker,
    pub app_env: AppEnvironment,
}

impl RabbitConsumer {
    pub fn new(app_env: AppEnvironment, docker_client: Docker) -> Self {
        RabbitConsumer {
            app_env,
            docker_client,
        }
    }
}

async fn update_task_status(
    connection: &Connection,
    uuid: String,
    status: String,
    result: Option<StandCheckResult>,
) -> Result<(), String> {
    let data = QueueTaskResponse {
        uuid: uuid.clone(),
        status: status.clone(),
        result,
    };
    info!("Task {}: state {}", uuid, status);
    match serde_json::to_string(&data) {
        Ok(string) => {
            let channel_res = connection.create_channel().await;
            if let Err(e) = channel_res {
                error!("Failed to open channel: {:?}", e);
                return Err(e.to_string());
            }
            let channel = channel_res.unwrap();
            let content = string.as_bytes();
            match channel
                .basic_publish(
                    "resultsExchange",
                    "",
                    BasicPublishOptions::default(),
                    content,
                    BasicProperties::default(),
                )
                .await
            {
                Ok(_) => return Ok(()),
                Err(e) => {
                    error!("Task update error: {:?}", e);
                    return Err(e.to_string());
                }
            }
        }
        Err(e) => return Err(e.to_string()),
    }
}

impl RabbitConsumer {
    pub async fn consume(&self, delivery: Delivery, conn: &Connection) {
        let data = String::from_utf8(delivery.data.clone());
        let chan_res = conn.create_channel().await;
        if let Err(ref e) = chan_res {
            error!("Failed to open channel: {:?}", e);
        }
        let _ = delivery.ack(BasicAckOptions::default()).await;
        match data {
            Err(e) => error!("Failed to serialize message into string: {:?}", e),
            Ok(message) => {
                let parse_res: Result<QueueTask, serde_json::Error> = from_str(message.as_str());
                match parse_res {
                    Ok(queue_task) => {
                        let _ = update_task_status(
                            conn,
                            queue_task.uuid.clone(),
                            "taken".to_string(),
                            None,
                        )
                        .await;
                        match deploy_stand(
                            self.docker_client.clone(),
                            queue_task.uuid.clone(),
                            queue_task.uuid.clone(),
                            queue_task.stand.clone(),
                        )
                        .await
                        {
                            Ok((network, containers)) => {
                                let _ = update_task_status(
                                    conn,
                                    queue_task.uuid.clone(),
                                    "processing".to_string(),
                                    None,
                                )
                                .await;
                                let mut check_actions = Vec::from(queue_task.stand.actions);
                                check_actions.extend(queue_task.check);
                                match timeout(
                                    Duration::from_secs(60),
                                    execute_stand_check(
                                        self.docker_client.clone(),
                                        &containers,
                                        check_actions,
                                        StandCheckResult::default(),
                                        Vec::from([0]),
                                    ),
                                )
                                .await
                                {
                                    Ok(res) => match res {
                                        // TODO: refactor this shi
                                        StandCheckEnum::Ok(result) => {
                                            let _ = update_task_status(
                                                conn,
                                                queue_task.uuid.clone(),
                                                "finished".to_string(),
                                                Some(result),
                                            )
                                            .await;
                                        }
                                        StandCheckEnum::Cancelled(result) => {
                                            let _ = update_task_status(
                                                conn,
                                                queue_task.uuid.clone(),
                                                "cancelled".to_string(),
                                                Some(result),
                                            )
                                            .await;
                                        }
                                        StandCheckEnum::Accepted(result) => {
                                            let _ = update_task_status(
                                                conn,
                                                queue_task.uuid.clone(),
                                                "accepted".to_string(),
                                                Some(result),
                                            )
                                            .await;
                                        }
                                    },
                                    Err(_) => {
                                        error!("Timeout error as task {}", queue_task.uuid.clone());
                                        let _ = update_task_status(
                                            conn,
                                            queue_task.uuid.clone(),
                                            "timeout".to_string(),
                                            None,
                                        )
                                        .await;
                                    }
                                }
                                destroy_stand(network, containers.values().map(|v| v).collect())
                                    .await;
                            }
                            Err(e) => {
                                error!("Failed to finish task: {}", e);
                                let _ = update_task_status(
                                    conn,
                                    queue_task.uuid,
                                    "error".to_string(),
                                    None,
                                )
                                .await;
                            }
                        }
                    }
                    Err(e) => {
                        error!("Failed to deserialize task: {}", e);
                    }
                }
            }
        }
    }
}
