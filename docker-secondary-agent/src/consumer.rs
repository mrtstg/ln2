use crate::deploy::docker::*;
use crate::structs::app_env::{self, AppEnvironment};
use crate::structs::check_result::StandCheckResult;
use crate::structs::queue_response::QueueTaskResponse;
use crate::structs::queue_task::QueueTask;
use amqprs::channel::{self, BasicPublishArguments, Channel};
use amqprs::BasicProperties;
use amqprs::{channel::BasicAckArguments, consumer::AsyncConsumer};
use async_std::future::timeout;
use async_trait::async_trait;
use docker_api::docker::Docker;
use log::*;
use serde_json::from_str;
use std::time::Duration;

pub struct RabbitConsumer {
    pub docker_client: Docker,
    pub app_env: AppEnvironment,
    pub publish_args: BasicPublishArguments,
}

impl RabbitConsumer {
    pub fn new(
        app_env: AppEnvironment,
        publish_args: BasicPublishArguments,
        docker_client: Docker,
    ) -> Self {
        RabbitConsumer {
            app_env,
            publish_args,
            docker_client,
        }
    }
}

async fn update_task_status(
    channel: &Channel,
    publish_args: BasicPublishArguments,
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
            let content = string.as_bytes().to_vec();
            match channel
                .basic_publish(BasicProperties::default(), content, publish_args)
                .await
            {
                Ok(_) => return Ok(()),
                Err(e) => return Err(e.to_string()),
            }
        }
        Err(e) => return Err(e.to_string()),
    }
}

#[async_trait]
impl AsyncConsumer for RabbitConsumer {
    async fn consume<'life0, 'life1>(
        &'life0 mut self,
        channel: &'life1 amqprs::channel::Channel,
        deliver: amqprs::Deliver,
        _basic_properties: amqprs::BasicProperties,
        content: Vec<u8>,
    ) where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        let data = String::from_utf8(content);
        let _ = channel
            .basic_ack(BasicAckArguments::new(deliver.delivery_tag(), false))
            .await;
        match data {
            Err(e) => error!("Failed to serialize message into string: {:?}", e),
            Ok(message) => {
                let parse_res: Result<QueueTask, serde_json::Error> = from_str(message.as_str());
                match parse_res {
                    Ok(queue_task) => {
                        let _ = update_task_status(
                            channel,
                            self.publish_args.clone(),
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
                                    channel,
                                    self.publish_args.clone(),
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
                                    ),
                                )
                                .await
                                {
                                    Ok((_, res)) => {
                                        let _ = update_task_status(
                                            channel,
                                            self.publish_args.clone(),
                                            queue_task.uuid.clone(),
                                            "finished".to_string(),
                                            Some(res),
                                        )
                                        .await;
                                    }
                                    Err(_) => {
                                        error!("Timeout error as task {}", queue_task.uuid.clone());
                                        let _ = update_task_status(
                                            channel,
                                            self.publish_args.clone(),
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
                            Err(_) => {
                                let _ = update_task_status(
                                    channel,
                                    self.publish_args.clone(),
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
