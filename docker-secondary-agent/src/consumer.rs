use crate::structs::app_env::{self, AppEnvironment};
use crate::structs::queue_response::QueueTaskResponse;
use crate::structs::queue_task::QueueTask;
use amqprs::channel::{self, BasicPublishArguments, Channel};
use amqprs::BasicProperties;
use amqprs::{channel::BasicAckArguments, consumer::AsyncConsumer};
use async_trait::async_trait;
use serde_json::from_str;

pub struct RabbitConsumer {
    pub app_env: AppEnvironment,
    pub publish_args: BasicPublishArguments,
}

impl RabbitConsumer {
    pub fn new(app_env: AppEnvironment, publish_args: BasicPublishArguments) -> Self {
        RabbitConsumer {
            app_env,
            publish_args,
        }
    }
}

async fn update_task_status(
    channel: &Channel,
    publish_args: BasicPublishArguments,
    uuid: String,
    status: String,
) -> Result<(), String> {
    let data = QueueTaskResponse {
        uuid,
        status,
        result: None,
    };
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
        basic_properties: amqprs::BasicProperties,
        content: Vec<u8>,
    ) where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        let data = String::from_utf8(content);
        match data {
            Err(e) => println!("Failed to serialize message into string: {:?}", e),
            Ok(message) => {
                let parse_res: Result<QueueTask, serde_json::Error> = from_str(message.as_str());
                match parse_res {
                    Ok(queue_task) => {
                        update_task_status(
                            channel,
                            self.publish_args.clone(),
                            queue_task.uuid,
                            "taken".to_string(),
                        )
                        .await;
                    }
                    Err(e) => {
                        println!("Failed to deserialize task: {}", e);
                    }
                }
            }
        }
        channel
            .basic_ack(BasicAckArguments::new(deliver.delivery_tag(), false))
            .await
            .unwrap();
    }
}
