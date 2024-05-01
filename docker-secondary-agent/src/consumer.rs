use crate::structs::app_env::{self, AppEnvironment};
use crate::structs::queue_task::QueueTask;
use amqprs::{channel::BasicAckArguments, consumer::AsyncConsumer};
use async_trait::async_trait;
use serde_json::from_str;

pub struct RabbitConsumer {
    pub app_env: AppEnvironment,
}

impl RabbitConsumer {
    pub fn new(app_env: AppEnvironment) -> Self {
        RabbitConsumer { app_env }
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
                        println!("{:?}", queue_task.stand);
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
