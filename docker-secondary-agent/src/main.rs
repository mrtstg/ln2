mod consumer;
mod deploy;
mod structs;

use consumer::RabbitConsumer;
use docker_api::docker::Docker;
use dotenv::dotenv;
use env_logger::{self, Env, Target};
use futures_util::StreamExt;
use lapin::{options::*, types::FieldTable, Connection, ConnectionProperties, Result};
use log::{debug, info, warn};
use std::sync::Arc;
use structs::app_env::*;
use tokio::sync::Mutex;

async fn tokio_main() -> Result<()> {
    let app_env = get_app_environment();

    info!("Opening RabbitMQ connection!");
    let rabbit_conn = Connection::connect(
        format!(
            "amqp://{}:{}@{}:{}",
            app_env.rabbit_user, app_env.rabbit_pass, app_env.rabbit_host, app_env.rabbit_port
        )
        .as_str(),
        ConnectionProperties::default(),
    )
    .await?;

    info!("Creating clients!");
    let rabbit_chan = rabbit_conn.create_channel().await?;
    let docker_client = Docker::new(app_env.clone().docker_url).unwrap();
    match docker_client.ping().await {
        Ok(docker_info) => debug!("{:?}", docker_info),
        Err(e) => panic!("{}", e),
    }

    info!("Declaring requestsQueue!");
    let mut queue_options = QueueDeclareOptions::default();
    queue_options.durable = true;
    let _ = rabbit_chan
        .queue_declare(
            "requestsQueue",
            queue_options.clone(),
            FieldTable::default(),
        )
        .await?;

    info!("Declaring resultsQueue!");
    let _ = rabbit_chan
        .queue_declare("resultsQueue", queue_options.clone(), FieldTable::default())
        .await?;

    let _ = rabbit_chan
        .exchange_declare(
            "resultsExchange",
            lapin::ExchangeKind::Direct,
            ExchangeDeclareOptions::default(),
            FieldTable::default(),
        )
        .await;

    let _ = rabbit_chan
        .queue_bind(
            "resultsQueue",
            "resultsExchange",
            "",
            QueueBindOptions::default(),
            FieldTable::default(),
        )
        .await;

    info!("Starting consumer pool!");
    let mut consumer = rabbit_chan
        .basic_consume(
            "requestsQueue",
            "",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let conn_arc = Arc::new(Mutex::new(rabbit_conn));
    let env_arc = Arc::new(Mutex::new(app_env));
    let docker_arc = Arc::new(Mutex::new(docker_client));
    while let Some(delivery) = consumer.next().await {
        let conn_binding = Arc::clone(&conn_arc);
        let env_binding = Arc::clone(&env_arc);
        let docker_binding = Arc::clone(&docker_arc);
        tokio::spawn(async move {
            let delivery = delivery.unwrap();

            let consumer = RabbitConsumer::new(
                env_binding.lock().await.clone(),
                docker_binding.lock().await.clone(),
            );

            let conn = conn_binding.lock().await;
            let _ = consumer.consume(delivery, &*conn).await;
        });
    }
    Ok(())
}

fn main() {
    dotenv().ok();
    let app_env = get_app_environment();
    if app_env.agent_debug {
        warn!("!!! AGENT DEBUG ENABLED !!!");
    }

    env_logger::Builder::from_env(Env::default().default_filter_or(if app_env.agent_debug {
        "debug"
    } else {
        "info"
    }))
    .target(Target::Stdout)
    .init();

    let _ = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .worker_threads(app_env.threads_amount)
        .build()
        .unwrap()
        .block_on(async { tokio_main().await });
}
