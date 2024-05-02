mod consumer;
mod structs;

use amqprs::{
    channel::{
        BasicConsumeArguments, BasicPublishArguments, QueueBindArguments, QueueDeclareArguments,
    },
    connection::{Connection, OpenConnectionArguments},
};
use consumer::RabbitConsumer;
use dotenv::dotenv;
use env_logger::{self, Env, Target};
use log::{info, warn};
use signal_hook::consts::SIGTERM;
use signal_hook::iterator::Signals;
use std::io::Read;
use structs::app_env::*;

#[tokio::main]
async fn main() {
    dotenv().ok();
    let app_env = get_app_environment();

    env_logger::Builder::from_env(Env::default().default_filter_or(if app_env.agent_debug {
        "debug"
    } else {
        "info"
    }))
    .target(Target::Stdout)
    .init();
    if app_env.agent_debug {
        warn!("!!! AGENT DEBUG ENABLED !!!");
    }

    info!("Opening RabbitMQ connection!");
    let rabbit_conn = Connection::open(&OpenConnectionArguments::new(
        app_env.rabbit_host.as_str(),
        app_env.rabbit_port,
        app_env.rabbit_user.as_str(),
        app_env.rabbit_pass.as_str(),
    ))
    .await
    .unwrap();

    let rabbit_chan = rabbit_conn.open_channel(None).await.unwrap();

    info!("Declaring requestsQueue!");
    let (queue_name, _, _) = rabbit_chan
        .queue_declare(QueueDeclareArguments::durable_client_named("requestsQueue"))
        .await
        .unwrap()
        .unwrap();

    info!("Declaring resultsQueue!");
    let (response_queue_name, _, _) = rabbit_chan
        .queue_declare(QueueDeclareArguments::durable_client_named("resultsQueue"))
        .await
        .unwrap()
        .unwrap();

    rabbit_chan
        .queue_bind(QueueBindArguments::new(
            response_queue_name.as_str(),
            "resultsExchange",
            "",
        ))
        .await
        .unwrap();

    info!("Starting consumer!");
    let args = BasicConsumeArguments::new(queue_name.as_str(), "");
    rabbit_chan
        .basic_consume(
            RabbitConsumer::new(
                app_env.clone(),
                BasicPublishArguments::new("resultsExchange", ""),
            ),
            args,
        )
        .await
        .unwrap();

    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();

    rabbit_chan.close().await.unwrap();
    rabbit_conn.close().await.unwrap();
    return ();
}
