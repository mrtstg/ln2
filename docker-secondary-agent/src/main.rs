mod consumer;
mod structs;

use amqprs::{
    channel::{BasicConsumeArguments, QueueDeclareArguments},
    connection::{Connection, OpenConnectionArguments},
};
use consumer::RabbitConsumer;
use dotenv::dotenv;
use signal_hook::consts::SIGTERM;
use signal_hook::iterator::Signals;
use structs::app_env::*;

#[tokio::main]
async fn main() {
    dotenv().ok();
    let app_env = get_app_environment();

    if app_env.agent_debug {
        println!("!!! AGENT DEBUG ENABLED !!!");
    }

    let rabbit_conn = Connection::open(&OpenConnectionArguments::new(
        app_env.rabbit_host.as_str(),
        app_env.rabbit_port,
        app_env.rabbit_user.as_str(),
        app_env.rabbit_pass.as_str(),
    ))
    .await
    .unwrap();

    //rabbit_conn
    //    .register_callback(DefaultConnectionCallback)
    //    .await
    //    .unwrap();

    let rabbit_chan = rabbit_conn.open_channel(None).await.unwrap();
    //rabbit_chan
    //    .register_callback(DefaultChannelCallback)
    //    .await
    //    .unwrap();

    let (queue_name, _, _) = rabbit_chan
        .queue_declare(QueueDeclareArguments::durable_client_named("requestsQueue"))
        .await
        .unwrap()
        .unwrap();

    let args = BasicConsumeArguments::new(queue_name.as_str(), "");
    rabbit_chan
        .basic_consume(RabbitConsumer::new(app_env.clone()), args)
        .await
        .unwrap();

    let mut signals = Signals::new(&[SIGTERM]).unwrap();
    loop {
        for _ in signals.forever() {
            break;
        }
    }

    return ();
}
