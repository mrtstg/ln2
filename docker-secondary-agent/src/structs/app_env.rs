use std::env;

#[derive(Debug, Clone)]
pub struct AppEnvironment {
    pub rabbit_user: String,
    pub rabbit_pass: String,
    pub rabbit_host: String,
    pub rabbit_port: u16,
    pub agent_debug: bool,
    pub docker_url: String,
}

pub fn get_app_environment() -> AppEnvironment {
    let rabbit_user = env::var("RABBITMQ_DEFAULT_USER").expect("Failed to get rabbitmq user!");
    let rabbit_pass = env::var("RABBITMQ_DEFAULT_PASS").expect("Failed to get rabbitmq pass!");
    let rabbit_host = env::var("RABBITMQ_HOST").expect("Failed to get rabbitmq host!");
    let rabbit_port = env::var("RABBITMQ_PORT")
        .expect("Failed to get rabbitmq port!")
        .parse::<u16>()
        .expect("Failed to parse rabbitmq port!");
    let agent_debug_res = env::var("AGENT_DEBUG");
    let agent_debug: bool = match agent_debug_res {
        Ok(v) => v == "1",
        Err(_) => false,
    };
    let docker_url = env::var("DOCKER_SOCKET_URL").expect("Failed to get docker socket URL!");
    AppEnvironment {
        rabbit_user,
        rabbit_pass,
        rabbit_host,
        rabbit_port,
        agent_debug,
        docker_url,
    }
}
