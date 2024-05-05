use containers_api::conn::tty::TtyChunk;
use docker_api::api::container::*;
use docker_api::api::network::*;
use docker_api::docker::Docker;
use docker_api::models::EndpointSettings;
use docker_api::models::NetworkingConfig;
use docker_api::opts::ContainerCreateOpts;
use docker_api::opts::ExecCreateOpts;
use docker_api::opts::ExecStartOpts;
use docker_api::opts::NetworkCreateOpts;
use docker_api::Exec;
use futures_util::StreamExt;
use log::*;
use std::collections::HashMap;
use std::time::Duration;
use tokio::time::sleep;

use crate::structs::check_result::StandCheckResult;
use crate::structs::stand_check::StandCheckStage;
use crate::structs::stand_data::StandContainerData;
use crate::structs::stand_data::StandData;

pub async fn create_network(docker: Docker, network_name: String) -> Result<Network, String> {
    let network_api = Networks::new(docker);
    match network_api
        .create(
            &NetworkCreateOpts::builder(network_name)
                .check_duplicate(true)
                .enable_ipv6(false)
                .build(),
        )
        .await
    {
        Ok(n) => Ok(n),
        Err(e) => Err(e.to_string()),
    }
}

pub async fn destroy_stand(network: Network, containers: Vec<&Container>) -> () {
    for container in containers {
        let _ = container.kill(None).await;
        let _ = container.delete().await;
    }
    let _ = network.delete().await;
}

pub async fn execute_docker_command(
    docker: &Docker,
    container: &Container,
    command: Vec<&str>,
) -> Result<(String, String, Option<isize>), String> {
    let exec_instance_res = Exec::create(
        docker.clone(),
        container.id(),
        &ExecCreateOpts::builder()
            .command(command)
            .attach_stdout(true)
            .attach_stderr(true)
            .build(),
    )
    .await;
    match exec_instance_res {
        Err(e) => return Err(e.to_string()),
        Ok(exec_instance) => match exec_instance.start(&ExecStartOpts::default()).await {
            Err(e) => Err(e.to_string()),
            Ok(mut command_res) => {
                let mut stdout: Vec<u8> = Vec::new();
                let mut stderr: Vec<u8> = Vec::new();
                while let Some(data_chunk) = command_res.next().await {
                    match data_chunk {
                        Ok(d) => match d {
                            TtyChunk::StdOut(bytes) => stdout.extend(bytes),
                            TtyChunk::StdErr(bytes) => stderr.extend(bytes),
                            _other => {}
                        },
                        Err(_) => continue,
                    }
                }
                let stdout_str = String::from_utf8(stdout).unwrap_or(String::new());
                let stderr_str = String::from_utf8(stderr).unwrap_or(String::new());
                let exec_details = exec_instance.inspect().await;
                let exit_code = match exec_details {
                    Err(e) => None,
                    Ok(details) => details.exit_code,
                };
                return Ok((stdout_str, stderr_str, exit_code));
            }
        },
    }
}

pub async fn execute_stand_check(
    docker: Docker,
    containers_map: &HashMap<String, Container>,
    actions: Vec<StandCheckStage>,
) -> (HashMap<String, String>, StandCheckResult) {
    let mut variable_stack: HashMap<String, String> = HashMap::new();
    let mut check_res = StandCheckResult::default();
    for action in actions {
        match action {
            StandCheckStage::CopyFile(payload) => {
                if let Some(container) = containers_map.get(payload.container.as_str()) {
                    match container
                        .copy_file_into(payload.clone().file_path, payload.file_content.as_bytes())
                        .await
                    {
                        Ok(_) => {}
                        Err(e) => error!(
                            "Failed to create {} on {}: {:?}",
                            payload.file_path, payload.container, e
                        ),
                    }
                }
            }
            StandCheckStage::ExecuteCommand(payload) => {
                if let Some(container) = containers_map.get(payload.container.as_str()) {
                    match execute_docker_command(
                        &docker.clone(),
                        container,
                        payload.command.split(" ").collect(),
                    )
                    .await
                    {
                        Ok((stdout, stderr, exit_code)) => {
                            if let Some(record_key) = payload.record_into {
                                let record_out = if payload.format_output {
                                    format_command_out(stdout)
                                } else {
                                    stdout
                                };
                                info!(
                                    "Exit code of command {} is {:?}",
                                    payload.command, exit_code
                                );
                                variable_stack.insert(record_key.clone(), record_out.clone());
                                if payload.record_stdout {
                                    check_res
                                        .values
                                        .insert(record_key, serde_json::Value::String(record_out));
                                }
                            }
                        }
                        Err(e) => error!(
                            "Failed to execute command {} on {}: {:?}",
                            payload.command, payload.container, e
                        ),
                    };
                }
            }
            StandCheckStage::CompareResults(payload) => {
                check_res.max_score += payload.score;
                if let Some(first_v) = variable_stack.get(payload.first.as_str()) {
                    if let Some(second_v) = variable_stack.get(payload.second.as_str()) {
                        if first_v == second_v {
                            check_res.score += payload.score;
                        }
                    }
                }
            }
            StandCheckStage::DeclareVariable(payload) => {
                variable_stack.insert(payload.variable_name, payload.variable_value);
            }
        }
    }
    return (variable_stack, check_res);
}

fn format_command_out(out: String) -> String {
    return out
        .replace("\t", " ")
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(" ");
}

pub async fn deploy_stand(
    docker: Docker,
    name_base: String,
    network_name: String,
    stand_data: StandData,
) -> Result<(Network, HashMap<String, Container>), String> {
    let network = create_network(docker.clone(), network_name).await?;
    let mut containers_map: HashMap<String, Container> = HashMap::new();
    for container_data in stand_data.containers {
        let container_api = Containers::new(docker.clone());
        match deploy_container(
            container_api,
            name_base.clone(),
            &network,
            container_data.clone(),
        )
        .await
        {
            Ok(c) => {
                let _ = c.start().await;
                containers_map.insert(container_data.name, c);
                if let Some(timeout) = container_data.timeout {
                    sleep(Duration::from_secs(timeout as u64)).await;
                }
            }
            Err(e) => {
                destroy_stand(network, containers_map.iter().map(|v| v.1).collect()).await;
                return Err(e);
            }
        };
    }

    Ok((network, containers_map))
}

pub async fn deploy_container(
    containers: Containers,
    name_base: String,
    network: &Network,
    container_data: StandContainerData,
) -> Result<Container, String> {
    let mut create_opts = ContainerCreateOpts::builder()
        .name(format!(
            "{}-{}",
            name_base.clone(),
            container_data.name.clone()
        ))
        .image(container_data.image)
        .stop_signal("SIGKILL")
        .volumes(Vec::from([format!(
            "/tmp/{}-{}:/tmp/{}-{}",
            name_base,
            container_data.name.clone(),
            name_base,
            container_data.name.clone()
        )]))
        .log_driver("json-file")
        .log_driver_config(Vec::from([("max-size", "10m")]))
        .network_config(NetworkingConfig {
            endpoints_config: Some(HashMap::from_iter([(
                network.id().to_string(),
                EndpointSettings {
                    aliases: None,
                    driver_opts: None,
                    endpoint_id: None,
                    gateway: None,
                    global_i_pv_6_address: None,
                    global_i_pv_6_prefix_len: None,
                    ipam_config: None,
                    ip_address: None,
                    ip_prefix_len: None,
                    i_pv_6_gateway: None,
                    links: None,
                    mac_address: None,
                    network_id: Some(network.id().to_string()),
                },
            )])),
        });
    if let Some(env) = container_data.environment {
        create_opts = create_opts.env(env.iter().map(|(k, v)| format!("{}={}", k, v)));
    }
    if let Some(cmd) = container_data.command {
        create_opts = create_opts.command(cmd.split(" "));
    }
    if let Some(hostname) = container_data.hostname {
        create_opts = create_opts.hostname(hostname);
    }
    let container_res = containers.create(&create_opts.build()).await;
    match container_res {
        Ok(container) => Ok(container),
        Err(e) => Err(e.to_string()),
    }
}
