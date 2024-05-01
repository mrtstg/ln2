use crate::structs::stand_check::StandCheckStage;
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Deserialize, Debug)]
pub struct StandData {
    pub containers: Vec<StandContainerData>,
    pub actions: Vec<StandCheckStage>,
}

#[derive(Deserialize, Debug)]
pub struct ContainerVolume {
    pub host: String,
    pub path: String,
}

#[derive(Deserialize, Debug)]
pub struct StandContainerData {
    pub name: String,
    pub image: String,
    pub environment: Option<HashMap<String, String>>,
    pub command: Option<String>,
    pub hostname: Option<String>,
    #[serde(default = "default_volumes")]
    pub volumes: Vec<ContainerVolume>,
    pub timeout: Option<u32>,
}

fn default_volumes() -> Vec<ContainerVolume> {
    return Vec::new();
}
