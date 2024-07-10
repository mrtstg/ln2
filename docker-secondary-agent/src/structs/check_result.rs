use crate::structs::check_message::CheckMessage;
use serde::*;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Deserialize, Serialize, Clone)]
pub enum StandCheckEnum {
    Ok(StandCheckResult),
    Cancelled,
    Accepted(StandCheckResult),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct StandCheckResult {
    pub accepted: bool,
    pub score: u32,
    #[serde(rename = "scoreGate")]
    pub score_gate: u32,
    pub values: HashMap<String, Value>,
    pub messages: Vec<CheckMessage>,
}

impl Default for StandCheckResult {
    fn default() -> Self {
        StandCheckResult {
            accepted: false,
            score: 0,
            score_gate: 0,
            values: HashMap::new(),
            messages: Vec::new(),
        }
    }
}
