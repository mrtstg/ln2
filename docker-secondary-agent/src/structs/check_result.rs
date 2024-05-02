use serde::*;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
pub struct StandCheckResult {
    pub score: u32,
    #[serde(rename = "maxScore")]
    pub max_score: u32,
    pub values: HashMap<String, Value>,
}

impl Default for StandCheckResult {
    fn default() -> Self {
        StandCheckResult {
            score: 0,
            max_score: 0,
            values: HashMap::new(),
        }
    }
}
