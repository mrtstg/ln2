use serde::*;

use super::check_result::StandCheckResult;

#[derive(Serialize, Deserialize)]
pub struct QueueTaskResponse {
    pub uuid: String,
    pub status: String,
    pub result: Option<StandCheckResult>,
}
