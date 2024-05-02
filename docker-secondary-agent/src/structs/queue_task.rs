use serde::*;

use super::{stand_check::StandCheckStage, stand_data::StandData};

#[derive(Deserialize, Debug, Clone)]
pub struct QueueTask {
    pub uuid: String,
    pub stand: StandData,
    pub check: Vec<StandCheckStage>,
}
