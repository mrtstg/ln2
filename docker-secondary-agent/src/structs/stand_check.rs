use serde::*;

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "action")]
pub enum StandCheckStage {
    #[serde(rename = "copy")]
    CopyFile(CopyFileStage),
    #[serde(rename = "command")]
    ExecuteCommand(ExecuteCommandStage),
    #[serde(rename = "points")]
    AddPoints(AddPointsStage),
    #[serde(rename = "compareVars")]
    CompareVariables(CompareVariablesStage),
    #[serde(rename = "compareStatusCode")]
    CompareLatestStatusCode(CompareLatestStatusCodeStage),
    #[serde(rename = "declare")]
    DeclareVariable(DeclareVariableStage),
    #[serde(rename = "stopCheck")]
    StopCheck,
    #[serde(rename = "displayMessage")]
    DisplayMessage(DisplayMessageStage),
    #[serde(rename = "displayVariable")]
    DisplayVariable(DisplayVariableStage),
    #[serde(rename = "acceptCheck")]
    AcceptCheck,
    #[serde(rename = "setPointsGate")]
    SetPointsGate(SetPointsGateStage),
}

#[derive(Deserialize, Debug, Clone)]
pub struct SetPointsGateStage {
    pub amount: u32,
}

#[derive(Deserialize, Debug, Clone)]
pub struct CopyFileStage {
    pub target: String,
    pub content: String,
    pub path: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ExecuteCommandStage {
    pub target: String,
    pub command: String,
    #[serde(rename = "formatOutput")]
    pub format_output: bool,
    #[serde(rename = "recordInto")]
    pub record_into: Option<String>,
    #[serde(rename = "reportError")]
    pub report_error: bool,
}

#[derive(Deserialize, Debug, Clone)]
pub struct AddPointsStage {
    pub amount: u32,
}

#[derive(Deserialize, Debug, Clone)]
pub struct CompareVariablesStage {
    pub first: String,
    pub second: String,
    #[serde(rename = "positiveActions")]
    pub positive_actions: Vec<StandCheckStage>,
    #[serde(rename = "negativeActions")]
    pub negative_actions: Vec<StandCheckStage>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct CompareLatestStatusCodeStage {
    #[serde(rename = "awaitedStatus")]
    pub awaited_status: isize,
    #[serde(rename = "positiveActions")]
    pub positive_actions: Vec<StandCheckStage>,
    #[serde(rename = "negativeActions")]
    pub negative_actions: Vec<StandCheckStage>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DeclareVariableStage {
    #[serde(rename = "variableName")]
    pub variable_name: String,
    #[serde(rename = "variableValue")]
    pub variable_value: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DisplayMessageStage {
    pub message: String,
    pub title: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DisplayVariableStage {
    #[serde(rename = "variableName")]
    pub variable_name: String,
    pub message: String,
    pub title: String,
}
