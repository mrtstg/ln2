use serde::*;

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "action")]
pub enum StandCheckStage {
    #[serde(rename = "copy")]
    CopyFile(CopyFileStage),
    #[serde(rename = "command")]
    ExecuteCommand(ExecuteCommandStage),
    #[serde(rename = "compareVars")]
    CompareResults(CompareResultsStage),
    #[serde(rename = "declare")]
    DeclareVariable(DeclareVariableStage),
}

#[derive(Deserialize, Debug, Clone)]
pub struct CopyFileStage {
    pub container: String,
    #[serde(rename = "fileContent")]
    pub file_content: String,
    #[serde(rename = "filePath")]
    pub file_path: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ExecuteCommandStage {
    pub container: String,
    pub command: String,
    #[serde(rename = "recordStdout")]
    pub record_stdout: bool,
    #[serde(rename = "formatOutput")]
    pub format_output: bool,
    #[serde(rename = "recordInto")]
    pub record_into: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct CompareResultsStage {
    pub first: String,
    pub second: String,
    pub score: u32,
}

#[derive(Deserialize, Debug, Clone)]
pub struct DeclareVariableStage {
    #[serde(rename = "variableName")]
    pub variable_name: String,
    #[serde(rename = "variableValue")]
    pub variable_value: String,
}
