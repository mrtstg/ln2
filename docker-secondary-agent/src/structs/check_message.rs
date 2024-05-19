use serde::*;

#[derive(Serialize, Deserialize)]
pub enum CheckBlockType {
    #[serde(rename = "code")]
    Code,
    #[serde(rename = "message")]
    Message,
}

#[derive(Serialize, Deserialize)]
pub struct CheckMessageBlock {
    #[serde(rename = "type")]
    pub block_type: CheckBlockType,
    pub content: String,
}

impl CheckMessageBlock {
    pub fn new_message(content: String) -> Self {
        CheckMessageBlock {
            content,
            block_type: CheckBlockType::Message,
        }
    }

    pub fn new_code(content: String) -> Self {
        CheckMessageBlock {
            content,
            block_type: CheckBlockType::Code,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct CheckMessage {
    pub title: String,
    pub blocks: Vec<CheckMessageBlock>,
}

impl Default for CheckMessage {
    fn default() -> Self {
        CheckMessage {
            title: String::new(),
            blocks: Vec::new(),
        }
    }
}

impl CheckMessage {
    pub fn new(title: String) -> Self {
        CheckMessage {
            title,
            blocks: Vec::new(),
        }
    }
}
