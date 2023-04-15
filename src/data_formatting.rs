use serde_json::Value;

pub trait Data {
    fn get_json(&self) -> Value;
}