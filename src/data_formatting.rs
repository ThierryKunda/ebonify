use serde_json::Value;
use crate::error::ConversionError;

pub trait Data {
    fn get_json(&self) -> Result<Value, ConversionError>;
}

struct JsonData {
    value: Value
}


struct HttpData {
    uri: String
}


impl Data for JsonData {
    fn get_json(&self) -> Result<Value, ConversionError> {
        Ok(self.value.clone())
    }
}

impl Data for HttpData {
    fn get_json(&self) -> Result<Value, ConversionError> {
        let body = reqwest::blocking::get(&self.uri);
        match body {
            Ok(resp) => match resp.json::<Value>() {
                Ok(v) => Ok(v),
                Err(_) => Err(ConversionError::new("Error during data conversion to JSON")),
            }
            Err(_) => Err(ConversionError::new("Error from HTTP request")),
        }

    }
}