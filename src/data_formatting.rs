use serde_json::Value;
use crate::error::ConversionError;

pub trait Data {
    fn get_json(&self) -> Result<Value, ConversionError>;
}

struct JsonData {
    value: String
}


struct HttpData {
    uri: String
}

pub struct FileData {
    path: String
}


impl Data for JsonData {
    fn get_json(&self) -> Result<Value, ConversionError> {
        let val = serde_json::from_str::<Value>(&self.value);
        match val {
            Ok(v) => Ok(v),
            Err(e) => match e.classify() {
                serde_json::error::Category::Syntax => Err(
                    ConversionError::new("The string is not a syntactically well formatted JSON")
                ),
                _ => Err(ConversionError::new("Unknow error")),
            },
        }
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

impl FileData {
    pub fn new(filepath: &str) -> Self {
        Self { path: filepath.to_string() }
    }
}

impl Data for FileData {
    fn get_json(&self) -> Result<Value, ConversionError> {
        match EbnfTreeBuilder::from_file(&self.path) {
            Ok(t) => Ok(t.create_json_schema()),
            Err(_) => Err(ConversionError::new("Error while parsing the file")),
        }

    }
}