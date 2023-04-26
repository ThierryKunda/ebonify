use std::fmt::{Display, Debug};
use std::error::Error;


#[derive(Debug, Clone)]
pub struct ParsingError {
    error_details: String
}

impl ParsingError {
    pub fn new(error_details: &str) -> Self {
        Self { error_details: error_details.to_string() }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The pre-processing failed : {}", &self.error_details)
    }
}

impl Error for ParsingError {}

#[derive(Debug)]
pub struct RuleError;

impl Display for RuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The rule is invalid")
    }
}

impl Error for RuleError {}

#[derive(Debug)]
pub struct JSONParsingError;

impl Display for JSONParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The JSON content is invalid")
    }
}
#[derive(Debug)]
pub struct ConversionError {
    message: String
}

impl ConversionError {
    pub fn new(message: &str) -> Self {
        ConversionError { message: message.to_string() }
    }
}

impl Error for ConversionError {}

impl Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The conversion between the two types is invalid.\nReason : {}", self.message)
    }
}