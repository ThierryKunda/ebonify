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
pub struct RuleError {
    error_details: String
}

impl RuleError {
    pub fn new(details: &str) -> Self {
        RuleError { error_details: details.to_string() }
    }
}

impl Display for RuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The rule is invalid : {}", &self.error_details)
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

#[derive(Debug)]
pub struct SourcePathError {
    error_details: String
}

impl SourcePathError {
    pub fn new(error_details: &str) -> Self {
        Self { error_details: error_details.to_string() }
    }
}

impl Error for SourcePathError {}

impl Display for SourcePathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error during path processing : {}", self.error_details)
    }
}

#[derive(Debug)]
pub struct ParserError {
    error_details: String
}

impl ParserError {
    pub fn new(error_details: &str) -> Self {
        Self {
            error_details: error_details.to_string(),
        }
    }
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error while creating the parser : {}", self.error_details)
    }
}