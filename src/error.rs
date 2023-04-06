use std::fmt::{Display, Debug};
use std::error::Error;

#[derive(Debug)]
pub struct PreTreatmentError;

impl Display for PreTreatmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The pre-processing failed")
    }
}

impl Error for PreTreatmentError {}

#[derive(Debug)]
struct RuleError {}

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