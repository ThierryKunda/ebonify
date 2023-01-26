use std::fmt::{Display, Debug};

#[derive(Debug)]
pub struct PreTreatmentError;

impl Display for PreTreatmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The treatment failed")
    }
}

impl std::error::Error for PreTreatmentError {}

#[derive(Debug)]
pub struct JSONParsingError;

impl Display for JSONParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The JSON content is invalid")
    }
}