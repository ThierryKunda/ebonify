use std::fmt::{Display, Debug};

pub struct PreTreatmentError;

impl Display for PreTreatmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The treatment failed")
    }
}

impl Debug for PreTreatmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ file: {}, line: {} }}", file!(), line!())
    }
}

impl std::error::Error for PreTreatmentError {}