use crate::error::PreTreatmentError;
use super::tree::EbnfTree;

pub struct EbnfTreeBuilder {
}

impl EbnfTreeBuilder {
    pub fn from_file(path: &str) -> Result<EbnfTree, PreTreatmentError> {
        let mut res = EbnfTree::from_file(path)?;
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }
    
    pub fn from_plain_text(text: &str) -> Result<EbnfTree, PreTreatmentError> {
        let mut res = EbnfTree::from(text);
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }
}