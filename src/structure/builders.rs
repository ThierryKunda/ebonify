use crate::error::PreTreatmentError;
use super::tree::EbnfTree;

struct EbnfTreeBuilder {
}

impl EbnfTreeBuilder {
    pub fn from_file(path: &str) -> Result<EbnfTree, PreTreatmentError> {
        let mut res = EbnfTree::from_file(path)?;
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }
 
}