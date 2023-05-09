use crate::{error::{ParsingError, ConversionError}, data_formatting::Data};
use super::tree::EbnfTree;

pub struct EbnfTreeBuilder {
}

impl EbnfTreeBuilder {
    pub fn from_file(path: &str) -> Result<EbnfTree, ParsingError> {
        let mut res = EbnfTree::from_file(path)?;
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }
    
    pub fn from_plain_text(text: &str) -> Result<EbnfTree, ParsingError> {
        let mut res = EbnfTree::from_string(text)?;
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }

    pub fn from_data_format(data: &impl Data) -> Result<EbnfTree, ConversionError> {
        let mut res = EbnfTree::from_json(data.get_json()?)?;
        res.update_all_definition_nodes_count(true);
        res.update_identified_counts();
        Ok(res)
    }
}