use std::error::Error;
use std::fs;

use crate::ast::{create_definition_tree, tokens_as_ref};
use crate::error::PreTreatmentError;
use crate::pre_treatment::{split_members, split_lines, tokenize_rule_from_str, split_lines_from_file};
use crate::ebnf_syntax::*;
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct EbnfTree {
    pub syntax_source_name: Option<String>,
    pub nodes_count: u32,
    pub rules: BTreeMap<String, Rc<Rule>>,
}

impl EbnfTree {
    pub fn new(source_name: &str) -> Self {
        EbnfTree {
            syntax_source_name: Some(source_name.to_string()),
            nodes_count: 0,
            rules: BTreeMap::new(),
        }
    }

    pub fn from_file(filepath: &str) -> Result<EbnfTree, PreTreatmentError> {
        let file_content = split_lines_from_file(filepath);
        match file_content {
            Ok(content) => {
                let mut pairs: Vec<(String, Rc<Rule>)> = Vec::new();
                let rulename_definition_pairs = split_members(content);
                for (rule_name, mut definition) in rulename_definition_pairs {
                    // Removal of the semi-colon at the end of a definition
                    definition.pop();
                    let tokens = tokenize_rule_from_str(definition);
                    pairs.push((rule_name, create_definition_tree(&tokens)));
                }
                Ok(
                    EbnfTree::from(pairs)
                )
            },
            Err(_) => Err(PreTreatmentError),
        }
    }
}

impl From<Vec<(String, Rc<Rule>)>> for EbnfTree {
    fn from(rulename_definition_pairs: Vec<(String, Rc<Rule>)>) -> Self {
        let mut pairs: BTreeMap<String, Rc<Rule>> = BTreeMap::new();
        for (rule_name, definition) in rulename_definition_pairs {
            pairs.insert(rule_name.to_string(), definition);
        }
        EbnfTree {
            syntax_source_name: None,
            nodes_count: 0,
            rules: pairs,
        }
    }
}

impl From<&str> for EbnfTree {
    fn from(text: &str) -> Self {
    let mut pairs: BTreeMap<String, Rc<Rule>> = BTreeMap::new();
    let mut rules_by_line = split_lines(text.to_string());
    for r  in rules_by_line.iter_mut() {
        r.pop();
    }
    let rulename_definition_pairs = split_members(rules_by_line);
    for (rule_name, definition) in rulename_definition_pairs {
        let tokens = tokenize_rule_from_str(definition);
        pairs.insert(rule_name, create_definition_tree(&tokens));
    }
        EbnfTree {
            syntax_source_name: None,
            nodes_count: 0,
            rules: pairs,
        }
    }
}