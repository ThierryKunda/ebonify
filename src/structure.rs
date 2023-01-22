use std::ops::Deref;

use crate::ast::*;
use crate::error::PreTreatmentError;
use crate::pre_treatment::{split_members, split_lines, tokenize_rule_from_str, split_lines_from_file};
use crate::ebnf_syntax::*;
use crate::utils::AssocRuleCounter;
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct EbnfTree {
    pub syntax_source_name: Option<String>,
    pub nodes_count_per_definition: AssocRuleCounter,
    pub rules: BTreeMap<String, Rc<Rule>>,
    pub identified_counts: AssocRuleCounter,
}

impl EbnfTree {
    pub fn new(source_name: &str) -> Self {
        EbnfTree {
            syntax_source_name: Some(source_name.to_string()),
            nodes_count_per_definition: AssocRuleCounter::new(),
            rules: BTreeMap::new(),
            identified_counts: AssocRuleCounter::new()
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
                    let def = create_definition_tree(&tokens);
                    pairs.push((rule_name, get_pure_tree(def)));
                }
                Ok(EbnfTree::from(&pairs))
            },
            Err(_) => Err(PreTreatmentError),
        }
    }

    pub fn add_rule(&mut self, rulename: &String, definition: &Rc<Rule>) {
        self.rules.insert(rulename.to_string(), Rc::clone(definition));
        for (_, def) in self.rules.iter_mut() {
            *def = tree_with_id_ref((rulename, definition), def);
        }
    }
    
    pub fn update_identified_counts(&mut self) {
        let check_if_id = |atom: &Rule| {
            if let Rule::Identifier(id) = atom.deref() {
                AssocRuleCounter::from(vec![(id.to_string(), 1)])
            } else {
                AssocRuleCounter::from(vec![])
            }
        };
        let mut counters_list: Vec<AssocRuleCounter> = Vec::new();
        for (_, def) in self.rules.iter() {
            let cnt = counting_single_result(
                &def, &check_if_id,
                &|_| AssocRuleCounter::from(vec![]),
                &|_, cnt| cnt,
                 &|_, cnt1, cnt2| cnt1 + cnt2
            );
            counters_list.push(cnt);
        }
        self.identified_counts = counters_list.iter().fold(AssocRuleCounter::new(), |acc, cnt| acc + cnt.clone());
    }
}

impl From<&Vec<(String, Rc<Rule>)>> for EbnfTree {
    fn from(rulename_definition_pairs: &Vec<(String, Rc<Rule>)>) -> Self {
        let mut pairs: BTreeMap<String, Rc<Rule>> = BTreeMap::new();
        for (rule_name, definition) in rulename_definition_pairs {
            pairs.insert(rule_name.to_string(), Rc::clone(&definition));
        }
        EbnfTree {
            syntax_source_name: None,
            rules: pairs,
            nodes_count_per_definition: AssocRuleCounter::new(),
            identified_counts: AssocRuleCounter::new(),
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
            rules: pairs,
            nodes_count_per_definition: AssocRuleCounter::new(),
            identified_counts: AssocRuleCounter::new(),
        }
    }
}