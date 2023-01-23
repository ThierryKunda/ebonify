use std::ops::Deref;
use std::path::Path;
use serde_json::{Value, Map, Number};

use crate::ast::*;
use crate::error::PreTreatmentError;
use crate::pre_treatment::{split_members, split_lines, tokenize_rule_from_str, split_lines_from_file};
use crate::ebnf_syntax::*;
use crate::utils::{AssocRuleCounter, Counter, RuleCounter};
use std::collections::BTreeMap;
use std::rc::{Rc, Weak};

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
        let fp = Path::new(filepath);
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
                let mut res = EbnfTree::from(&pairs);
                let mut syntax_source_name = fp.file_name().unwrap().to_str().unwrap().to_string();
                for _ in 0..5 {
                    syntax_source_name.pop();
                }
                res.syntax_source_name = Some(syntax_source_name);
                Ok(res)
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
    pub fn update_definition_nodes_count(&mut self, rulename: &String, counting_ref: bool) {
        if let Some(def) = self.rules.get(rulename) {
            let c_atom = |_: &Rule| AssocRuleCounter::from(vec![(rulename.to_string(), 1)]);
            let c_ref = |_: &Weak<Rule>| if counting_ref { AssocRuleCounter::from(vec![(rulename.to_string(), 1)]) } else { AssocRuleCounter::new() };
            let oosc = |_: &Rule, cnt| cnt;
            let ootc = |_: &Rule, cnt1, cnt2| cnt1 + cnt2;
            
            let counter = counting_single_result(
                def,
                &c_atom,
                &c_ref,
                &oosc,
                &ootc
            );
            self.nodes_count_per_definition.add_any_to_element(Some(rulename), counter.get_associated_count(Some(rulename)).unwrap());
        }
    }

    pub fn update_all_definition_nodes_count(&mut self, counting_ref: bool) {
        let oosc = |_: &Rule, cnt| cnt;
        let ootc = |_: &Rule, cnt1, cnt2| cnt1 + cnt2;
        let c_atom = |_: &Rule| RuleCounter::from(1);
        let c_ref = |_: &Weak<Rule>| if counting_ref { RuleCounter::from(1) } else { RuleCounter::new() };
        for (name, def) in self.rules.iter() {
            let counter = counting_single_result(
                def,
                &c_atom,
                &c_ref,
                &oosc,
                &ootc
            );
            self.nodes_count_per_definition.add_any_to_element(Some(name), counter.total() as usize);

            self.nodes_count_per_definition.update_total(counter.total() as usize);
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

    pub fn get_rule_name_from_ref(&self, rule: &Weak<Rule>) -> Option<String> {
        match rule.upgrade() {
            Some(rl) => {
                for (name, def) in self.rules.iter() {
                    if are_same_tree(&rl, def) {
                        return Some(name.to_string());
                    }
                }
                return None;
            },
            None => None,
        }
    }

    fn create_rule_json_schema(&self, rule_name: &String, definition: &Rc<Rule>) -> Value {
        let mut m = Map::new();
        match definition.deref() {
            Rule::Literal(lit) => {
                m.insert(String::from("node_type"), Value::String(String::from("atom")));
                m.insert(String::from("node"), Value::String(String::from("literal")));
                m.insert(String::from("value"), Value::String(lit.to_string()));
                Value::Object(m)
            },
            Rule::Identifier(id) => {
                m.insert(String::from("node_type"), Value::String(String::from("atom")));
                m.insert(String::from("node"), Value::String(String::from("identifier")));
                m.insert(String::from("value"), Value::String(id.to_string()));
                Value::Object(m)
            },
            Rule::Ref(r) => {
                m.insert(String::from("node_type"), Value::String(String::from("single")));
                m.insert(String::from("node"), Value::String(String::from("identifier")));
                m.insert(String::from("value"), if let Some(sub) = self.get_rule_name_from_ref(r) { Value::String(sub.to_string()) } else { Value::Null } );
                Value::Object(m)
            },
            Rule::Alternation(left, right) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("alternation")));
                m.insert(String::from("left"), self.create_rule_json_schema(rule_name, left));
                m.insert(String::from("right"), self.create_rule_json_schema(rule_name, right));
                Value::Object(m)
            },
            Rule::Concatenation(left, right) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("concatenation")));
                m.insert(String::from("left"), self.create_rule_json_schema(rule_name, left));
                m.insert(String::from("right"), self.create_rule_json_schema(rule_name, right));
                Value::Object(m)
            },
            Rule::Exception(left, right) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("exception")));
                m.insert(String::from("left"), self.create_rule_json_schema(rule_name, left));
                m.insert(String::from("right"), self.create_rule_json_schema(rule_name, right));
                Value::Object(m)
            },
            Rule::Optional(sub) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("optional")));
                m.insert(String::from("sub_rule"), self.create_rule_json_schema(rule_name, sub));
                Value::Object(m)
            },
            Rule::Repetition(sub) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("repetition")));
                m.insert(String::from("sub_rule"), self.create_rule_json_schema(rule_name, sub));
                Value::Object(m)
            },
            Rule::Grouping(sub) => {
                m.insert(String::from("node_type"), Value::String(String::from("dual")));
                m.insert(String::from("node"), Value::String(String::from("grouping")));
                m.insert(String::from("sub_rule"), self.create_rule_json_schema(rule_name, sub));
                Value::Object(m)
            },
        }
    }

    pub fn create_json_schema(&self) -> Value {
        let mut schema = Map::new();
        schema.insert(String::from("syntax_source_name"), if let Some(v) = self.syntax_source_name.as_ref() {Value::String(v.to_string())} else {Value::Null});
        schema.insert(
            String::from("rules"),
            Value::Array(
                self.rules.iter()
                .map(|(name, def)| self.create_rule_json_schema(name, def))
                .collect()
            )
        );
        schema.insert(String::from("identified_count"), Value::Object({
            let mut m = Map::new();
            for (name, def) in self.identified_counts.iter() {
                m.insert(name.to_string(), Value::Number(Number::from(def.clone())));
            }
            m
        }));
        schema.insert(String::from("nodes_count_per_definition"), Value::Object({
            let mut m = Map::new();
            for (name, def) in self.nodes_count_per_definition.iter() {
                m.insert(name.to_string(), Value::Number(Number::from(def.clone())));
            }
            m
        }));
        Value::Object(schema)
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