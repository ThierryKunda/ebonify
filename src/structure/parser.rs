use std::rc::Rc;
use super::tree::EbnfTree;
use crate::ebnf_syntax::{Rule, AtomicKind};

enum ParsingStatus {
    BeforeStarting,
    OnGoing,
    Success,
    Failure
}

struct Parser<'a> {
    tree: &'a EbnfTree,
    string_to_match: String,
    stack: Vec<Rc<Rule>>,
    status: ParsingStatus
}

impl<'a> Parser<'a> {
    pub fn new(string_to_match: &'a String, tree: &'a EbnfTree) -> Self {
        Parser {
            tree,
            string_to_match: string_to_match.clone(),
            stack: string_to_match
                .chars()
                .map(|c| Rc::new(Rule::Atomic(c.to_string(), AtomicKind::Literal)))
                .collect(),
            status: ParsingStatus::BeforeStarting
        }
    }
}