use std::rc::Rc;
use super::tree::EbnfTree;
use crate::ebnf_syntax::Rule;

enum ParsingStatus {
    BeforeStarting,
    OnGoing,
    Acceptation,
    Failure
}

struct Parser<'a> {
    tree: &'a EbnfTree,
    string_to_match: String,
    stack: Vec<String>,
    status: ParsingStatus
}

impl<'a> Parser<'a> {
    pub fn new(string_to_match: &'a String, tree: &'a EbnfTree) -> Self {
        Parser {
            tree,
            string_to_match: string_to_match.clone(),
            stack: string_to_match
                .chars()
                .map(|c| c.to_string())
                .collect(),
            status: ParsingStatus::BeforeStarting
        }
    }
}