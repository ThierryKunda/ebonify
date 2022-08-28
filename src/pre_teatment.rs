use regex;
use std::fs;
use crate::ebnf_syntax::*;
use crate::error::PreTreatmentError;

pub fn split_lines(filepath: &str) -> Result<Vec<String>, PreTreatmentError> {
    let file_content = fs::read_to_string(filepath);
    let content: String;
    match file_content {
        Ok(ct) => {
            content = ct;
        },
        Err(_) => {
            return Err(PreTreatmentError);
        },
    }
    let re = regex::Regex::new(r".+;").unwrap();
    let content_str = content.as_str();
    let matches_iter = re.find_iter(content_str).map(|m| m.as_str());
    let mut v: Vec<String> = Vec::new();
    for mat in matches_iter {
        v.push(mat.to_string())
    }
    return Ok(v);
}

pub fn split_members_aux(rule: String) -> Vec<String>{
    let rule_split: Vec<&str> = rule.split("=").collect();
    let rules: Vec<String> = rule_split.iter()
        .map(|mem| mem.trim())
        .map(|mem| mem.to_string())
        .collect();
    rules
}

fn split_members(rules: Vec<String>) -> Vec<Vec<String>> {
    let mut members: Vec<Vec<String>> = Vec::new();
    for r in rules.iter() {
        members.push(split_members_aux(r.to_string()));
    }
    members
}

pub fn tokenize(token: String) -> Token {
    let token_as_str = token.as_str();
    match token_as_str {
        "," => Token::Op(Operator::Concatenation),
        "|" => Token::Op(Operator::Alternation),
        "-" => Token::Op(Operator::Exception),
        "[" => Token::Op(Operator::OptionalL),
        "]" => Token::Op(Operator::OptionalR),
        "{" => Token::Op(Operator::RepetitionL),
        "}" => Token::Op(Operator::RepetitionR),
        "(" => Token::Op(Operator::GroupingL),
        ")" => Token::Op(Operator::GroupingR),
        s => {
            if s.starts_with("\"") && s.ends_with("\"") {
                return Token::Rl(Rule::Literal(s.trim_matches('"').to_string()));
            } else if s.starts_with("\"") || s.ends_with("\"") {
                return Token::Invalid;
            }
            Token::Rl(Rule::Identifier(s.to_string()))
        },
    }
}

pub fn tokenize_rule(rule: Vec<String>) -> Vec<Token> {
    rule.iter().map(|v| tokenize(v.to_string())).collect()
}

pub fn validate_rule(rules: Vec<Token>) -> bool {
    rules.iter().all(|t| match t {
        Token::Invalid => false,
        _ => true,
    })
}