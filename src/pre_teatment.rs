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

pub fn brackets_paired(operator1: &Operator, operator2: &Operator) -> bool {
    match (operator1,operator2) {
        (Operator::OptionalL, Operator::OptionalR) => true,
        (Operator::GroupingL, Operator::GroupingR) => true,
        (Operator::RepetitionL, Operator::RepetitionR) => true,
        _ => false,

    }
}

pub fn valid_dual_operators(rule: &Vec<Token>) -> bool {
    let mut operators_list: Vec<&Operator> = Vec::new();
    for token in rule {
        match token {
            Token::Op(op) => match op {
                Operator::OptionalL => operators_list.push(op),
                Operator::OptionalR => operators_list.push(op),
                Operator::RepetitionL => operators_list.push(op),
                Operator::RepetitionR => operators_list.push(op),
                Operator::GroupingL => operators_list.push(op),
                Operator::GroupingR => operators_list.push(op),
                _ => ()
            },
            _ => (),
        }
    }
    let mut operators_test: Vec<&Operator> = Vec::new();
    match operators_list.get(0) {
        Some(v) => operators_test.push(v),
        None => return true,
    }
    let size = operators_list.len();
    for i in 1..size {
        let current_op = operators_list.get(i);
        let last_test_op = operators_test.last();
        match (last_test_op, current_op) {
            (Some(op1), Some(op2)) => {
                if brackets_paired(op1, op2) {
                    operators_test.pop();
                } else {
                    let o = *op1;
                    operators_test.push(o);
                }
            },
            (Some(op1), None) => {
                let o = *op1;
                operators_test.push(o)
            },
            (None, _) => return operators_test.len() == 0,
        }
    }
operators_test.len() == 0
}

pub fn valid_single_operators(rule: &Vec<Token>) -> bool {
    match (rule.first(), rule.last()) {
        (None, None) => todo!(),
        (Some(first), Some(last)) => match (first, last) {
            (Token::Op(a), Token::Op(b)) => match (a,b) {
                (Operator::Alternation, _) => return false,
                (Operator::Concatenation, _) => return false,
                (Operator::Exception, _) => return false,
                (_, Operator::Alternation) => return false,
                (_, Operator::Concatenation) => return false,
                (_, Operator::Exception) => return false,
                _ => ()
            },
            _ => ()
        },
        _ => ()
    }

    let mut i: usize = 0;
    let rule_size = rule.len();

    while i+1 < rule_size {
        match (rule.get(i), rule.get(i+1)) {
            (Some(a), Some(b)) => {
                match (a,b) {
                    (Token::Op(x), Token::Op(y)) => {
                        match (x,y) {
                            (Operator::Alternation, Operator::Alternation) => return false,
                            (Operator::Alternation, Operator::Concatenation) => return false,
                            (Operator::Alternation, Operator::Exception) => return false,
                            (Operator::Concatenation, Operator::Concatenation) => return false,
                            (Operator::Concatenation, Operator::Alternation) => return false,
                            (Operator::Concatenation, Operator::Exception) => return false,
                            (Operator::Exception, Operator::Exception) => return false,
                            (Operator::Exception, Operator::Alternation) => return false,
                            (Operator::Exception, Operator::Concatenation) => return false,
                            _ => ()
                        }
                    },
                    _ => ()
                }
            },
            _ => ()

        }
        i += 1;
    }
    return true
}