use regex;
use std::fs;
use std::ops::Deref;
use std::rc::Rc;
use crate::ebnf_syntax::*;
use crate::error::PreTreatmentError;

pub fn split_lines(content: String) -> Vec<String> {
    let re = regex::Regex::new(r"[^;\n]+;").unwrap();
    let content_str = content.as_str();
    let matches_iter = re
        .find_iter(content_str)
        .map(|m| m.as_str().trim());
    let mut v: Vec<String> = Vec::new();
    for mat in matches_iter {
        v.push(mat.to_string());
    }
    v
}

pub fn split_lines_from_file(filepath: &str) -> Result<Vec<String>, PreTreatmentError> {
    let file_content = fs::read_to_string(filepath);
    match file_content {
        Ok(ct) => {
            Ok(split_lines(ct))
        },
        Err(_) => {
            Err(PreTreatmentError)
        },
    }
}

pub fn split_members_aux(rule: String) -> (String, String) {
    let rule_split: Vec<&str> = rule.split("=").collect();
    let mut rules: Vec<String> = rule_split.iter()
        .map(|mem| mem.trim())
        .map(|mem| mem.to_string())
        .collect();
    (rules.remove(0), rules.remove(0))
}

pub fn split_members(rules: Vec<String>) -> Vec<(String, String)> {
    let mut members: Vec<(String, String)> = Vec::new();
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
                return Token::Rl(Rc::new(Rule::Literal(s.trim_matches('"').to_string())));
            } else if s.starts_with("\'") && s.ends_with("\'") {
                return Token::Rl(Rc::new(Rule::Literal(s.trim_matches('\'').to_string())));
            } else if s.starts_with("\"") || s.starts_with("\'") || s.ends_with("\"") || s.ends_with("\'") {
                return Token::Invalid;
            } else if s.starts_with("\"") || s.ends_with("\"") {
                return Token::Invalid;
            }
            Token::Rl(Rc::new(Rule::Identifier(s.to_string())))
        },
    }
}

pub fn tokenize_rule(rule: Vec<String>) -> Vec<Token> {
    rule.iter().map(|v| tokenize(v.to_string())).collect()
}

pub fn tokenize_rule_from_str<'a>(rule: String) -> Vec<Token> {
    let re = regex::Regex::new(r"([^\u0022\||,|\-|\{|\(|\[|\]|\)|\}]+)|(\||,|\-|\{|\(|\[|\]|\)|\})|(\u0022[^\u0022]+\u0022)|('[^']+')").unwrap();
    let matches_iter = re.find_iter(rule.as_str());
    let m: Vec<String> = matches_iter
        .map(|el| el.as_str().trim().to_string())
        .filter(|el| el != &String::from(""))
        .collect();
    tokenize_rule(m)
}

pub fn tokenize_file(file: &str) -> Vec<Vec<Token>> {
    let file_lines = split_lines_from_file(file);
    match file_lines {
        Ok(v) => {
            let res = v
            .into_iter()
            .map(|el| tokenize_rule_from_str(el)).collect();
            res
        },
        Err(_) => vec![vec![Token::Invalid]],
    }
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
pub fn is_binary(operator: &Operator) -> bool {
    match operator {
        Operator::Alternation => true,
        Operator::Concatenation => true,
        Operator::Exception => true,
        _ => false
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
    match operators_list.first() {
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

pub fn valid_single_operators(rule: &Vec<&Token>) -> bool {
    match (rule.first(), rule.last()) {
        (None, None) => return true,
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

pub fn valid_following_operators(rule: &Vec<Token>) -> bool {
    if rule.len() == 0 {
        return true;
    }
    let max_index = rule.len() - 1;
    for i in 0..max_index {
        match (rule.get(i), rule.get(i+1)) {
            (None, Some(_)) => (),
            (_, None) => return true,
            (Some(tk1), Some(tk2)) => match (tk1, tk2) {
                (Token::Op(op1), Token::Op(op2)) => match (op1, op2) {
                    (Operator::GroupingL, Operator::Alternation) => return false,
                    (Operator::GroupingL, Operator::Concatenation) => return false,
                    (Operator::GroupingL, Operator::Exception) => return false,

                    (Operator::RepetitionL, Operator::Alternation) => return false,
                    (Operator::RepetitionL, Operator::Concatenation) => return false,
                    (Operator::RepetitionL, Operator::Exception) => return false,

                    (Operator::OptionalL, Operator::Alternation) => return false,
                    (Operator::OptionalL, Operator::Concatenation) => return false,
                    (Operator::OptionalL, Operator::Exception) => return false,
                    
                    (Operator::Alternation, Operator::GroupingR) => return false,
                    (Operator::Alternation, Operator::RepetitionR) => return false,
                    (Operator::Alternation, Operator::OptionalR) => return false,

                    (Operator::Concatenation, Operator::GroupingR) => return false,
                    (Operator::Concatenation, Operator::RepetitionR) => return false,
                    (Operator::Concatenation, Operator::OptionalR) => return false,
                    
                    (Operator::Exception, Operator::GroupingR) => return false,
                    (Operator::Exception, Operator::RepetitionR) => return false,
                    (Operator::Exception, Operator::OptionalR) => return false,

                    _ => ()
                },
                _ => ()
            },
        }
    }
    return true;
}

pub fn tokens_equals(token1: &Token, token2: &Token) -> bool {
    match (token1, token2) {
        (Token::Op(op1), Token::Op(op2)) => match (op1, op2) {
            (Operator::Alternation, Operator::Alternation) => true,
            (Operator::Concatenation, Operator::Concatenation) => true,
            (Operator::Exception, Operator::Exception) => true,
            (Operator::OptionalL, Operator::OptionalL) => true,
            (Operator::OptionalR, Operator::OptionalR) => true,
            (Operator::RepetitionL, Operator::RepetitionL) => true,
            (Operator::RepetitionR, Operator::RepetitionR) => true,
            (Operator::GroupingL, Operator::GroupingL) => true,
            (Operator::GroupingR, Operator::GroupingR) => true,
            _ => false,
        },
        (Token::Rl(rl1), Token::Rl(rl2)) => match (rl1.deref(), rl2.deref()) {
            (Rule::Literal(s1), Rule::Literal(s2)) |
            (Rule::Identifier(s1), Rule::Identifier(s2)) => s1 == s2,
            _ => false,
        }
        _ => false,
    }
}

pub fn rules_equals(rule1: &Vec<&Token>, rule2: &Vec<&Token>) -> bool {
    for i in 0..rule1.len() {
        let a = rule1.get(i);
        let b = rule2.get(i);
        match (a, b) {
            (None, _) => return false,
            (_, None) => return false,
            (Some(tk1), Some(tk2)) => {
                if !tokens_equals(*tk1, *tk2) {
                    return false;
                }
            },
        };
    }
    true
}