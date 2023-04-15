#[cfg(test)]
mod tests;

use crate::ebnf_syntax::{Operator, Token};

use regex;
use std::fs;
use std::ops::Deref;
use std::rc::Rc;
use crate::ebnf_syntax::*;
use crate::error::ParsingError;
/// Split the lines of an input string and return the result
/// 
/// # Arguments
/// 
/// * `content` - The content to be splitted
/// 
/// # Examples
/// ```
/// use ebonify::parsing::split_lines;
/// let lines = split_lines(String::from(
///     "my_rule = {abc,def,ghi};\n
///     tk2 = ok | nok;"
/// ));
/// assert_eq!(lines, vec![String::from("my_rule = {abc,def,ghi};"), String::from("tk2 = ok | nok;")]);
/// ```
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

/// Split the lines of a file and return the result
/// 
/// # Arguments
/// 
/// * `filepath` - The path of the file holding some content
/// 
/// # Examples
/// 
/// ```
/// use ebonify::parsing::split_lines_from_file;
/// let lines = split_lines_from_file("tests_samples/separation.txt").unwrap();
/// assert_eq!(lines, vec![
///     String::from("Line 1 : ABCD;"),
///     String::from("Line 2 : EFGH;"),
///     String::from("Line n : ...;")]
/// );
/// ```
pub fn split_lines_from_file(filepath: &str) -> Result<Vec<String>, ParsingError> {
    let file_content = fs::read_to_string(filepath);
    match file_content {
        Ok(ct) => {
            Ok(split_lines(ct))
        },
        Err(_) => {
            Err(ParsingError)
        },
    }
}

/// Split the elements separated with a '=' and returns a tuple of both elements
/// 
/// # Arguments
/// 
/// * `rule` - string to be splitted
/// 
/// # Examples
/// 
/// ```
/// use ebonify::parsing::split_members_aux;
/// let split_1 = split_members_aux(String::from("abc = def"));
/// let split_2 = split_members_aux(String::from("abc + 123 = def + 456"));
/// assert_eq!(split_1, (String::from("abc"), String::from("def")));
/// assert_eq!(split_2, (String::from("abc + 123"), String::from("def + 456")));
/// ```
pub fn split_members_aux(rule: String) -> (String, String) {
    let rule_split: Vec<&str> = rule.split("=").collect();
    let mut rules: Vec<String> = rule_split.iter()
        .map(|mem| mem.trim())
        .map(|mem| mem.to_string())
        .collect();
    (rules.remove(0), rules.remove(0))
}

/// Apply [split_members_aux] to a list of string
/// 
/// # Arguments
/// 
/// * `rules` - list of string to split with a single '=' as a separator
/// 
/// # Examples
/// 
/// ```
/// use ebonify::parsing::split_members;
/// let split_list = split_members(vec![
///     String::from("abc = def"),
///     String::from("abc + 123 = def + 456")
/// ]);
/// 
/// assert_eq!(
///     split_list,
///     vec![
///         (String::from("abc"), String::from("def")),
///         (String::from("abc + 123"), String::from("def + 456"))
///     ]
/// );
/// ```
pub fn split_members(rules: Vec<String>) -> Vec<(String, String)> {
    let mut members: Vec<(String, String)> = Vec::new();
    for r in rules.iter() {
        members.push(split_members_aux(r.to_string()));
    }
    members
}

/// **Returns the associated semantic element of a string**
/// 
/// In the EBNF metasyntax there are three types of element :
/// - terminal symbol, represented by/equivalent to a string
/// - non-terminal symbol, which refers to a definition of syntax rules
/// using terminal and/or non-terminal symbol
/// - and operator, which can be splitted in two classes :
/// unary-type and binary-type operators
/// 
/// # Arguments
/// 
/// * `token` - the string to be *tokenized*
fn tokenize(token: String) -> Token {
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
                return Token::Rl(Rc::new(Rule::Atomic(s.trim_matches('"').to_string(), AtomicKind::Literal)));
            } else if s.starts_with("\'") && s.ends_with("\'") {
                return Token::Rl(Rc::new(Rule::Atomic(s.trim_matches('\'').to_string(), AtomicKind::Literal)));
            } else if s.starts_with("\"") || s.starts_with("\'") || s.ends_with("\"") || s.ends_with("\'") {
                return Token::Invalid;
            } else if s.starts_with("\"") || s.ends_with("\"") {
                return Token::Invalid;
            }
            Token::Rl(Rc::new(Rule::Atomic(s.to_string(), AtomicKind::Identifier)))
        },
    }
}

/// Returns a list of tokens from a list of string
/// 
/// It uses the [tokenize] function to get each token
/// 
/// # Arguments
/// 
/// * rule - the list of string to be tokenized
pub fn tokenize_rule(rule: Vec<String>) -> Vec<Token> {
    rule.iter().map(|v| tokenize(v.to_string())).collect()
}

/// Returns a list of token from a string
/// 
/// # Arguments
/// 
/// * rule - the rule as a single string
pub fn tokenize_rule_from_str<'a>(rule: String) -> Vec<Token> {
    let re = regex::Regex::new(r"(('[^']+')|[^\u0022\||,|\-|\{|\(|\[|\]|\)|\}]+)|(\||,|\-|\{|\(|\[|\]|\)|\})|(\u0022[^\u0022]+\u0022)").unwrap();
    let matches_iter = re.find_iter(rule.as_str());
    let m: Vec<String> = matches_iter
        .map(|el| el.as_str().trim().to_string())
        .filter(|el| el != &String::from(""))
        .collect();
    tokenize_rule(m)
}

/// Returns an 2D array of tokens from a file
/// 
/// A line is representated as a vector of tokens
/// 
/// # Arguments
/// 
/// * file - the path of the file to be *tokenized*
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

/// Checks if a rule is validate from the tokenization process.
/// 
/// The EBNF elements has to be well typed to be converted into semantic objects.
/// 
/// # Arguments
/// 
/// * rules - the sequence of tokens to be validated
/// 
/// # Examples
/// 
/// ```
/// use ebonify::ebnf_syntax::Token;
/// use ebonify::parsing::{tokenize_rule_from_str, validate_rule};
/// let tokens_0 = tokenize_rule_from_str(String::from("a"));
/// let tokens_1 = tokenize_rule_from_str(String::from("'a'"));
/// let tokens_2 = tokenize_rule_from_str(String::from("a | b | 'c'"));
/// let tokens_3 = tokenize_rule_from_str(String::from("'a | b | c'"));
/// 
/// let tokens_4 = tokenize_rule_from_str(String::from("'a"));
/// let tokens_5 = tokenize_rule_from_str(String::from("a'"));
/// let tokens_6 = tokenize_rule_from_str(String::from("a | 'b | c"));
/// 
/// assert!(validate_rule(tokens_0));
/// assert!(validate_rule(tokens_1));
/// assert!(validate_rule(tokens_2));
/// assert!(validate_rule(tokens_3));
/// 
/// assert!(!validate_rule(tokens_4));
/// assert!(!validate_rule(tokens_5));
/// assert!(!validate_rule(tokens_6));
/// ```
pub fn validate_rule(rules: Vec<Token>) -> bool {
    rules.iter().all(|t| match t {
        Token::Invalid => false,
        _ => true,
    })
}


/// Checks if brackets are paired
/// 
/// If the operators are not brackets, it returns `false`
/// 
/// # Arguments
/// 
/// * operator1 - first bracket
/// * operator2 - second bracket
/// 
/// # Examples
/// 
/// ```
/// use ebonify::ebnf_syntax::Operator;
/// use ebonify::parsing::brackets_paired;
/// 
/// assert!(brackets_paired(&Operator::GroupingL, &Operator::GroupingR));
/// assert!(brackets_paired(&Operator::OptionalR, &Operator::OptionalL) == false);
/// ```
pub fn brackets_paired(operator1: &Operator, operator2: &Operator) -> bool {
    match (operator1,operator2) {
        (Operator::OptionalL, Operator::OptionalR) => true,
        (Operator::GroupingL, Operator::GroupingR) => true,
        (Operator::RepetitionL, Operator::RepetitionR) => true,
        _ => false,
        
    }
}

/// Checks if a list of tokens contains valid dual operators (unary operations)
/// 
/// The operators to be checked are :
/// - Repetition
/// - Grouping
/// - Optional
/// 
/// # Arguments
/// 
/// * rule - the list of tokens to be checked
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

// Checks if a list of tokens contains valid single operators (binary operations)
/// 
/// The operators to be checked are :
/// - Alternation
/// - Concatenation
/// - Exception
/// 
/// # Arguments
/// 
/// * rule - the list of tokens to be checked
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

/// Checks if a dual operator does not follow an other one
/// 
/// As a dual operator has to be surrounded with sub-rules,
/// it's forbidden for (at least) two following dual operator to be present in a rule definition
/// 
/// # Arguments
/// 
/// * rule - the list of tokens to be checked
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

/// Checks if two tokens are the same (for operators) or equivalent (for symbols)
/// 
/// # Arguments
/// 
/// * token1 - first token
/// * token2 - second token
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
            (Rule::Atomic(s1, AtomicKind::Literal), Rule::Atomic(s2, AtomicKind::Literal)) |
            (Rule::Atomic(s1, AtomicKind::Identifier), Rule::Atomic(s2, AtomicKind::Identifier)) => s1 == s2,
            _ => false,
        }
        _ => false,
    }
}

/// Checks if two list of tokens are equivalent
/// 
/// # Arguments
/// 
/// * token1 - first list of tokens
/// * token2 - second tokens
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

/// Checks if a list of tokens references contains valid dual operators (unary operations)
/// 
/// *Use the [valid_dual_operators] function to check owned tokens*
/// 
/// The operators to be checked are :
/// - Repetition
/// - Grouping
/// - Optional
/// 
/// # Arguments
/// 
/// * rule - the list of tokens to be checked
pub fn valid_dual_ref_operators(rule: &Vec<&Token>) -> bool {
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
    let mut current_op: Option<&&Operator>;
    let mut last_test_op: Option<&&Operator>;
    let size = operators_list.len();
    for i in 1..size {
        current_op = operators_list.get(i);
        last_test_op = operators_test.last();
        match (last_test_op, current_op) {
            (Some(op1), Some(op2)) => {
                if brackets_paired(op1, op2) {
                    operators_test.pop();
                } else {
                    let o = *op2;
                    operators_test.push(o);
                }
            },
            (None, Some(op2)) => {
                let o = *op2;
                operators_test.push(o)
            },
            (_, None) => return operators_test.len() == 0,
        }
    }
operators_test.len() == 0
}

/// Returns a list of tokens references from a list of owned ones
/// 
/// # Arguments
/// 
/// * rule - the rule of tokens to be referenced
pub fn tokens_as_ref(rule: &Vec<Token>) -> Vec<&Token> {
    let mut tokens: Vec<&Token> = Vec::new();
    for tk in rule {
        tokens.push(tk);
    }
    tokens
}