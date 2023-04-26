use crate::ebnf_syntax::{Operator, Token};

use regex;
use std::fs;
use std::path::Path;
use std::rc::Rc;

use crate::ebnf_syntax::*;
use crate::error::ParsingError;

use path_absolutize::*;
/// Split the lines of an input string and return the result
/// 
/// # Arguments
/// 
/// * `content` - The content to be splitted
/// 
/// # Examples
/// ```
/// use ebonify::parsing::pre_processing::split_lines;
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
/// use ebonify::parsing::pre_processing::split_lines_from_file;
/// let lines = split_lines_from_file("tests_samples/separation.txt").unwrap();
/// assert_eq!(lines, vec![
///     String::from("Line 1 : ABCD;"),
///     String::from("Line 2 : EFGH;"),
///     String::from("Line n : ...;")]
/// );
/// ```
pub fn split_lines_from_file(filepath: &str) -> Result<Vec<String>, ParsingError> {
    let p = Path::new(filepath);
    match p.absolutize() {
        Ok(v) => if let Some(path_abs) = v.to_str() {
            let file_content = fs::read_to_string(path_abs);
            match file_content {
                Ok(ct) => {
                    Ok(split_lines(ct))
                },
                Err(_) => {
                    Err(ParsingError::new("impossible to read the file"))
                },
            }
        } else {
            Err(ParsingError::new("valid unicode failed path for string slice generation"))
        },
        Err(_) => Err(ParsingError::new("failed to get the absolute path of the file")),
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
/// use ebonify::parsing::pre_processing::split_members_aux;
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
/// use ebonify::parsing::pre_processing::split_members;
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