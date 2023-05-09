use crate::ebnf_syntax::{Operator, Token};

use std::ops::Deref;

use crate::ebnf_syntax::*;

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
/// use ebonify::parsing::pre_processing::tokenize_rule_from_str;
/// use ebonify::parsing::validation::validate_rule;
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
/// use ebonify::parsing::validation::brackets_paired;
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

/// Checks if a list of tokens contains valid single operators (binary operations)
/// on tokens
/// 
/// The operators to be checked are :
/// - Alternation
/// - Concatenation
/// - Exception
/// 
/// # Arguments
/// 
/// * rule - the list of tokens to be checked
pub fn valid_pure_single_operators(rule: &Vec<Token>) -> bool {
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

/// Alternative of [valid_pure_single_operators] using tokens references
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