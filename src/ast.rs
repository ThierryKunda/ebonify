use crate::{ebnf_syntax::*, pre_teatment::{brackets_paired}};



pub fn get_least_prior_binary_index(rule: &Vec<&Token>) -> Option<usize> {
    if rule.len() < 3 {
        return None;
    }
    if rule.len() == 3 {
        return Some(1);
    }
    let mut test_stack: Vec<&Token> = Vec::new();
    test_stack.push(rule.first().unwrap());
    let mut i = 1;
    while !test_stack.is_empty() && i < rule.len()-1 {
        match (test_stack.last().unwrap(), rule.get(i).unwrap()) {
            (Token::Op(last_of_stack), Token::Op(current)) => {
                match (last_of_stack, current) {
                    (Operator::RepetitionL, Operator::RepetitionR) |
                    (Operator::OptionalL, Operator::OptionalR) |
                    (Operator::GroupingL, Operator::GroupingR) => { test_stack.pop(); },
                    (_, Operator::Alternation) | (_, Operator::Concatenation) | (_, Operator::Exception) => (),
                    _ => test_stack.push(rule.get(i).unwrap()),
                }
            }
            _ => ()
        }
        i += 1;
    }
    for j in i..rule.len()-1 {
        match rule.get(j).unwrap() {
            Token::Op(op) => match op {
                Operator::Alternation | Operator::Concatenation | Operator::Exception => return Some(j),
                _ => (),
            }
            _ => ()
        }
    }
    return None;
}

pub fn tokens_as_ref<'a>(rule: &'a Vec<Token<'a>>) -> Vec<&'a Token<'a>> {
    let mut tokens: Vec<&Token> = Vec::new();
    for tk in rule {
        tokens.push(tk);
    }
    tokens
}

pub fn least_prior_is_unary(rule: &Vec<Token>) -> bool {
    // We borrow the rule without the "extremities"
    let tokens_test = rule.get(1..rule.len()-2).unwrap();
    let mut test_vec: Vec<&Token> = Vec::new();
    for tk in tokens_test {
        test_vec.push(tk);
    }
    return valid_dual_ref_operators(&test_vec);
}

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