use crate::{ebnf_syntax::*, pre_teatment::{brackets_paired, valid_dual_operators}};

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