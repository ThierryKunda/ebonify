use crate::ebnf_syntax::*;
use crate::pre_treatment::{brackets_paired, valid_single_operators, valid_dual_ref_operators};

pub fn has_highter_priority_to(op1: &Operator, op2: &Operator) -> bool {
    match (op1, op2) {
        (&Operator::Exception, &Operator::Exception) => false,
        (&Operator::Exception, _) => true,
        (&Operator::Concatenation, &Operator::Concatenation) => false,
        (&Operator::Concatenation, _) => true,
        _ => false,
    }
}

pub fn least_prior_is_unary(rule: &Vec<&Token>) -> bool {
    // We borrow the rule without the "extremities"
    let mut test_vec: Vec<&Token> = Vec::new();
    for i in 1..rule.len()-1 {
        test_vec.push(rule.get(i).unwrap());
    }
    return valid_dual_ref_operators(&test_vec) && valid_single_operators(&test_vec);
}

pub fn with_priority_parentheses<'a>(rule: Vec<&'a Token>) -> Vec<&'a Token> {
    let rule_len = rule.len();
    if rule_len == 1 {
        rule
    } else if rule_len == 3 {
        let (first, mid, last) = (
            *rule.first().unwrap(),
            *rule.get(1).unwrap(),
            *rule.last().unwrap()
        );
        match (first, mid, last) {
            // with_priority_parentheses(< a >) -> < a >
            (Token::Op(_), Token::Rl(_), Token::Op(_)) => rule,
            // with_priority_parentheses(a . b) -> ( a . b )
            (Token::Rl(_), Token::Op(_), Token::Rl(_)) => surround_with_grouping(rule),
            _ => vec![&Token::Invalid],
        }
    } else if rule_len == 5 {
        let (first, second, third, fourth, last) = (
            *rule.get(0).unwrap(),
            *rule.get(1).unwrap(),
            *rule.get(2).unwrap(),
            *rule.get(3).unwrap(),
            *rule.get(4).unwrap()
        );
        match (first, third, last) {
            // with_priority_parentheses(< a . b >) -> < a . b >
            (Token::Op(_), Token::Op(_), Token::Op(_)) => rule,
            // with_priority_parentheses(a . b * c) -> ( a . b ) * c if . >> *
            //                                         a . ( b * c ) else
            (Token::Rl(_), Token::Rl(_), Token::Rl(_)) => match (second, fourth) {
                    (Token::Op(op1), Token::Op(op2)) => if has_highter_priority_to(op1, op2) {
                        concat_rules(
                            surround_with_grouping(vec![first, second, third]),
                            vec![fourth, last],
                            None
                        )
                    } else {
                        concat_rules(
                            vec![first, second],
                            surround_with_grouping(vec![third, fourth, last]),
                            None
                        )
                    },
                    _ => vec![&Token::Invalid]
            },
            // < a > . b -> < a > . b 
            // OR
            // a . < b > -> a . < b >
            (Token::Op(_), Token::Op(_), Token::Rl(_)) |
            (Token::Rl(_), Token::Op(_), Token::Op(_)) => rule,
            _ => vec![&Token::Invalid]
        }
    } else {
        if least_prior_is_unary(&rule) {
            // with_priority_parentheses(< A >) -> < with_priority_parentheses(A) >
            let first = *rule.first().unwrap();
            let last = *rule.last().unwrap();
            concat_rules(
                vec![first], 
                concat_rules(
                    with_priority_parentheses(get_rule_without_first_last(rule)), 
                    vec![last],
                    None
                ),
                None
            )
        } else {
            // with_priority_parentheses(A . B * C) -> with_priority_parentheses(A . B) * with_priority_parentheses(C) if . >> *
            //                                         with_priority_parentheses(A) . with_priority_parentheses(B * C)
            let least_prior_idx = get_least_prior_binary_index(&rule);
            match least_prior_idx {
                Some(i) => {
                    let (left_part, right_part) = split_rule_by_index(&rule, i);
                    concat_rules(
                        with_priority_parentheses(left_part),
                        surround_with_grouping(with_priority_parentheses(right_part)),
                        rule.get(i)
                    )
                },
                None => vec![&Token::Invalid],
            }
        }
    }
}

pub fn concat_rules<'a>(mut rule1: Vec<&'a Token>, mut rule2: Vec<&'a Token>, middle: Option<&&'a Token>) -> Vec<&'a Token> {
    if let Some(m) = middle {
        rule1.push(*m);
    }
    rule1.append(&mut rule2);
    rule1
}

pub fn surround_with_grouping<'a>(mut rule: Vec<&'a Token>) -> Vec<&'a Token> {
    rule.insert(0, &Token::Op(Operator::GroupingL));
    rule.push(&Token::Op(Operator::GroupingR));
    rule
}

pub fn split_rule_by_index<'a>(rule: &Vec<&'a Token>, split_pos: usize) -> (Vec<&'a Token>, Vec<&'a Token>) {
    let mut left_part: Vec<&Token> = Vec::new();
    let mut right_part: Vec<&Token> = Vec::new();
    let rl_size = rule.len();
    let split_pos_cp = split_pos.clone();
    for i in 0..split_pos {
        left_part.push(rule.get(i).unwrap());
    }
    for i in split_pos_cp+1..rl_size {
        right_part.push(rule.get(i).unwrap());
    }
    (left_part, right_part)
}

pub fn copy_rule<'a>(rule: &'a Vec<&Token>) -> Vec<&'a Token> {
    let mut new_rule: Vec<&Token> = Vec::new();
    for tk in rule {
        new_rule.push(tk);
    }
    new_rule
}

pub fn get_rule_without_first_last(rule: Vec<&Token>) -> Vec<&Token>{
    let mut new_rule: Vec<&Token> = Vec::new();
    for i in 1..rule.len()-1 {
        new_rule.push(rule.get(i).unwrap());
    }
    return new_rule;
}

pub fn get_least_prior_binary_index(rule: &Vec<&Token>) -> Option<usize> {
    let mut sub_tree_idx = 0_usize; 
    let mut ops_tuples: Vec<(usize, usize, &Operator)> = Vec::new();
    let mut stack_test: Vec<&Operator> = Vec::new();
    for (i, tk) in rule.iter().enumerate() {
        if let Token::Op(op) = tk {
            if is_binary(op) {
                ops_tuples.push((i, sub_tree_idx, op));
            } else {
                if let Some(last) = stack_test.last() {
                    if brackets_paired(last, op) {
                        stack_test.pop();
                        sub_tree_idx -= 1;
                    } else {
                        stack_test.push(op);
                        sub_tree_idx += 1;
                    }
                } else {
                    stack_test.push(op);
                    sub_tree_idx += 1;
                }
            }
        }
    }
    let first_layer_ops: Vec<(usize, usize, &Operator)> = ops_tuples.into_iter()
        .filter(|op| if let (_, 0, _) = op {
            true
        } else {
            false
        })
        .collect();
    let mut res;
    match first_layer_ops.get(0) {
        Some(first) => res = first,
        None => return None,
    }
    for i in 0..first_layer_ops.len() {
        match first_layer_ops.get(i) {
            Some(op) => {
                if has_highter_priority_to(res.2, op.2) {
                    res = op;
                }
            },
            None => (),
        }
    }
    Some(res.0)
    
}

pub fn is_binary(operator: &Operator) -> bool {
    match operator {
        Operator::Alternation => true,
        Operator::Concatenation => true,
        Operator::Exception => true,
        _ => false
    }
}
