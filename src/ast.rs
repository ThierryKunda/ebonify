use std::rc::Rc;
use std::ops::Deref;

use crate::{ebnf_syntax::*, pre_teatment::{brackets_paired, tokenize_rule_from_str, split_members, is_binary, valid_single_operators}};


pub fn are_same_tree<'a>(rule_1: &'a Rule, rule_2: &'a Rule) -> bool {
    match (rule_1, rule_2) {
        (
            Rule::Literal(s1),
            Rule::Literal(s2),
        ) |
        (
            Rule::Identifier(s1),
            Rule::Identifier(s2),
        ) => s1 == s2,
        (
            Rule::RepetRef(el1),
            Rule::RepetRef(el2),
        ) | 
        (
            Rule::GrpRef(el1),
            Rule::GrpRef(el2),
        ) |
        (
            Rule::OptRef(el1),
            Rule::OptRef(el2),
        ) |
        (
            Rule::Ref(el1),
            Rule::Ref(el2),
        ) => match ((*el1).upgrade(), (*el2).upgrade()) {
            (Some(a), Some(b)) => are_same_tree(a.deref(), b.deref()),
            _ => false,
        },

        (
            Rule::Repetition(el1),
            Rule::Repetition(el2),
        ) |
        (
            Rule::Grouping(el1),
            Rule::Grouping(el2),
        ) |
        (
            Rule::Optional(el1),
            Rule::Optional(el2),
        ) => are_same_tree(el1, el2),


        (
            Rule::AlterRef(sub_1, sub_2),
            Rule::AlterRef(sub_3, sub_4),
        ) |
        (
            Rule::ConcatRef(sub_1, sub_2),
            Rule::ConcatRef(sub_3, sub_4),
        ) |
        (
            Rule::ExceptRef(sub_1, sub_2),
            Rule::ExceptRef(sub_3, sub_4),
        ) => match ((*sub_1).upgrade(), (*sub_2).upgrade(), (*sub_3).upgrade(), (*sub_4).upgrade()) {
            (Some(a), Some(b), Some(c), Some(d)) => {
                are_same_tree(a.deref(), c.deref()) &&
                are_same_tree(b.deref(), d.deref())
            }
            _ => false,
        },
        (
            Rule::AlterRefL(sub_1, sub_2),
            Rule::AlterRefL(sub_3, sub_4),
        ) |
        (
            Rule::ConcatRefL(sub_1, sub_2),
            Rule::ConcatRefL(sub_3, sub_4),
        )
        |
        (
            Rule::ExceptRefL(sub_1, sub_2),
            Rule::ExceptRefL(sub_3, sub_4),
        ) => match ((*sub_1).upgrade(), (*sub_3).upgrade()) {
            (Some(a), Some(b)) => {
                are_same_tree(a.deref(), b.deref()) &&
                are_same_tree(sub_2, sub_4)
            }
            _ => false,
        },
        (
            Rule::AlterRefR(sub_1, sub_2),
            Rule::AlterRefR(sub_3, sub_4),
        ) |
        (
            Rule::ConcatRefR(sub_1, sub_2),
            Rule::ConcatRefR(sub_3, sub_4),
        )
        |
        (
            Rule::ExceptRefR(sub_1, sub_2),
            Rule::ExceptRefR(sub_3, sub_4),
        ) => match ((*sub_2).upgrade(), (*sub_4).upgrade()) {
            (Some(a), Some(b)) => {
                are_same_tree(a.deref(), b.deref()) &&
                are_same_tree(sub_1, sub_3)
            }
            _ => false,
        },

        (
            Rule::Alternation(sub_1, sub_2),
            Rule::Alternation(sub_3, sub_4),
        ) |
        (
            Rule::Concatenation(sub_1, sub_2),
            Rule::Concatenation(sub_3, sub_4),
        )
        |
        (
            Rule::Exception(sub_1, sub_2),
            Rule::Exception(sub_3, sub_4),
        ) => are_same_tree(sub_1, sub_3) && are_same_tree(sub_2, sub_4),

        _ => false
    }
}



pub fn create_definition_tree<'a>(rule: &'a Vec<Token>) -> Rule {
    let rule_tree = create_rule_tree(rule);
    
    tree_without_grouping(Rc::new(rule_tree))
}

pub fn tree_without_grouping(rule: Rc<Rule>) -> Rule {
    match rule.deref() {
        Rule::Literal(_) | Rule::Identifier(_) | Rule::Ref(_) 
        | Rule::AlterRef(_, _) | Rule::ConcatRef(_, _) | Rule::ExceptRef(_, _)
        | Rule::RepetRef(_) | Rule::OptRef(_) => copy_rule_tree(rule),
        Rule::GrpRef(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rule::Ref(Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ConcatRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rule::ConcatRefL(Rc::downgrade(&el), Rc::new(tree_without_grouping(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        }
        Rule::AlterRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rule::AlterRefL(Rc::downgrade(&el), Rc::new(tree_without_grouping(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        }
        Rule::ExceptRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rule::ExceptRefL(Rc::downgrade(&el), Rc::new(tree_without_grouping(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ConcatRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rule::ConcatRefR(Rc::new(tree_without_grouping(Rc::clone(sub_1))), Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::AlterRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rule::AlterRefR(Rc::new(tree_without_grouping(Rc::clone(sub_1))), Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ExceptRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rule::ExceptRefR(Rc::new(tree_without_grouping(Rc::clone(sub_1))), Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::Repetition(sub_tree) => Rule::Repetition(
            Rc::new(tree_without_grouping(Rc::clone(sub_tree))),
        ),
        Rule::Optional(sub_tree) => Rule::Optional(
            Rc::new(tree_without_grouping(Rc::clone(sub_tree))),
        ),
        Rule::Grouping(sub_tree) => tree_without_grouping(Rc::clone(sub_tree)),
        Rule::Concatenation(sub_1, sub_2) => Rule::Concatenation(
            Rc::new(tree_without_grouping(Rc::clone(sub_1))),
            Rc::new(tree_without_grouping(Rc::clone(sub_2)))
        ),
        Rule::Alternation(sub_1, sub_2) => Rule::Alternation(
            Rc::new(tree_without_grouping(Rc::clone(sub_1))),
            Rc::new(tree_without_grouping(Rc::clone(sub_2)))
        ),
        Rule::Exception(sub_1, sub_2) => Rule::Exception(
            Rc::new(tree_without_grouping(Rc::clone(sub_1))),
            Rc::new(tree_without_grouping(Rc::clone(sub_2)))
        ),
    }
}

pub fn copy_rule_tree(rule: Rc<Rule>) -> Rule {
    match rule.deref() {
        Rule::Literal(lit) => Rule::Literal(lit.to_string()),
        Rule::Identifier(id) => Rule::Identifier(id.to_string()),
        Rule::Ref(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rule::Ref(Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::RepetRef(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rule::RepetRef(Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::GrpRef(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rule::GrpRef(Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::OptRef(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rule::OptRef(Rc::downgrade(&el)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ConcatRef(sub1, sub2) => match (sub1.upgrade(), sub2.upgrade()) {
            (Some(s1), Some(s2)) => Rule::ConcatRef(Rc::downgrade(&s1), Rc::downgrade(&s2)),
            _ => Rule::Identifier("Invalid".to_string()),
        },
        Rule::AlterRef(sub1, sub2) => match (sub1.upgrade(), sub2.upgrade()) {
            (Some(s1), Some(s2)) => Rule::AlterRef(Rc::downgrade(&s1), Rc::downgrade(&s2)),
            _ => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ExceptRef(sub1, sub2) => match (sub1.upgrade(), sub2.upgrade()) {
            (Some(s1), Some(s2)) => Rule::ExceptRef(Rc::downgrade(&s1), Rc::downgrade(&s2)),
            _ => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ConcatRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(s1) => Rule::ConcatRefL(Rc::downgrade(&s1), Rc::new(copy_rule_tree(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::AlterRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(s1) => Rule::AlterRefL(Rc::downgrade(&s1), Rc::new(copy_rule_tree(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ExceptRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(s1) => Rule::ExceptRefL(Rc::downgrade(&s1), Rc::new(copy_rule_tree(Rc::clone(sub_2)))),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ConcatRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(s2) => Rule::ConcatRefR(Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::downgrade(&s2)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::AlterRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(s2) => Rule::AlterRefR(Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::downgrade(&s2)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::ExceptRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(s2) => Rule::ExceptRefR(Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::downgrade(&s2)),
            None => Rule::Identifier("Invalid".to_string()),
        },
        Rule::Repetition(sub_tree) => Rule::Repetition(Rc::new(copy_rule_tree(Rc::clone(sub_tree)))),
        Rule::Grouping(sub_tree) => Rule::Grouping(Rc::new(copy_rule_tree(Rc::clone(sub_tree)))),
        Rule::Optional(sub_tree) => Rule::Optional(Rc::new(copy_rule_tree(Rc::clone(sub_tree)))),
        Rule::Concatenation(sub_1, sub_2) => Rule::Concatenation(
            Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::new(copy_rule_tree(Rc::clone(sub_2)))
        ),
        Rule::Alternation(sub_1, sub_2) => Rule::Alternation(
            Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::new(copy_rule_tree(Rc::clone(sub_2)))
        ),
        Rule::Exception(sub_1, sub_2) => Rule::Exception(
            Rc::new(copy_rule_tree(Rc::clone(sub_1))), Rc::new(copy_rule_tree(Rc::clone(sub_2)))
        ),
    }
}

pub fn create_rule_tree<'a>(rule: &'a Vec<Token>) -> Rule {
    let rule_as_ref = tokens_as_ref(rule);
    let rule_with_prior_brackets = with_priority_parentheses(rule_as_ref);
    println!();
    let res = create_rule_tree_by_ref(rule_with_prior_brackets);
    res
}

pub fn create_rule_tree_by_ref(rule: Vec<&Token>) -> Rule {
    if rule.len() == 1 {
        match rule.first().unwrap() {
            Token::Rl(rl) => match rl.deref() {
                Rule::Literal(lit) => return Rule::Literal(lit.to_string()),
                Rule::Identifier(id) => return Rule::Identifier(id.to_string()),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            _ => return Rule::Identifier("Invalid".to_string()),
        };
    }
    if rule.len() == 3 {
        match (rule.first().unwrap(), rule.get(1).unwrap(), rule.last().unwrap()) {
            (Token::Op(op1), Token::Rl(rl), Token::Op(_)) => match op1 {
                Operator::OptionalL => return Rule::OptRef(Rc::downgrade(rl)),
                Operator::RepetitionL => return Rule::RepetRef(Rc::downgrade(rl)),
                Operator::GroupingL => return Rule::GrpRef(Rc::downgrade(rl)),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            (Token::Rl(rl1), Token::Op(op), Token::Rl(rl2)) => match op {
                Operator::Alternation => return Rule::AlterRef(Rc::downgrade(rl1), Rc::downgrade(rl2)),
                Operator::Concatenation => return Rule::ConcatRef(Rc::downgrade(rl1), Rc::downgrade(rl2)),
                Operator::Exception => return Rule::ExceptRef(Rc::downgrade(rl1), Rc::downgrade(rl2)),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            
            _ => return Rule::Identifier("Invalid".to_string()),
        };
    }
    if least_prior_is_unary(&rule) {
        match rule.first().unwrap() {
            Token::Op(Operator::RepetitionL) => return Rule::Repetition(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            Token::Op(Operator::OptionalL) => return Rule::Optional(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            Token::Op(Operator::GroupingL) => return Rule::Grouping(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            _ => Rule::Identifier("Invalid".to_string()),
        };
    }
    let i = get_least_prior_binary_index(&rule);
    match i {
        Some(idx) => {
            let el = *rule.get(idx).unwrap();
            let (left_part, right_part) = split_rule_by_index(&rule, idx);
            match el {
                Token::Op(Operator::Alternation) => return Rule::Alternation(Rc::new(create_rule_tree_by_ref(left_part)), Rc::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Concatenation) => return Rule::Concatenation(Rc::new(create_rule_tree_by_ref(left_part)), Rc::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Exception) => return Rule::Exception(Rc::new(create_rule_tree_by_ref(left_part)), Rc::new(create_rule_tree_by_ref(right_part))),
                _ => return Rule::Identifier("Invalid".to_string()),
            }
        }
        None => Rule::Identifier("Invalid".to_string()),
    };
    Rule::Identifier("Invalid".to_string())
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
            }
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

pub fn tokens_as_ref(rule: &Vec<Token>) -> Vec<&Token> {
    let mut tokens: Vec<&Token> = Vec::new();
    for tk in rule {
        tokens.push(tk);
    }
    tokens
}

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

pub fn create_trees(definitions: Vec<String>) -> Vec<(String, Rule)> {   
    let mut trees: Vec<(String, Rule)> = Vec::new();
    let name_def_pairs = split_members(definitions);
    for def in name_def_pairs {
        let r = tokenize_rule_from_str(def.1);
        trees.push((def.0, create_definition_tree(&r)));
    }
    trees
}