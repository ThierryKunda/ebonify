use std::rc::Rc;
use std::ops::Deref;

use crate::{ebnf_syntax::*, pre_teatment::{brackets_paired}};


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
    let mut rule_as_ref = tokens_as_ref(rule);
    let rule_with_prior_brackets = with_priority_parentheses(&mut rule_as_ref);
    create_rule_tree_by_ref(rule_with_prior_brackets)
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

pub fn with_priority_parentheses<'a, 'b>(rule: &'a mut Vec<&'b Token>) -> Vec<&'b Token> {
    let mut new_rule: Vec<&Token> = Vec::new();
    match rule.len() {
        1 => new_rule.push(rule.first().unwrap()), // with_priority_parentheses(a) -> a
        3 => {
            let (first, mid, last) = (
                *rule.first().unwrap(),
                *rule.get(1).unwrap(),
                *rule.last().unwrap()
            );
            match (first, mid, last) {
                (Token::Op(_), Token::Rl(_), Token::Op(_)) => {
                    // with_priority_parentheses(< a >) -> < a >
                    new_rule.push(first);
                    new_rule.push(mid);
                    new_rule.push(last);
                },
                (Token::Rl(_), Token::Op(op), Token::Rl(_)) => match op {
                    // with_priority_parentheses(a . b) -> ( a . b )
                    Operator::Alternation | Operator::Concatenation | Operator::Exception => {
                        new_rule.push(&Token::Op(Operator::GroupingL));
                        new_rule.push(first);
                        new_rule.push(mid);
                        new_rule.push(last);
                        new_rule.push(&Token::Op(Operator::GroupingR));
                    },
                    _ => rule.push(&Token::Invalid),
                },
                _ => (),
            }
        },
        5 => {
            let (first, second, third, fourth, last) = (
                *rule.get(0).unwrap(),
                *rule.get(1).unwrap(),
                *rule.get(2).unwrap(),
                *rule.get(3).unwrap(),
                *rule.get(4).unwrap()
            );
            match (first, third, last) {
                (Token::Op(_), Token::Op(_), Token::Op(_)) => {
                    // with_priority_parentheses(< a . b >) -> < a . b >
                    new_rule.append(&mut vec![first, second, third, fourth, last]);
                },
                (Token::Rl(_), Token::Rl(_), Token::Rl(_)) => {
                    // with_priority_parentheses(a . b * c) -> ( a . b ) * c if . >> *
                    //                                         a . ( b * c ) else
                    match (second, fourth) {
                        (Token::Op(op1), Token::Op(op2)) => {
                            if has_highter_priority_to(op1, op2) {
                                new_rule.push(&Token::Op(Operator::GroupingL));
                                new_rule.append(&mut vec![first, second, third]);
                                new_rule.push(&Token::Op(Operator::GroupingR));
                                new_rule.append(&mut vec![fourth, last]);
                            } else {
                                new_rule.append(&mut vec![first, second]);
                                new_rule.push(&Token::Op(Operator::GroupingL));
                                new_rule.append(&mut vec![third, fourth, last]);
                                new_rule.push(&Token::Op(Operator::GroupingR));
                            }
                        },
                        _ => new_rule.push(&Token::Invalid),
                    }
                }
                _ => new_rule.push(&Token::Invalid),
            }
        }
        _ => if least_prior_is_unary(rule) {
            // with_priority_parentheses(< A >) -> < with_priority_parentheses(A) >
            let mut sub_rule: Vec<&Token> = Vec::new();
            new_rule.push(*rule.first().unwrap());
            for i in 1..rule.len()-1 {
                sub_rule.push(*rule.get(i).unwrap());
            }
            for el in with_priority_parentheses(&mut sub_rule) {
                new_rule.push(el);
            }
            rule.push(*rule.last().unwrap());
        } else {
            let idx1 = get_least_prior_binary_index(rule);
            match idx1 {
                Some(i) => {
                    let op1 = *rule.get(i).unwrap();
                    let (mut left, mut right) = split_rule_by_index(rule, i);
                    let idx2 = get_least_prior_binary_index(&right);
                    match idx2 {
                        Some(j) => {
                            // with_priority_parentheses(A . B * C) -> with_priority_parentheses(A . B) * with_priority_parentheses(C) if . >> *
                            //                                         with_priority_parentheses(A) . with_priority_parentheses(B * C)
                            let op2 = *rule.get(j).unwrap();
                            let (mut mid, mut end) = split_rule_by_index(&right, i);
                            match (op1, op2) {
                                (Token::Op(o1), Token::Op(o2)) => if has_highter_priority_to(o1, o2) {
                                    new_rule.push(&Token::Op(Operator::GroupingL));
                                    for el in with_priority_parentheses(&mut left) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op1);
                                    for el in with_priority_parentheses(&mut mid) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(&Token::Op(Operator::GroupingR));
                                    new_rule.push(op2);
                                    for el in with_priority_parentheses(&mut end) {
                                        new_rule.push(el);
                                    }

                                } else {
                                    for el in with_priority_parentheses(&mut left) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op1);
                                    new_rule.push(&Token::Op(Operator::GroupingL));
                                    for el in with_priority_parentheses(&mut mid) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op2);
                                    for el in with_priority_parentheses(&mut end) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(&Token::Op(Operator::GroupingR));
                                },
                                _ => new_rule.push(&Token::Invalid),
                            }
                        },
                        None => {
                            // with_priority_parentheses(A . B) -> (with_priority_parentheses(A) . with_priority_parentheses(B))
                            new_rule.push(&Token::Op(Operator::GroupingL));
                            new_rule.append(&mut with_priority_parentheses(&mut left));
                            new_rule.push(op1);
                            new_rule.append(&mut with_priority_parentheses(&mut right));
                            new_rule.push(&Token::Op(Operator::GroupingL));
                        }
                    }

                },
                None => new_rule.push(&Token::Invalid),
            }
        },

    }
    
    new_rule
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
    if rule.len() < 3 {
        return None;
    }
    if rule.len() == 3 {
        return Some(1);
    }
    match rule.first().unwrap() {
        Token::Op(_) => (),
        Token::Rl(_) => return Some(1),
        Token::Invalid => return None,
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
        (&Operator::Alternation, &Operator::Alternation) => false,
        (&Operator::Alternation, _) => true,
        _ => false,
    }
}

pub fn least_prior_is_unary(rule: &Vec<&Token>) -> bool {
    // We borrow the rule without the "extremities"
    let mut test_vec: Vec<&Token> = Vec::new();
    for i in 1..rule.len()-1 {
        test_vec.push(rule.get(i).unwrap());
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