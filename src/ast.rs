use std::rc::Rc;
use std::ops::Deref;

use crate::{ebnf_syntax::*, pre_treatment::{brackets_paired, tokenize_rule_from_str, split_members, is_binary, valid_single_operators}};

pub fn get_pure_tree(rule: Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Literal(lit) => Rc::new(Rule::Literal(lit.to_string())),
        Rule::Identifier(id) => Rc::new(Rule::Identifier(id.to_string())),
        Rule::AlterRef(left, right) => match (left.upgrade(), right.upgrade()) {
            (Some(a), Some(b)) => match (a.deref(), b.deref()) {
                (Rule::Identifier(id1), Rule::Identifier(id2)) => Rc::new(
                    Rule::Alternation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Identifier(id2.to_string()))
                    )
                ),
                (Rule::Identifier(id1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Alternation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(lit1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Alternation(
                        Rc::new(Rule::Identifier(lit1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(id1), Rule::Identifier(lit2)) => Rc::new(
                    Rule::Alternation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                _ => rule
            },
            (None, Some(b)) => match b.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::AlterRefL(
                        left.to_owned(),
                        Rc::new(Rule::Literal(lit.to_string()))
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::AlterRefL(
                        left.to_owned(),
                        Rc::new(Rule::Identifier(id.to_string()))
                    )
                ),
                _ => rule
                
            },
            (Some(a), None) => match a.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::AlterRefR(
                        Rc::new(Rule::Literal(lit.to_string())),
                        right.to_owned()
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::AlterRefR(
                        Rc::new(Rule::Identifier(id.to_string())),
                        right.to_owned()
                    )
                ),
                _ => rule
                
            },
            (None, None) => rule,
            
        },
        Rule::ConcatRef(left, right) => match (left.upgrade(), right.upgrade()) {
            (Some(a), Some(b)) => match (a.deref(), b.deref()) {
                (Rule::Identifier(id1), Rule::Identifier(id2)) => Rc::new(
                    Rule::Concatenation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Identifier(id2.to_string()))
                    )
                ),
                (Rule::Identifier(id1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Concatenation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(lit1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Concatenation(
                        Rc::new(Rule::Identifier(lit1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(id1), Rule::Identifier(lit2)) => Rc::new(
                    Rule::Concatenation(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                _ => rule
            },
            (None, Some(b)) => match b.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::ConcatRefL(
                        left.to_owned(),
                        Rc::new(Rule::Literal(lit.to_string()))
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::ConcatRefL(
                        left.to_owned(),
                        Rc::new(Rule::Identifier(id.to_string()))
                    )
                ),
                _ => rule
                
            },
            (Some(a), None) => match a.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::ConcatRefR(
                        Rc::new(Rule::Literal(lit.to_string())),
                        right.to_owned()
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::ConcatRefR(
                        Rc::new(Rule::Identifier(id.to_string())),
                        right.to_owned()
                    )
                ),
                _ => rule
                
            },
            (None, None) => rule,
            
        },
        Rule::ExceptRef(left, right) => match (left.upgrade(), right.upgrade()) {
            (Some(a), Some(b)) => match (a.deref(), b.deref()) {
                (Rule::Identifier(id1), Rule::Identifier(id2)) => Rc::new(
                    Rule::Exception(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Identifier(id2.to_string()))
                    )
                ),
                (Rule::Identifier(id1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Exception(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(lit1), Rule::Literal(lit2)) => Rc::new(
                    Rule::Exception(
                        Rc::new(Rule::Identifier(lit1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                (Rule::Literal(id1), Rule::Identifier(lit2)) => Rc::new(
                    Rule::Exception(
                        Rc::new(Rule::Identifier(id1.to_string())),
                        Rc::new(Rule::Literal(lit2.to_string()))
                    )
                ),
                _ => rule
            },
            (None, Some(b)) => match b.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::ExceptRefL(
                        left.to_owned(),
                        Rc::new(Rule::Literal(lit.to_string()))
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::ExceptRefL(
                        left.to_owned(),
                        Rc::new(Rule::Identifier(id.to_string()))
                    )
                ),
                _ => rule
                
            },
            (Some(a), None) => match a.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::ExceptRefR(
                        Rc::new(Rule::Literal(lit.to_string())),
                        right.to_owned()
                    )
                ),
                Rule::Identifier(id) => Rc::new(
                    Rule::ExceptRefR(
                        Rc::new(Rule::Identifier(id.to_string())),
                        right.to_owned()
                    )
                ),
                _ => rule
                
            },
            (None, None) => rule,
            
        },
        Rule::RepetRef(sub) => match sub.deref().upgrade() {
            Some(st) => match st.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::Repetition(
                        Rc::new(Rule::Literal(lit.to_string()))
                    ),
                ),
                Rule::Identifier(lit) => Rc::new(
                    Rule::Repetition(
                        Rc::new(Rule::Identifier(lit.to_string()))
                    ),
                ),
                
                _ => rule,
            }
            _ => rule
        },
        Rule::GrpRef(sub) => match sub.deref().upgrade() {
            Some(st) => match st.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::Grouping(
                        Rc::new(Rule::Literal(lit.to_string()))
                    ),
                ),
                Rule::Identifier(lit) => Rc::new(
                    Rule::Grouping(
                        Rc::new(Rule::Identifier(lit.to_string()))
                    ),
                ),
                
                _ => rule,
            }
            _ => rule
        },
        Rule::OptRef(sub) => match sub.deref().upgrade() {
            Some(st) => match st.deref() {
                Rule::Literal(lit) => Rc::new(
                    Rule::Optional(
                        Rc::new(Rule::Literal(lit.to_string()))
                    ),
                ),
                Rule::Identifier(lit) => Rc::new(
                    Rule::Optional(
                        Rc::new(Rule::Identifier(lit.to_string()))
                    ),
                ),
                
                _ => rule,
            }
            _ => rule
        },
        Rule::AlterRefL(_, _) | Rule::AlterRefR(_, _) |
        Rule::ConcatRefL(_, _) | Rule::ConcatRefR(_, _) |
        Rule::ExceptRefL(_, _) | Rule::ExceptRefR(_, _) | 
        Rule::Ref(_) => rule,
        Rule::Alternation(left, right) => Rc::new(
            Rule::Alternation(
                get_pure_tree(Rc::clone(left)),
                get_pure_tree(Rc::clone(right)),
            )
        ),
        Rule::Concatenation(left, right) => Rc::new(
            Rule::Concatenation(
                get_pure_tree(Rc::clone(left)),
                get_pure_tree(Rc::clone(right)),
            )
        ),
        Rule::Exception(left, right) => Rc::new(
            Rule::Exception(
                get_pure_tree(Rc::clone(left)),
                get_pure_tree(Rc::clone(right)),
            )
        ),
        Rule::Repetition(sub) => Rc::new(
            Rule::Repetition(
                get_pure_tree(Rc::clone(sub))
            )
        ),
        Rule::Grouping(sub) => Rc::new(
            Rule::Grouping(
                get_pure_tree(Rc::clone(sub))
            )
        ),
        Rule::Optional(sub) => Rc::new(
            Rule::Optional(
                get_pure_tree(Rc::clone(sub))
            )
        ),
        
    }
}

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



pub fn create_definition_tree<'a>(rule: &'a Vec<Token>) -> Rc<Rule> {
    let rule_tree = create_rule_tree(rule);
    tree_without_grouping(Rc::new(rule_tree))
}

pub fn tree_without_grouping(rule: Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Literal(_) | Rule::Identifier(_) | Rule::Ref(_) 
        | Rule::AlterRef(_, _) | Rule::ConcatRef(_, _) | Rule::ExceptRef(_, _)
        | Rule::RepetRef(_) | Rule::OptRef(_) => Rc::clone(&rule),
        Rule::GrpRef(sub_tree) => match sub_tree.upgrade() {
            Some(el) => Rc::clone(&el),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        },
        Rule::ConcatRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rc::new(Rule::ConcatRefL(Rc::downgrade(&el), tree_without_grouping(Rc::clone(sub_2)))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        }
        Rule::AlterRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rc::new(Rule::AlterRefL(Rc::downgrade(&el), tree_without_grouping(Rc::clone(sub_2)))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        }
        Rule::ExceptRefL(sub_1, sub_2) => match sub_1.upgrade() {
            Some(el) => Rc::new(Rule::ExceptRefL(Rc::downgrade(&el), tree_without_grouping(Rc::clone(sub_2)))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        },
        Rule::ConcatRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rc::new(Rule::ConcatRefR(tree_without_grouping(Rc::clone(sub_1)), Rc::downgrade(&el))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        },
        Rule::AlterRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rc::new(Rule::AlterRefR(tree_without_grouping(Rc::clone(sub_1)), Rc::downgrade(&el))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        },
        Rule::ExceptRefR(sub_1, sub_2) => match sub_2.upgrade() {
            Some(el) => Rc::new(Rule::ExceptRefR(tree_without_grouping(Rc::clone(sub_1)), Rc::downgrade(&el))),
            None => Rc::new(Rule::Identifier("Invalid".to_string())),
        },
        Rule::Repetition(sub_tree) => Rc::new(
            Rule::Repetition(
                tree_without_grouping(Rc::clone(sub_tree))
            )
        ),
        Rule::Optional(sub_tree) => Rc::new(
            Rule::Optional(
                tree_without_grouping(Rc::clone(sub_tree))
            )
        ),
        Rule::Grouping(sub_tree) => tree_without_grouping(Rc::clone(sub_tree)),
        Rule::Concatenation(sub_1, sub_2) => Rc::new(
            Rule::Concatenation(
                tree_without_grouping(Rc::clone(sub_1)),
                tree_without_grouping(Rc::clone(sub_2))
            )
        ),
        Rule::Alternation(sub_1, sub_2) => Rc::new(
            Rule::Alternation(
                tree_without_grouping(Rc::clone(sub_1)),
                tree_without_grouping(Rc::clone(sub_2))
            )
        ),
        Rule::Exception(sub_1, sub_2) => Rc::new(
            Rule::Exception(
                tree_without_grouping(Rc::clone(sub_1)),
                tree_without_grouping(Rc::clone(sub_2))
            )
        ),
    }
}

pub fn predicate_single_result<PA, PR, VS, VD>(rule: &Rc<Rule>, pred_atomic: &PA, pred_ref: &PR, op_on_single_truthness: &VS, op_on_dual_truthness: &VD) -> bool
    where
        PA: Fn(&Rule) -> bool,
        PR: Fn(&Weak<Rule>) -> bool,
        VS: Fn(&Rule, bool) -> bool,
        VD: Fn(&Rule, bool, bool) -> bool {
    match rule.deref() {
        Rule::Literal(_) |
        Rule::Identifier(_) => pred_atomic(rule.deref()),
        Rule::Alternation(left, right) |
        Rule::Concatenation(left, right) |
        Rule::Exception(left, right)
        => op_on_dual_truthness(
            rule,
            predicate_single_result(left, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness),
            predicate_single_result(right, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness)
        ),
        Rule::Repetition(sub) |
        Rule::Grouping(sub) |
        Rule::Optional(sub) => op_on_single_truthness(rule, predicate_single_result(sub, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness)),
        Rule::Ref(r) |
        Rule::RepetRef(r) |
        Rule::GrpRef(r) |
        Rule::OptRef(r)
        => op_on_single_truthness(rule, pred_ref(r)),
        Rule::AlterRefL(left, right) |
        Rule::ConcatRefL(left, right) |
        Rule::ExceptRefL(left, right) => op_on_dual_truthness(
            rule,
            pred_ref(left),
            predicate_single_result(right, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness)
        ),
        Rule::AlterRefR(left, right) |
        Rule::ConcatRefR(left, right) |
        Rule::ExceptRefR(left, right) => op_on_dual_truthness(
            rule,
            predicate_single_result(left, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness),
            pred_ref(right)
        ),
        Rule::ConcatRef(left, right) |
        Rule::AlterRef(left, right) |
        Rule::ExceptRef(left, right) => op_on_dual_truthness(rule, pred_ref(left), pred_ref(right)),
    }
}

pub fn create_rule_tree<'a>(rule: &'a Vec<Token>) -> Rule {
    let rule_as_ref = tokens_as_ref(rule);
    let rule_with_prior_brackets = with_priority_parentheses(rule_as_ref);
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

pub fn create_trees(definitions: Vec<String>) -> Vec<(String, Rc<Rule>)> {   
    let mut trees: Vec<(String, Rc<Rule>)> = Vec::new();
    let name_def_pairs = split_members(definitions);
    for def in name_def_pairs {
        let r = tokenize_rule_from_str(def.1);
        let t = create_definition_tree(&r);
        trees.push((def.0, t));
    }
    trees
}