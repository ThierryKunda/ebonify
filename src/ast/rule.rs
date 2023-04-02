use std::ops::Deref;
use std::rc::{Rc, Weak};

use crate::pre_treatment::{tokenize_rule_from_str, split_members, tokens_as_ref};
use crate::utils::{Counter, diff_str};

use crate::ebnf_syntax::*;

use super::tokens::*;

pub fn grammarize_optional(rule: &Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Atomic(_, _) | Rule::Ref(_) => Rc::clone(rule),
        // gram(<a>) -> <gram(a)>
        Rule::Single(sub, kind) => Rc::new(Rule::Single(
            grammarize_optional(sub),
            kind.clone()
        )),
        Rule::Dual(left, kind, right) => match (left.deref(), right.deref()) {
            // gram([a] . [b]) -> gram(a) | gram(b) | (gram(a) . gram(b))
            (Rule::Single(sub1, SingleKind::Optional), Rule::Single(sub2, SingleKind::Optional)) => Rc::new(Rule::Dual(
                grammarize_optional(sub1),
                DualKind::Alternation,
                Rc::new(Rule::Dual(
                    grammarize_optional(sub2), 
                    DualKind::Alternation, 
                    Rc::new(Rule::Dual(
                        grammarize_optional(sub1), 
                        kind.clone(), 
                        grammarize_optional(sub2)
                    ))
                ))
            )),
            // gram(a . [b]) -> gram(a) | (gram(a) . gram(b))
            (_, Rule::Single(sub, SingleKind::Optional)) => Rc::new(Rule::Dual(
                grammarize_optional(left),
                DualKind::Alternation,
                Rc::new(Rule::Dual(
                    grammarize_optional(left), 
                    kind.clone(), 
                    grammarize_optional(sub)
                )))),
            // gram([a] . b)
            (Rule::Single(sub, SingleKind::Optional), _) => if let DualKind::Exception = kind {
                // gram([a] - b) -> "" | gram(a) - gram(b) if . is -
                Rc::new(Rule::Dual(
                    Rc::new(Rule::Atomic(String::from(""), AtomicKind::Literal)),
                    DualKind::Alternation,
                    Rc::new(Rule::Dual(
                        grammarize_optional(sub),
                        kind.clone(),
                        grammarize_optional(right)
                    ))))
            } else {
                // gram([a] . b) -> gram(b) | gram(a) . gram(b) else
                Rc::new(Rule::Dual(
                    grammarize_optional(right),
                    DualKind::Alternation,
                    Rc::new(Rule::Dual(
                        grammarize_optional(sub), 
                        kind.clone(), 
                        grammarize_optional(right)
                    ))))
            },
            // gram(a . b) -> gram(a) . gram(b)
            _ => Rc::new(Rule::Dual(
                grammarize_optional(left),
                kind.clone(),
                grammarize_optional(right)
            ))
        }
    }
}

fn grammarize_exc_asc(rule: &Rc<Rule>) -> Rc<Rule> {

    match rule.deref() {
        Rule::Atomic(_,_) | Rule::Ref(_) => Rc::clone(rule),
        Rule::Dual(left, kind, right) => match (left.deref(), kind, right.deref()) {
            (
                Rule::Atomic(txt1, AtomicKind::Literal),
                DualKind::Exception,
                Rule::Atomic(txt2, AtomicKind::Literal)
            ) => Rc::new(Rule::Atomic(diff_str(txt1, txt2), AtomicKind::Literal)),
            (
                Rule::Atomic(txt1, AtomicKind::Literal),
                DualKind::Concatenation,
                Rule::Atomic(txt2, AtomicKind::Literal)
            ) => Rc::new(Rule::Atomic(txt1.clone()+txt2, AtomicKind::Literal)),
            _ => {
                let gram_r1 = grammarize_exc_asc(left);
                let gram_r2 = grammarize_exc_asc(right);
                match (gram_r1.deref(), kind, gram_r2.deref()) {
                    (
                        Rule::Atomic(s1, AtomicKind::Literal),
                        DualKind::Exception,
                        Rule::Atomic(s2, AtomicKind::Literal)
                    ) => Rc::new(Rule::Atomic(
                        diff_str(s1, s2),
                        AtomicKind::Literal
                    )),
                    (
                        Rule::Atomic(s1, AtomicKind::Literal),
                        DualKind::Concatenation,
                        Rule::Atomic(s2, AtomicKind::Literal)
                    ) => Rc::new(Rule::Atomic(
                        s1.clone()+s2, AtomicKind::Literal
                    )),
                    _ => Rc::new(Rule::Dual(gram_r1, kind.clone(), gram_r2))
                }
            }
        },
        Rule::Single(sub, kind) => {
            let gram_sub = grammarize_exc_asc(sub);
            Rc::new(Rule::Single(gram_sub, kind.clone()))
        }

    }
}

fn grammarize_exc_desc(rule: &Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Atomic(_,_) | Rule::Ref(_) => Rc::clone(rule),
        Rule::Single(sub, kind) => Rc::new(Rule::Single(
            grammarize_exc_desc(sub),
            kind.clone() 
        )),
        Rule::Dual(left, DualKind::Exception, right) => match (left.deref(), right.deref()) {
            // Exception between two literals
            (Rule::Atomic(s1, AtomicKind::Literal), Rule::Atomic(s2, AtomicKind::Literal)) => Rc::new(Rule::Atomic(
                diff_str(s1, s2),
                AtomicKind::Literal
            )),
            // Exception between any atomic-kind rules
            (Rule::Atomic(s1, k1), Rule::Atomic(s2, k2)) => Rc::new(Rule::Dual(
                Rc::new(Rule::Atomic(s1.to_string(), k1.clone())),
                DualKind::Exception,
                Rc::new(Rule::Atomic(s2.to_string(), k2.clone()))
            )),
            // Exception between a ref and another rule (or vice-versa)
            (Rule::Ref(_), _) | (_, Rule::Ref(_)) => Rc::new(Rule::Dual(
                Rc::clone(left), 
                DualKind::Exception,
                Rc::clone(right) 
            )),
            // gram(a - (b . c)) -> gram(a - b) . gram(a - c)
            (_, Rule::Dual(b, knd, c)) => match knd {
                // gram(a - (b | c)) -> gram(a - b) | gram(a - c)
                DualKind::Alternation => todo!(),
                // gram(a - (b , c)) -> gram(a) - gram(b , c)
                DualKind::Concatenation => todo!(),
                // gram(a - (b - c)) -> gram(a) - gram(b - c)
                DualKind::Exception => todo!(),
            },
            // gram((a . b) - c) -> gram(a - c) . gram(b - c)
            (Rule::Dual(a, knd, b), _) => match knd {
                // gram((a | b) - c) -> gram(a - c) | gram(b - c)
                DualKind::Alternation => todo!(),
                // gram((a - b) , c) -> gram(a - b) , gram(c)
                DualKind::Concatenation => todo!(),
                // gram((a - b) - c) -> gram(a - b) | gram(a - c)
                DualKind::Exception => todo!()
            },
            // gram(<a> - <b>) -> gram(<a - b>)
            (Rule::Single(sub1, knd1), Rule::Single(sub2, knd2)) => if same_single_kind(knd1, knd2) {
                grammarize_exc_desc(
                    &Rc::new(Rule::Single(
                            Rc::new(
                                Rule::Dual(Rc::clone(sub1), DualKind::Exception, Rc::clone(sub2))
                            ), 
                            knd1.clone()
                        ),
                    ),
                )
            } else {
            // gram(<a> - ?b?) -> gram(<a - ?b?>)
            grammarize_exc_desc(
                &Rc::new(Rule::Single(
                        Rc::new(
                            Rule::Dual(Rc::clone(sub1), DualKind::Exception, Rc::new(
                                Rule::Single(Rc::clone(sub2), knd2.clone())
                            ))
                        ), 
                        knd1.clone()
                    ),
                ),
            )
            },
            _ => Rc::new(Rule::Dual(
                grammarize_exc_desc(left),
                DualKind::Exception,
                grammarize_exc_desc(right)
            ))
        },
        Rule::Dual(left, kind, right) => Rc::new(Rule::Dual(
            grammarize_exc_desc(left), 
            kind.clone(),
            grammarize_exc_desc(right) 
        )),
    }
}

pub fn grammarize_exception(rule: &Rc<Rule>) -> Rc<Rule> {
    grammarize_exc_asc(&grammarize_exc_desc(rule))
}

pub fn grammarize_repetition(rule: &Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Atomic(_, _) | Rule::Ref(_) => Rc::clone(rule),
        Rule::Single(sub, SingleKind::Repetition) => Rc::new(Rule::Dual(
            grammarize_repetition(sub),
            DualKind::Alternation,
            Rc::new(Rule::Dual(
                grammarize_repetition(sub),
                DualKind::Concatenation,
                Rc::new(Rule::Ref(Rc::downgrade(&sub)))
            ))
        )),
        Rule::Single(sub, kind) => Rc::new(Rule::Single(
            grammarize_repetition(sub),
            kind.clone()
        )),
        Rule::Dual(left, kind, right) => Rc::new(Rule::Dual(
            grammarize_repetition(left),
            kind.clone(),
            grammarize_repetition(right)
        ))
    }
}

pub fn tree_with_id_ref(name_def_pair: (&String, &Rc<Rule>), rule_to_transform: &Rc<Rule>) -> Rc<Rule> {
    match rule_to_transform.deref() {
        Rule::Atomic(s, AtomicKind::Identifier) => if s == name_def_pair.0 {
            Rc::new(Rule::Ref(Rc::downgrade(name_def_pair.1)))
        } else {
            Rc::clone(rule_to_transform)
        },
        Rule::Atomic(_, AtomicKind::Literal) | Rule::Ref(_) => Rc::clone(rule_to_transform),
        Rule::Single(sub, kind) => Rc::new(Rule::Single(
            tree_with_id_ref(name_def_pair, sub),
            kind.clone()
        )),
        Rule::Dual(left, kind, right) => Rc::new(Rule::Dual(
            tree_with_id_ref(name_def_pair, left),
            kind.clone(),
            tree_with_id_ref(name_def_pair, right)
        )),
    }
}

pub fn get_pure_tree(rule: Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Atomic(s, kind) => Rc::new(Rule::Atomic(s.to_string(), kind.clone())),
        Rule::Single(sub, kind) => Rc::new(
            Rule::Single(get_pure_tree(Rc::clone(sub)), kind.clone())
        ),
        Rule::Dual(left, kind, right) => Rc::new(
          Rule::Dual(
            get_pure_tree(Rc::clone(left)), kind.clone(), get_pure_tree(Rc::clone(right)))  
        ),
        Rule::Ref(r) => match r.upgrade() {
            Some(sub) => match sub.deref() {
                Rule::Atomic(s, kind) => Rc::new(Rule::Atomic(s.to_string(), kind.clone())),
                _ => Rc::clone(&rule)
            },
            None => Rc::new(Rule::Atomic(String::from("None reference"), AtomicKind::Literal)),
        },
    }
}

pub fn are_same_tree<'a>(rule_1: &'a Rule, rule_2: &'a Rule, leaves_compared: bool, deep: bool) -> bool {
    match (rule_1, rule_2) {
        (Rule::Atomic(_, _), Rule::Ref(_)) |
        (Rule::Ref(_), Rule::Atomic(_, _)) => !leaves_compared,
        (Rule::Atomic(s1, k1), Rule::Atomic(s2, k2)) => if leaves_compared { s1 == s2 && same_atomic_kind(k1, k2) } else { true },
        (Rule::Ref(r1), Rule::Ref(r2)) => if !deep { true } else {
            match (r1.upgrade(), r2.upgrade()) {
                (None, None) => true,
                (Some(sub_1), Some(sub_2)) => are_same_tree(sub_1.deref(), sub_2.deref(), leaves_compared, deep),
                _ => false,
            }
        },
        (Rule::Single(sub1, k1), Rule::Single(sub2, k2)) => are_same_tree(sub1, sub2, leaves_compared, deep) && same_single_kind(k1, k2),
        (Rule::Dual(left1, k1, right1), Rule::Dual(left2, k2, right2)) => are_same_tree(left1, left2, leaves_compared, deep)
            && are_same_tree(right1, right2, leaves_compared, deep) && same_dual_kind(k1, k2),
        _ => false,
    }
}

pub fn same_atomic_kind(k1: &AtomicKind, k2: &AtomicKind) -> bool {
    match (k1, k2) {
        (AtomicKind::Literal, AtomicKind::Literal) => true,
        (AtomicKind::Identifier, AtomicKind::Identifier) => true,
        _ => false,
    }
}

pub fn same_single_kind(k1: &SingleKind, k2: &SingleKind) -> bool {
    match (k1, k2) {
        (SingleKind::Repetition, SingleKind::Repetition) => true,
        (SingleKind::Grouping, SingleKind::Grouping) => true,
        (SingleKind::Optional, SingleKind::Optional) => true,
        _ => false,
    }
}

pub fn same_dual_kind(k1: &DualKind, k2: &DualKind) -> bool {
    match (k1, k2) {
        (DualKind::Alternation, DualKind::Alternation) => true,
        (DualKind::Concatenation, DualKind::Concatenation) => true,
        (DualKind::Exception, DualKind::Exception) => true,
        _ => false,
    }
}

pub fn create_definition_tree<'a>(rule: &'a Vec<Token>) -> Rc<Rule> {
    let rule_tree = create_rule_tree(rule);
    tree_without_grouping(Rc::new(rule_tree))
}

pub fn tree_without_grouping(rule: Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Atomic(_, _) | Rule::Ref(_) => Rc::clone(&rule),
        Rule::Single(sub, SingleKind::Grouping) => tree_without_grouping(Rc::clone(sub)),
        Rule::Single(sub, kind) => Rc::new(Rule::Single(
            tree_without_grouping(Rc::clone(sub)),
            kind.clone())
        ),
        Rule::Dual(left, kind, right) => Rc::new(Rule::Dual(
            tree_without_grouping(Rc::clone(left)),
            kind.clone(),
            tree_without_grouping(Rc::clone(right)))
        ),
    }
}

pub fn generate_rule_result<GA, GR, GS, GD>(rule: &Rc<Rule>, gen_from_atomic: &GA, gen_from_ref: &GR, gen_from_single: &GS, gen_from_dual: &GD) -> Rc<Rule>
where
    GA: Fn(&Rule) -> Rc<Rule>,
    GR: Fn(&Weak<Rule>) -> Rc<Rule>,
    GS: Fn(&Rule, &Rc<Rule>) -> Rc<Rule>,
    GD: Fn(&Rule, &Rc<Rule>, &Rc<Rule>) -> Rc<Rule> {
        match rule.deref() {
            Rule::Atomic(_, _) => gen_from_atomic(rule),
            Rule::Single(sub, _) => gen_from_single(rule, sub),
            Rule::Dual(left, _, right) => gen_from_dual(rule, left, right),
            Rule::Ref(r) => gen_from_ref(r)
        }
}

pub fn counting_single_result<T, I, Cnt: Counter<T, I> + Clone, PA, PR, VS, VD>(rule: &Rc<Rule>, count_atomic: &PA, count_ref: &PR, op_on_single_counter: &VS, op_on_two_counters: &VD) -> Cnt
where
    PA: Fn(&Rule) -> Cnt,
    PR: Fn(&Weak<Rule>) -> Cnt,
    VS: Fn(&Rule, Cnt) -> Cnt,
    VD: Fn(&Rule, Cnt, Cnt) -> Cnt {
        match rule.deref() {
            Rule::Atomic(_, _) => count_atomic(rule.deref()),
            Rule::Single(sub, _) => op_on_single_counter(rule, counting_single_result(sub, count_atomic, count_ref, op_on_single_counter, op_on_two_counters)),
            Rule::Dual(left, _, right) => op_on_two_counters(
                rule,
                counting_single_result(left, count_atomic, count_ref, op_on_single_counter, op_on_two_counters),
                counting_single_result(right, count_atomic, count_ref, op_on_single_counter, op_on_two_counters)
            ),
            Rule::Ref(r) => op_on_single_counter(rule, count_ref(r))
        }
    }

pub fn predicate_single_result<PA, PR, VS, VD>(rule: &Rc<Rule>, pred_atomic: &PA, pred_ref: &PR, op_on_single_truthness: &VS, op_on_dual_truthness: &VD) -> bool
    where
        PA: Fn(&Rule) -> bool,
        PR: Fn(&Weak<Rule>) -> bool,
        VS: Fn(&Rule, bool) -> bool,
        VD: Fn(&Rule, bool, bool) -> bool {
    match rule.deref() {
        Rule::Atomic(_, _) => pred_atomic(rule.deref()),
        Rule::Single(sub, _) => op_on_single_truthness(rule, predicate_single_result(sub, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness)),
        Rule::Dual(left, _, right) => op_on_dual_truthness(
            rule,
            predicate_single_result(left, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness),
            predicate_single_result(right, pred_atomic, pred_ref, op_on_single_truthness, op_on_dual_truthness)
        ),
        Rule::Ref(r) => op_on_single_truthness(rule, pred_ref(r))
    }
}

pub fn create_rule_tree<'a>(rule: &'a Vec<Token>) -> Rule {
    let rule_as_ref: Vec<&Token> = tokens_as_ref(rule);
    let rule_with_prior_brackets = with_priority_parentheses(rule_as_ref);
    let res = create_rule_tree_by_ref(rule_with_prior_brackets);
    res
}

pub fn create_rule_tree_by_ref(rule: Vec<&Token>) -> Rule {
    if rule.len() == 1 {
        match rule.first().unwrap() {
            Token::Rl(rl) => match rl.deref() {
                Rule::Atomic(s, kind) => return Rule::Atomic(s.to_string(), kind.clone()),
                _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
            },
            _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
        };
    }
    if rule.len() == 3 {
        match (rule.first().unwrap(), rule.get(1).unwrap(), rule.last().unwrap()) {
            (Token::Op(op1), Token::Rl(rl), Token::Op(_)) => match op1 {
                Operator::OptionalL => return Rule::Single(Rc::new(Rule::Ref(Rc::downgrade(rl))), SingleKind::Optional),
                Operator::RepetitionL => return Rule::Single(Rc::new(Rule::Ref(Rc::downgrade(rl))), SingleKind::Repetition),
                Operator::GroupingL => return Rule::Single(Rc::new(Rule::Ref(Rc::downgrade(rl))), SingleKind::Grouping),
                _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
            },
            (Token::Rl(rl1), Token::Op(op), Token::Rl(rl2)) => match op {
                Operator::Alternation => return Rule::Dual(Rc::new(Rule::Ref(Rc::downgrade(rl1))), DualKind::Alternation, Rc::new(Rule::Ref(Rc::downgrade(rl2)))),
                Operator::Concatenation => return Rule::Dual(Rc::new(Rule::Ref(Rc::downgrade(rl1))), DualKind::Concatenation, Rc::new(Rule::Ref(Rc::downgrade(rl2)))),
                Operator::Exception => return Rule::Dual(Rc::new(Rule::Ref(Rc::downgrade(rl1))), DualKind::Exception, Rc::new(Rule::Ref(Rc::downgrade(rl2)))),
                _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
            },
            
            _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
        };
    }
    if least_prior_is_unary(&rule) {
        match rule.first().unwrap() {
            Token::Op(Operator::RepetitionL) => return Rule::Single(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule))), SingleKind::Repetition),
            Token::Op(Operator::OptionalL) => return Rule::Single(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule))), SingleKind::Optional),
            Token::Op(Operator::GroupingL) => return Rule::Single(Rc::new(create_rule_tree_by_ref(get_rule_without_first_last(rule))), SingleKind::Grouping),
            _ => Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
        };
    }
    let i = get_least_prior_binary_index(&rule);
    match i {
        Some(idx) => {
            let el = *rule.get(idx).unwrap();
            let (left_part, right_part) = split_rule_by_index(&rule, idx);
            match el {
                Token::Op(Operator::Alternation) => return Rule::Dual(Rc::new(create_rule_tree_by_ref(left_part)), DualKind::Alternation, Rc::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Concatenation) => return Rule::Dual(Rc::new(create_rule_tree_by_ref(left_part)), DualKind::Concatenation, Rc::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Exception) => return Rule::Dual(Rc::new(create_rule_tree_by_ref(left_part)), DualKind::Exception, Rc::new(create_rule_tree_by_ref(right_part))),
                _ => return Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
            }
        }
        None => Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier),
    };
    Rule::Atomic("Invalid".to_string(), AtomicKind::Identifier)
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