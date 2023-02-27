use std::ops::Deref;
use std::rc::{Rc, Weak};

use crate::pre_treatment::{tokenize_rule_from_str, split_members, tokens_as_ref};
use crate::utils::{AssocRuleCounter, Counter};

use crate::ebnf_syntax::*;

use super::tokens::*;

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