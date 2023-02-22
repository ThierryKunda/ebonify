use std::collections::BTreeMap;
use std::rc::{Rc, Weak};
use std::ops::Deref;

use serde_json::Value;

use crate::utils::AssocRuleCounter;
use crate::{utils::Counter, ebnf_syntax::*, pre_treatment::{brackets_paired, tokenize_rule_from_str, split_members, is_binary, valid_single_operators}};

pub fn btree_rule_from_json(rules_json: Value) -> Option<BTreeMap<String, Rc<Rule>>> {
    if let Value::Object(name_def_pairs) = rules_json {
        let mut res: BTreeMap<String, Rc<Rule>> = BTreeMap::new();
        for (name, def) in name_def_pairs {
            res.insert(name, rule_from_json(def));
        }
        Some(res)
    } else {
        None
    }
}

pub fn assoc_counter_from_json(counter_json: &Value) -> Option<AssocRuleCounter> {
    if let Value::Object(m) = counter_json {
        let mut res = AssocRuleCounter::new();
        for (k,v) in m {
            if let Value::Number(num) = v {
                if let Some(qty) = num.as_u64() {
                    res.add_any_to_element(Some(k), qty as usize);
                }
            }
        }
        return Some(res);
    }
    return None;
}

pub fn rule_from_json(rule_json: Value) -> Rc<Rule> {
    if let Value::Object(obj) = rule_json {
        if let (Some(Value::String(single)), Some(value)) = (obj.get(&String::from("node")), obj.get(&String::from("value"))) {
            match (single.as_str(), value) {
                ("literal", Value::String(lit)) => return Rc::new(Rule::Literal(lit.to_string())),
                ("identifier", Value::String(id)) => return Rc::new(Rule::Identifier(id.to_string())),
                (_, Value::Object(m))
                => if let (Some(left), Some(right)) = (
                    m.get(&String::from("left")),
                    m.get(&String::from("right"))
                ) {
                    match single.as_str() {
                        "alternation" => return Rc::new(Rule::Alternation(
                            rule_from_json(left.clone()),
                            rule_from_json(right.clone())
                        )),
                        "concatenation" => return Rc::new(Rule::Concatenation(
                            rule_from_json(left.clone()),
                            rule_from_json(right.clone())
                        )),
                        "exception" => return Rc::new(Rule::Exception(
                            rule_from_json(left.clone()),
                            rule_from_json(right.clone())
                        )),
                        _ => (),
                    }
                },
                _ => ()
            }
        } else if let (Some(Value::String(dual)), Some(v)) = (obj.get(&String::from("node")), obj.get(&String::from("v"))) {
            match dual.as_str() {
                "repetition" => return Rc::new(Rule::Repetition(rule_from_json(v.clone()))),
                "grouping" => return Rc::new(Rule::Repetition(rule_from_json(v.clone()))),
                "optional" => return Rc::new(Rule::Repetition(rule_from_json(v.clone()))),
                _ => (),
            }
        }
    }
    return Rc::new(Rule::Identifier(String::from("")));
}

pub fn grammarize_repetition(rule: &Rc<Rule>) -> Rc<Rule> {
    match rule.deref() {
        Rule::Literal(lit) => Rc::new(Rule::Literal(lit.clone())),
        Rule::Identifier(id) => Rc::new(Rule::Identifier(id.clone())),
        Rule::Ref(r) => if let Some(sub) = r.upgrade() {
            Rc::new(Rule::Ref(Rc::downgrade(&sub)))
        } else {
            Rc::new(Rule::Identifier(String::from("No reference")))
        },
        Rule::Alternation(left, right) => Rc::new(
            Rule::Alternation(grammarize_repetition(left), grammarize_repetition(right))
        ),
        Rule::Concatenation(left, right) => Rc::new(
            Rule::Concatenation(grammarize_repetition(left), grammarize_repetition(right))
        ),
        Rule::Exception(left, right) => Rc::new(
            Rule::Exception(grammarize_repetition(left), grammarize_repetition(right))
        ),
        Rule::Grouping(sub) => Rc::new(Rule::Grouping(grammarize_repetition(sub))),
        Rule::Optional(sub) => Rc::new(Rule::Optional(grammarize_repetition(sub))),
        Rule::Repetition(sub) => Rc::new(
            Rule::Alternation(
                grammarize_repetition(sub),
                Rc::new(
                    Rule::Concatenation(
                        grammarize_repetition(sub), 
                        Rc::new(
                            Rule::Ref(Rc::downgrade(&grammarize_repetition(sub)))
                        )
                    )
                ) 
            )
        ),
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
            get_pure_tree(Rc::clone(left)), kind.clone(), get_pure_tree(Rc::clone(left)))  
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
        (Rule::Atomic(s1, k1), Rule::Atomic(s2, k2)) => if leaves_compared { s1 == s2 && same_atomic_kind(k1, k2) } else { true },
        (Rule::Single(sub1, k1), Rule::Single(sub2, k2)) => are_same_tree(sub1, sub2, leaves_compared, deep),
        (Rule::Dual(left1, k1, right1), Rule::Dual(left2, k2, right2)) => are_same_tree(left1, left1, leaves_compared, deep)
            && are_same_tree(right1, right2, leaves_compared, deep),
        (Rule::Ref(r1), Rule::Ref(r2)) => if !deep { true } else {
            match (r1.upgrade(), r2.upgrade()) {
                (None, None) => true,
                (Some(sub_1), Some(sub_2)) => are_same_tree(sub_1.deref(), sub_2.deref(), leaves_compared, deep),
                _ => false,
            }
        },
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
        Rule::Single(sub, kind) => Rc::new(Rule::Single(Rc::clone(sub), kind.clone())),
        Rule::Dual(left, kind, right) => Rc::new(Rule::Dual(
            Rc::clone(left),
            kind.clone(),
            Rc::clone(right))
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
    let rule_as_ref = tokens_as_ref(rule);
    let rule_with_prior_brackets = with_priority_parentheses(rule_as_ref);
    let res = create_rule_tree_by_ref(rule_with_prior_brackets);
    res
}

pub fn create_rule_tree_by_ref(rule: Vec<&Token>) -> Rule {
    if rule.len() == 1 {
        match rule.first().unwrap() {
            Token::Rl(rl) => match rl.deref() {
                Rule::Atomic(lit, AtomicKind::Literal) => return Rule::Atomic(lit.to_string(), AtomicKind::Literal),
                Rule::Atomic(id, AtomicKind::Identifier) => return Rule::Atomic(id.to_string(), AtomicKind::Identifier),
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