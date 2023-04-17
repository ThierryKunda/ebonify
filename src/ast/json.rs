use std::collections::BTreeMap;
use std::rc::Rc;

use serde_json::Value;

use crate::{ebnf_syntax::*, utils::*};

/// Creates a map with name-definition association from JSON
pub fn btree_rule_from_json(rules_json: Value) -> Option<BTreeMap<String, Rc<Rule>>> {
    if let Value::Object(name_def_pairs) = rules_json {
        let mut res: BTreeMap<String, Rc<Rule>> = BTreeMap::new();
        for (name, def) in name_def_pairs {
            res.insert(name, rule_from_json(&def)?);
        }
        Some(res)
    } else {
        None
    }
}

/// Creates an association counter from JSON
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

/// Creates a rule from JSON
pub fn rule_from_json(rule_json: &Value) -> Option<Rc<Rule>> {
    match rule_json {
        Value::Object(obj) => match obj.get("node")? {
            Value::String(s) => match s.as_str() {
                "literal" => if let Value::String(s) = obj.get("value")? {
                    Some(Rc::new(Rule::Atomic(
                        s.to_string(),
                        AtomicKind::Literal
                    )))
                } else {
                    None
                },
                "identifier" => if let Value::String(s) = obj.get("value")? {
                    Some(Rc::new(Rule::Atomic(
                        s.to_string(),
                        AtomicKind::Identifier
                    )))
                } else {
                    None
                },
                "alternation" => Some(Rc::new(Rule::Dual(
                    rule_from_json(obj.get("left")?)?,
                    DualKind::Alternation,
                    rule_from_json(obj.get("right")?)?
                ))),
                "concatenation" => Some(Rc::new(Rule::Dual(
                    rule_from_json(obj.get("left")?)?,
                    DualKind::Concatenation,
                    rule_from_json(obj.get("right")?)?
                ))),
                "exception" => Some(Rc::new(Rule::Dual(
                    rule_from_json(obj.get("left")?)?,
                    DualKind::Exception,
                    rule_from_json(obj.get("right")?)?
                ))),
                "repetition" => Some(Rc::new(Rule::Single(
                    rule_from_json(obj.get("value")?)?,
                    SingleKind::Repetition
                ))),
                "grouping" => Some(Rc::new(Rule::Single(
                    rule_from_json(obj.get("value")?)?,
                    SingleKind::Grouping
                ))),
                "optional" => Some(Rc::new(Rule::Single(
                    rule_from_json(obj.get("value")?)?,
                    SingleKind::Optional
                ))),
                _ => None
            }
            _ => None
        }
        _ => None
    }
}
