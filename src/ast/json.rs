use std::collections::BTreeMap;
use std::rc::Rc;

use serde_json::Value;

use crate::{ebnf_syntax::*, utils::*};

/// Creates a map with name-definition association from JSON
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
pub fn rule_from_json(rule_json: Value) -> Rc<Rule> {
    if let Value::Object(obj) = rule_json {
        if let (Some(Value::String(single)), Some(value)) = (obj.get(&String::from("node")), obj.get(&String::from("value"))) {
            match (single.as_str(), value) {
                ("literal", Value::String(lit)) => return Rc::new(Rule::Atomic(lit.to_string(), AtomicKind::Identifier)),
                ("identifier", Value::String(id)) => return Rc::new(Rule::Atomic(id.to_string(), AtomicKind::Identifier)),
                (_, Value::Object(m))
                => if let (Some(left), Some(right)) = (
                    m.get(&String::from("left")),
                    m.get(&String::from("right"))
                ) {
                    match single.as_str() {
                        "alternation" => return Rc::new(Rule::Dual(
                            rule_from_json(left.clone()),
                            DualKind::Alternation,
                            rule_from_json(right.clone())
                        )),
                        "concatenation" => return Rc::new(Rule::Dual(
                            rule_from_json(left.clone()),
                            DualKind::Concatenation,
                            rule_from_json(right.clone())
                        )),
                        "exception" => return Rc::new(Rule::Dual(
                            rule_from_json(left.clone()),
                            DualKind::Exception,
                            rule_from_json(right.clone())
                        )),
                        _ => (),
                    }
                },
                _ => ()
            }
        } else if let (Some(Value::String(dual)), Some(v)) = (obj.get(&String::from("node")), obj.get(&String::from("v"))) {
            match dual.as_str() {
                "repetition" => return Rc::new(Rule::Single(rule_from_json(v.clone()), SingleKind::Repetition)),
                "grouping" => return Rc::new(Rule::Single(rule_from_json(v.clone()), SingleKind::Repetition)),
                "optional" => return Rc::new(Rule::Single(rule_from_json(v.clone()), SingleKind::Optional)),
                _ => (),
            }
        }
    }
    return Rc::new(Rule::Atomic(String::from(""), AtomicKind::Identifier));
}
