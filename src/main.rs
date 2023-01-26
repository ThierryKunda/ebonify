mod ebnf_syntax;
mod error;
mod pre_treatment;
mod ast;
mod structure;
mod utils;

use serde_json::json;
use structure::EbnfTree;

fn main() {
    let example = EbnfTree::from_file("tests_samples/lang_mini.ebnf");
    if let Ok(mut tree) = example {
        tree.update_all_definition_nodes_count(true);
        tree.update_identified_counts();
        // for (name, definition) in tree.rules.iter() {
        //     println!("{} = {:?}\n", name, definition);
        // }
        // let schema = tree.create_json_schema();
        // println!("{:#}", schema);
        let repr_json = json!({
            "syntax_source_name": "my_syntax",
            "nodes_count_per_definition": {
                "my_rule": 2,
                "his_rule": 3,
            },
            "rules": {
                "my_rule": {
                    "node": "alternation",
                    "value": {
                        "left": {
                            "node": "literal",
                            "value": "hello"
                        },
                        "right": {
                            "node": "literal",
                            "value": "world"
                        }
                    }
                },
                "his_rule": {
                    "node": "alternation",
                    "value": {
                        "left": {
                            "node": "identifier",
                            "value": "my_rule"
                        },
                        "right": {
                            "node": "literal",
                            "value": "goodbye"
                        }
                    }
                }
            },
            "identified_counts": {
                "my_rule": 1,
                "his_rule": 1,
            }
        });
        let res = crate::structure::EbnfTree::from_json(repr_json);
        println!("{:?}\n", res);
    } else {
        println!("Error for the example");
    }
}