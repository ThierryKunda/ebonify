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
            "node": "alternation",
            "value": {
                "left": {
                    "node": "literal",
                    "value": "hello"
                },
                "right": {
                    "node": "identifier",
                    "value": "world"
                }
            }
        });
        let res = crate::ast::rule_from_json(repr_json);
        println!("{:?}", res);
    } else {
        println!("Error for the example");
    }
}