mod ebnf_syntax;
mod error;
mod pre_treatment;
mod ast;
mod structure;
mod utils;

use structure::EbnfTree;

fn main() {
    let example = EbnfTree::from_file("tests_samples/lang_mini.ebnf");
    if let Ok(mut tree) = example {
        tree.update_all_definition_nodes_count(true);
        tree.update_identified_counts();
        for (name, definition) in tree.rules {
            println!("{} = {:?}\n", name, definition);
        }
        println!("{:?}", tree.nodes_count_per_definition);
    } else {
        println!("Error for the example");
    }

}