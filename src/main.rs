mod ebnf_syntax;
mod error;
mod pre_treatment;
mod ast;
mod structure;

use structure::EbnfTree;

fn main() {
    let example = EbnfTree::from_file("tests_samples/lang_mini.ebnf");
    if let Ok(tree) = example {
        for (name, definition) in tree.rules {
            println!("{} = {:?}\n", name, definition);
        }
    } else {
        println!("Error for the example");
    }
}