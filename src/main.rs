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
        tree.update_identified_counts();
        for (name, definition) in tree.rules {
            println!("{} = {:?}\n", name, definition);
        }
        println!("{:?}", tree.identified_counts);
    } else {
        println!("Error for the example");
    }

}