// mod ebnf_syntax;
// mod error;
// mod parsing;
// mod ast;
// mod structure;
// mod utils;
mod cli;
use clap::Parser;
// use structure::EbnfTree;

fn main() {
    let cli = crate::cli::Cli::parse();
    if let Some(v) = cli.name {
        println!("{}", v);
    } else {
        println!("No input");
    }
}