#![crate_name = "ebonify"]
mod error;
pub mod parsing;
pub mod ebnf_syntax;
pub mod ast;
mod structure;
#[allow(unused)]
mod utils;
mod cli;