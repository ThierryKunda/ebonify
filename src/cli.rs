// use std::path::PathBuf;

use clap::{Parser, Subcommand, Args, ValueEnum};

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum SourceType {
    JSON,
    File,
    HTTP,
}

 #[derive(Parser, Debug)]
 #[command(author = "Thierry K. <kundathierry@gmail.com>", version = "1.0", about, long_about = None)]
pub struct Cli {
    pub name: Option<String>,

    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Debug, Args)]
pub struct ConvertArgs {
    source_type: Option<SourceType>,
    value: String
}

#[derive(Debug, Args)]
pub struct MatchArgs {
    syntax_source_type: Option<SourceType>,
    value: String
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    // Converts some content and return back the result
    Convert(ConvertArgs),
    // Check if the syntax corresponds to the input content
    Match(MatchArgs)
}