// use std::path::PathBuf;

use std::fmt::Display;

use clap::{Parser, Subcommand, Args, ValueEnum};

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum SourceType {
    JSON,
    File,
    HTTP,
}

impl Display for SourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            SourceType::JSON => write!(f, "json"),
            SourceType::File => write!(f, "file"),
            SourceType::HTTP => write!(f, "http"),
        }
    }
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
    #[arg(short, long)]
    #[arg(default_value_t = SourceType::JSON)]
    from_type: SourceType,
    #[arg(short, long)]
    dest_type: SourceType,
    #[arg(short, long)]
    value: String
}

#[derive(Debug, Args)]
pub struct MatchArgs {
    #[arg(short, long)]
    syntax_source_type: Option<SourceType>,
    #[arg(short, long)]
    value: String
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    // Converts some content and return back the result
    Convert(ConvertArgs),
    // Check if the syntax corresponds to the input content
    Match(MatchArgs)
}