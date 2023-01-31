use std::path::PathBuf;

use clap::{Parser, Command, Subcommand};

 #[derive(Parser, Debug)]
 #[command(author = "Thierry K. <kundathierry@gmail.com>", version = "1.0", about, long_about = None)]
pub struct Cli {
    pub name: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, Subcommand)]
enum Commands {
    // Converts some content and return back the result
    Convert,
    // Check if the syntax corresponds to the input content
    Match,
}