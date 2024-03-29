use clap::Parser;
use ebonify::cli::{self, SourceType};
use ebonify::cli::Commands;
use ebonify::data_formatting::*;

fn main() {
    let cli = crate::cli::Cli::parse();
    match &cli.command {
        Some(Commands::Convert(argmts)) => {
            convert(&argmts.from_type, &argmts.dest_type, &argmts.value, &argmts.pretty);
        },
        Some(Commands::Match(_)) => println!("Trying to check if a token is generated by a formal language"),
        None => println!("No command selected"),
    }
}

pub fn convert(from_type: &SourceType, dest_type: &SourceType, data: &String, pretty_printing: &bool) {
    match (from_type, dest_type) {
        (cli::SourceType::File, cli::SourceType::JSON) => {
            let data = FileData::new(data);
            match data.get_json() {
                Ok(v) => if pretty_printing == &true {
                    print!("{:#}", v)
                } else {
                    print!("{}", v)
                },
                Err(e) => println!("{}", e.to_string()),
            }
        },
        _ => println!("Other methods not implemented")
    }
}