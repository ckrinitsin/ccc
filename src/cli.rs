use std::path::PathBuf;
use anyhow::Result;
use clap::Parser;
use std::fs;
use crate::lex;
use crate::preprocess;
use crate::parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input c program
    file: PathBuf,

    /// Run lexer and stop afterwards
    #[arg(short, long)]
    lex: bool,

    /// Run lexer and parser, stop afterwards
    #[arg(short, long)]
    parse: bool,

    /// Run lexer, parser and codegen, stop afterwards
    #[arg(short, long)]
    codegen: bool,
}

pub fn cli() -> Result<()> {
    let args = Cli::parse();

    let content = fs::read_to_string(args.file)?;

    let content = preprocess::preprocess(content)?;

    let tokens = lex::lex(content)?;

    if args.lex {
        return Ok(());
    }

    let ast = parser::parse_tokens(tokens)?;

    if args.parse {
        return Ok(());
    }

    // TODO: Codegen 

    if args.codegen {
        return Ok(());
    }

    // TODO: Code emission

    Ok(())
}
