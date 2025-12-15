use crate::codegen;
use crate::lex;
use crate::parser;
use anyhow::Result;
use clap::Parser;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

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

    let mut asm_file = args.file.clone();
    _ = asm_file.set_extension("S");
    let mut preprocessed_file = args.file.clone();
    _ = preprocessed_file.set_extension("i");
    let mut binary_file = args.file.clone();
    _ = binary_file.set_extension("");

    let _ = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(args.file)
        .arg("-o")
        .arg(&preprocessed_file)
        .output()?;

    let content = fs::read_to_string(&preprocessed_file)?;
    fs::remove_file(preprocessed_file)?;

    let tokens = lex::lex(content)?;

    if args.lex {
        return Ok(());
    }

    let ast = parser::parse_tokens(tokens)?;

    if args.parse {
        return Ok(());
    }

    let asm = codegen::parse_ast(ast)?;

    if args.codegen {
        return Ok(());
    }

    let mut file = File::create(&asm_file)?;
    write!(file, "{}", asm)?;

    let _ = Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg(binary_file)
        .output()?;

    fs::remove_file(asm_file)?;

    Ok(())
}
