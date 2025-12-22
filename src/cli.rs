use crate::backend::codegen;
use crate::frontend::ir;
use crate::frontend::lex;
use crate::frontend::parser;
use crate::frontend::semantic;
use anyhow::Result;
use anyhow::bail;
use clap::Parser;
use std::fs;
use std::fs::File;
use std::io;
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

    /// Run lexer, parser and semantic analyzer, stop afterwards
    #[arg(short, long)]
    validate: bool,

    /// Run lexer, parser, semantic analyzer and ir, stop afterwards
    #[arg(short, long)]
    tacky: bool,

    /// Run lexer, parser, semantic analyzer, ir and codegen, stop afterwards
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

    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(args.file)
        .arg("-o")
        .arg(&preprocessed_file)
        .output()?;

    if !output.status.success() {
        io::stdout().write_all(&output.stdout)?;
        io::stderr().write_all(&output.stderr)?;
        bail!("Could not preprocess file");
    }

    let content = fs::read_to_string(&preprocessed_file)?;
    fs::remove_file(preprocessed_file)?;

    let tokens = lex::lex(content)?;

    if args.lex {
        println!("Tokens:");
        for tok in tokens {
            print!("{} ", tok);
        }
        return Ok(());
    }

    let ast = parser::parse_tokens(tokens)?;

    if args.parse {
        println!("Ast:");
        println!("{:?}", ast);
        return Ok(());
    }

    let analyzed_ast = semantic::resolve_ast(ast)?;

    if args.validate {
        println!("Analyzed Ast:");
        println!("{:?}", analyzed_ast);
        return Ok(());
    }

    let ir = ir::lift_to_ir(analyzed_ast)?;

    if args.tacky {
        println!("{}", ir);
        return Ok(());
    }

    let asm = codegen::gen_asm(ir)?;

    if args.codegen {
        println!("{}", asm);
        return Ok(());
    }

    let mut file = File::create(&asm_file)?;
    write!(file, "{}", asm)?;

    let output = Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg(binary_file)
        .output()?;

    fs::remove_file(asm_file)?;

    if !output.status.success() {
        io::stdout().write_all(&output.stdout)?;
        io::stderr().write_all(&output.stderr)?;
        bail!("Could not assemble and link file");
    }

    Ok(())
}
