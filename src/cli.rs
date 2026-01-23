use crate::backend::codegen;
use crate::frontend::ir;
use crate::frontend::lex;
use crate::frontend::parse;
use crate::frontend::semantic_analysis::identifier_resolution;
use crate::frontend::semantic_analysis::label_resolution;
use crate::frontend::semantic_analysis::loop_resolution;
use crate::frontend::semantic_analysis::type_check;
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
    /// Input c programs
    #[arg(required = true)]
    files: Vec<PathBuf>,

    /// Run lexer and stop afterwards
    #[arg(long)]
    lex: bool,

    /// Run lexer and parser, stop afterwards
    #[arg(long)]
    parse: bool,

    /// Run lexer, parser and semantic analyzer, stop afterwards
    #[arg(long)]
    validate: bool,

    /// Run lexer, parser, semantic analyzer and ir, stop afterwards
    #[arg(long)]
    tacky: bool,

    /// Run lexer, parser, semantic analyzer, ir and codegen, stop afterwards
    #[arg(long)]
    codegen: bool,

    /// Compiles and assemble the file, but do not link
    #[arg(short, long)]
    compile: bool,

    /// Output file
    #[arg(short, long)]
    output: Option<PathBuf>,
}

pub fn cli() -> Result<()> {
    let args = Cli::parse();

    let mut binary_file;
    if let Some(output_file) = args.output {
        binary_file = output_file;
    } else {
        binary_file = args.files[0].clone();
        _ = binary_file.set_extension("");
    }

    let mut asm_files = Vec::new();
    for file in args.files {
        let mut asm_file = file.clone();
        _ = asm_file.set_extension("S");
        let mut preprocessed_file = file.clone();
        _ = preprocessed_file.set_extension("i");

        let output = Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(&file)
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
            println!("Tokens for {:?}:", file);
            for tok in tokens {
                print!("{} ", tok);
            }
            continue;
        }

        let ast = parse::parse_tokens(tokens)?;

        if args.parse {
            println!("Ast for {:?}:", file);
            println!("{:?}", ast);
            continue;
        }

        let analyzed_ast = identifier_resolution::variable_resolution(ast)?;
        let analyzed_ast = label_resolution::label_resolution(analyzed_ast)?;
        let analyzed_ast = loop_resolution::loop_resolution(analyzed_ast)?;
        let (analyzed_ast, mut symbol_table) = type_check::type_check(analyzed_ast)?;

        if args.validate {
            println!("Analyzed Ast for {:?}:", file);
            println!("{:?}", analyzed_ast);
            continue;
        }

        let ir = ir::lift::lift_to_ir(analyzed_ast, &mut symbol_table)?;

        if args.tacky {
            println!("IR for {:?}:", file);
            println!("{}", ir);
            continue;
        }

        let asm = codegen::gen_asm(ir, &symbol_table)?;

        if args.codegen {
            println!("Codegen for {:?}:", file);
            println!("{}", asm);
            continue;
        }

        let mut file = File::create(&asm_file)?;
        write!(file, "{}", asm)?;

        asm_files.push(asm_file);
    }

    if args.lex || args.parse || args.validate || args.tacky || args.codegen {
        return Ok(());
    }

    let output;
    if args.compile {
        _ = binary_file.set_extension("o");
        output = Command::new("gcc")
            .arg("-c")
            .args(&asm_files)
            .arg("-o")
            .arg(binary_file)
            .output()?;
    } else {
        output = Command::new("gcc")
            .args(&asm_files)
            .arg("-o")
            .arg(binary_file)
            .output()?;
    }

    for asm_file in asm_files {
        fs::remove_file(asm_file)?;
    }

    if !output.status.success() {
        io::stdout().write_all(&output.stdout)?;
        io::stderr().write_all(&output.stderr)?;
        bail!("Could not assemble and link file");
    }

    Ok(())
}
