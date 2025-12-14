use std::path::PathBuf;
use anyhow::Result;
use clap::Parser;
use std::fs;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input c program
    file: PathBuf,
}

pub fn cli() -> Result<()> {
    let args = Cli::parse();

    let content = fs::read_to_string(args.file)?;

    Ok(())
}
