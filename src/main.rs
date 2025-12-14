use anyhow::Result;

mod cli;
mod lex;
mod preprocess;

fn main() -> Result<()> {
    cli::cli()
}
