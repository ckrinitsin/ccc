use anyhow::Result;

mod cli;
mod preprocess;

fn main() -> Result<()> {
    cli::cli()
}
