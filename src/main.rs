use chunk::Chunk;
use clap::Parser;
use op::{OP_ADD, OP_NEGATE, OP_RETURN, OP_SUBTRACT};
use std::{
    error::Error,
    fs,
    io::{self, Write},
};
use vm::GravloxVM;

mod chunk;
mod op;
mod value;
mod vm;

#[derive(Parser, Debug)]
struct Args {
    filename: Option<String>,

    #[arg(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut vm = GravloxVM::new();

    let args = Args::parse();

    match args.filename {
        Some(filename) => run_script(&filename),
        None => repl(),
    }?;

    Ok(())
}

fn run_script(filename: &str) -> Result<(), Box<dyn Error>> {
    let script = String::from_utf8(fs::read(filename)?)?;
    Ok(run(&script))
}

fn repl() -> Result<(), Box<dyn Error>> {
    loop {
        let mut buf = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut buf)?;
        run(&buf);
    }
}

fn run(source: &str) {}
