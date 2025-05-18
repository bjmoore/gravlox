use clap::Parser;
use compiler::compile;
use std::{
    error::Error,
    fs,
    io::{self, Write},
};
use vm::GravloxVM;

mod chunk;
mod compiler;
mod error;
mod lexer;
mod obj;
mod op;
mod take;
mod token;
mod value;
mod vm;

#[derive(Parser, Debug)]
struct Args {
    filename: Option<String>,

    #[arg(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    match args.filename {
        Some(filename) => run_script(&filename, args.debug),
        None => repl(args.debug),
    }?;

    Ok(())
}

fn run_script(filename: &str, debug: bool) -> Result<(), Box<dyn Error>> {
    let mut vm = GravloxVM::new();
    let source = String::from_utf8(fs::read(filename)?)?;
    let script = compile(source, debug);

    if let Some(script) = script {
        vm.interpret(script)?;
    }

    Ok(())
}

fn repl(debug: bool) -> Result<(), Box<dyn Error>> {
    let mut vm = GravloxVM::new();
    loop {
        let mut buf = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut buf)?;

        let script = compile(buf.clone(), debug);
        if let Some(script) = script {
            vm.interpret(script)?;
        }
    }
}
