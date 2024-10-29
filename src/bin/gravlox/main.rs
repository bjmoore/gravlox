use chunk::Chunk;
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
mod op;
mod token;
mod value;
mod vm;

const MAIN: &'static str = "main";

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
    let script = String::from_utf8(fs::read(filename)?)?;
    let mut chunk = Chunk::new(MAIN);
    let compile_ok = compile(script, &mut chunk, debug);

    if compile_ok {
        vm.interpret(&chunk)?;
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

        let mut chunk = Chunk::new(MAIN);
        let compile_ok = compile(buf.clone(), &mut chunk, debug);
        if compile_ok {
            vm.interpret(&chunk)?;
        }
    }
}
