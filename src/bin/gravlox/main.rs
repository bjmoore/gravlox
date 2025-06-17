use clap::Parser;
use compiler::compile;
use std::collections::VecDeque;
use std::{
    error::Error,
    fs,
    io::{self, Write},
};
use value::Value;
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
#[command(version, about)]
struct Args {
    filename: Option<String>,

    #[arg(short, long)]
    debug: bool,

    #[arg(long)]
    disassemble: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    match args.filename {
        Some(filename) => {
            if args.disassemble {
                disassemble(&filename)
            } else {
                run_script(&filename, args.debug)
            }
        }
        None => repl(args.debug),
    }?;

    Ok(())
}

fn run_script(filename: &str, debug: bool) -> Result<(), Box<dyn Error>> {
    let mut vm = GravloxVM::new();
    let source = String::from_utf8(fs::read(filename)?)?;
    let script = compile(source, debug);

    if let Some(script) = script {
        vm.interpret(script);
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
            vm.interpret(script);
        }
    }
}

fn disassemble(filename: &str) -> Result<(), Box<dyn Error>> {
    let source = String::from_utf8(fs::read(filename)?)?;
    let script = compile(source, false);
    let mut functions = VecDeque::new();

    if let Some(script) = script {
        functions.push_back(script);
    }

    while let Some(function) = functions.pop_front() {
        let function = function.borrow();
        let chunk = function.chunk.borrow();
        let name = function.name.as_ref().map_or("<root>", |s| s.as_str());

        println!("== {} ==", name);

        if chunk.constants.len() > 0 {
            println!("constants:");
            for (idx, constant) in chunk.constants.iter().enumerate() {
                if let Value::FunctionRef(f) = constant {
                    functions.push_back(f.clone());
                }
                println!("{idx}: {}", constant);
            }
            println!();
        }

        println!("bytes: {:?}", chunk.code);
        println!();

        println!("code:");
        println!("{}", chunk);
    }

    Ok(())
}
