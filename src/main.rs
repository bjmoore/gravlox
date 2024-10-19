use chunk::Chunk;
use op::{OP_CONSTANT, OP_NEGATE, OP_RETURN};
use std::error::Error;
use vm::GravloxVM;

mod chunk;
mod op;
mod value;
mod vm;

fn main() -> Result<(), Box<dyn Error>> {
    let mut vm = GravloxVM::new();
    let mut chunk = Chunk::new("demo");

    chunk.add_constant(3.14159, 1)?;
    chunk.add_code(OP_NEGATE, 2);
    chunk.add_code(OP_RETURN, 100);
    println!("{}", chunk);

    vm.interpret(&chunk);

    Ok(())
}
