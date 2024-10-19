use chunk::Chunk;
use op::{OP_CONSTANT, OP_RETURN};
use std::error::Error;
use value::Value;

mod chunk;
mod op;
mod value;

fn main() -> Result<(), Box<dyn Error>> {
    let mut chunk = Chunk::new("demo");

    chunk.add_constant(Value::Double(3.14159), 1)?;
    chunk.add_code(OP_RETURN, 100);

    println!("{}", chunk);

    Ok(())
}
