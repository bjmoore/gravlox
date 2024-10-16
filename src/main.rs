use chunk::Chunk;
use op::*;
use value::Value;

mod chunk;
mod op;
mod value;

fn main() {
    let mut chunk = Chunk::new("demo");

    let offset = chunk.add_constant(Value::Double(3.14159));
    chunk.add_op(Op::Constant, 1);
    chunk.add_data(offset as u8);
    chunk.add_op(Op::Return, 100);

    println!("{}", chunk);
}
