use chunk::Chunk;
use op::Op;
use value::Value;

mod chunk;
mod op;
mod value;

fn main() {
    let mut chunk = Chunk::new();

    chunk.add_op(Op::Return, 1);
    chunk.add_constant(Value::Double(1.234f64));
    chunk.add_op(Op::Constant(0), 800);

    println!("{}", chunk);
}
