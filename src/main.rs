use chunk::Chunk;
use value::Value;

mod chunk;
mod op;
mod value;

fn main() {
    let mut chunk = Chunk::new();

    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_CONSTANT, 1);
    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_CONSTANT, 1);
    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_CONSTANT, 1);
    chunk.add_op(op::OP_RETURN, 1);
    chunk.add_op(op::OP_RETURN, 1);

    println!("{}", chunk);
}
