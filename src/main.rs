use crate::value::Value;
use std::fmt::{Display, Error, Formatter};

mod value;

fn main() {
    let constants = Vec::from([Value::Double(1.0)]);
    let program = Vec::from([Op::Return, Op::Constant(&constants[0])]);

    program.iter().for_each(|op| println!("{}", op));
}

enum Op<'a> {
    Constant(&'a Value),
    Return,
}

impl<'a> Display for Op<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Op::Return => write!(f, "ret"),
            Op::Constant(val) => write!(f, "const {}", val),
        }
    }
}
