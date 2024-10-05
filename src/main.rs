use crate::value::Value;
use lazy_static::lazy_static;
use std::{
    fmt::{Display, Error, Formatter},
    sync::Mutex,
};

mod value;

lazy_static! {
    static ref CONSTANTS: Mutex<Vec<Value>> = Mutex::new(vec![]);
}

fn main() {
    CONSTANTS.lock().unwrap().push(Value::Double(1.0));
    let program = Vec::from([Op::Return, Op::Constant(0)]);

    program.iter().for_each(|op| println!("{}", op));
}

enum Op {
    Return,
    Constant(usize),
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Op::Return => write!(f, "ret"),
            Op::Constant(index) => write!(f, "const {}", CONSTANTS.lock().unwrap()[*index]),
        }
    }
}
