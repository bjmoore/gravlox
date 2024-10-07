use std::fmt::{Display, Error, Formatter};

pub enum Value {
    Double(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Value::Double(val) => write!(f, "{}", val),
        }
    }
}
