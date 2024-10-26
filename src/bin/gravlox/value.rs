use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
        }
    }
}
