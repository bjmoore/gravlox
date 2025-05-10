use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use crate::chunk::Chunk;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
    StringRef(Rc<RefCell<String>>),
    FunctionRef(Rc<RefCell<Function>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::StringRef(v) => write!(f, "{}", v.borrow()),
	    Value::FunctionRef(v) => write!(f, "{}", v.borrow().name),
        }
    }
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Number(n) => *n != 0f64,
	    _ => true
        }
    }

    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
	    (Value::StringRef(a), Value::StringRef(b)) => {
		*a == *b
	    },
            _ => false, // Different types are unequal
        }
    }
}

#[derive(Debug)]
pub struct Function {
    arity: usize,
    chunk: Chunk,
    name: String,
}

pub fn new_function(name: &str) -> Value {
    let func = Function {
	arity: 0,
	chunk: Chunk::new(name),
	name: String::from(name),
    };

    Value::FunctionRef(Rc::new(RefCell::new(func)))
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_truthiness_primitive() {
        assert_eq!(Value::Nil.as_bool(), false);
        assert_eq!(Value::Number(0.0).as_bool(), false);
        assert_eq!(Value::Number(1.0).as_bool(), true);
        assert_eq!(Value::Bool(false).as_bool(), false);
        assert_eq!(Value::Bool(true).as_bool(), true);
    }
}
