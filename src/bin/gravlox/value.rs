use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use crate::chunk::Chunk;
use crate::error::GravloxError;

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
    pub chunk: Chunk,
    name: String,
}

pub fn new_function(name: &str) -> FunctionPtr {
    let func = Function {
	arity: 0,
	chunk: Chunk::new(name),
	name: String::from(name),
    };

    FunctionPtr{ func: Rc::new(RefCell::new(func)) }
}

#[derive(Clone)]
pub struct FunctionPtr {
    pub func: Rc<RefCell<Function>>
}

impl FunctionPtr {
    pub fn new(func: &Rc<RefCell<Function>>) -> Self {
	Self {
	    func: func.clone()
	}
    }

    pub fn add_code(&mut self, byte: u8, line_number: u32) {
	self.func.borrow_mut().chunk.add_code(byte, line_number);
    }

    pub fn add_constant(&mut self, value: Value, line_number: u32) -> Result<usize, GravloxError> {
	self.func.borrow_mut().chunk.add_constant(value, line_number)
    }

    pub fn count(&self) -> usize {
        self.func.borrow().chunk.count()
    }

    pub fn patch_byte(&mut self, offset: usize, byte: u8) {
	self.func.borrow_mut().chunk.patch_byte(offset, byte)
    }
}

impl Display for FunctionPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "{}", self.func.borrow().name)
    }
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
