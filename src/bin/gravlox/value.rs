use crate::chunk::Chunk;
use crate::chunk::ChunkPtr;
use crate::obj::{make_obj, Obj};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
    StringRef(Obj<String>),
    FunctionRef(Obj<Function>),
    NativeRef(Obj<Native>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::StringRef(v) => write!(f, "{}", v.borrow()),
            Value::FunctionRef(v) => write!(
                f,
                "{}",
                v.borrow().name.as_ref().map_or("<root>", |n| n.as_str())
            ),
	    Value::NativeRef(_v) => write!(f, "<native>"),
        }
    }
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Number(n) => *n != 0f64,
            _ => true,
        }
    }

    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::StringRef(a), Value::StringRef(b)) => *a == *b,
            _ => false, // Different types are unequal
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    chunk: Obj<Chunk>,
    pub name: Option<String>,
}

impl Function {
    pub fn new(name: Option<&str>) -> Self {
        Self {
            arity: 0,
            chunk: make_obj(Chunk::new()),
            name: name.map(String::from),
        }
    }

    pub fn chunk(&self) -> ChunkPtr {
        self.chunk.clone()
    }
}

#[derive(Debug)]
pub struct Native {
    pub arity: usize,
    pub func: fn(&[Value]) -> Value,
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
