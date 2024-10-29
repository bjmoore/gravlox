use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
    ObjRef(Rc<RefCell<Obj>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::ObjRef(v) => write!(f, "{:?}", v.borrow()),
        }
    }
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Number(n) => *n == 0f64,
            Value::ObjRef(_) => true,
        }
    }

    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            _ => false, // Different types are unequal
        }
    }
}

#[derive(Debug, Clone)]
pub enum Obj {
    String(String),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(s) => write!(f, "{}", s),
        }
    }
}
