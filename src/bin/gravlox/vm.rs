use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::GravloxError;
use crate::op::*;
use crate::value::Obj;
use crate::value::Value;

pub struct GravloxVM {
    ip: *const u8,
    stack: Vec<Value>,
    heap: Vec<Rc<RefCell<Obj>>>,
}

impl GravloxVM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null(),
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), GravloxError> {
        self.ip = chunk.get_ip();
        self.run(chunk)
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), GravloxError> {
        loop {
            let opcode = self.read_byte();

            match opcode {
                OP_RETURN => {
                    return Ok(());
                }
                OP_CONSTANT => {
                    let const_idx = self.read_byte() as usize;
                    self.push(chunk.get_constant(const_idx).clone());
                }
                OP_CONSTANT_LONG => {
                    #[rustfmt::skip]
                    let const_idx = (self.read_byte() as usize) << 16 
                                         + (self.read_byte() as usize) << 8 
                                         + (self.read_byte() as usize);
                    self.push(chunk.get_constant(const_idx).clone());
                }
                OP_NEGATE => {
                    let value = self.peek(0);
                    if let Value::Number(value) = value {
                        let _ = self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        println!("[line {}]: Error: {}", chunk.get_line(self.ip), "asdf");
                        return self.runtime_error("Cannot negate non-number value", chunk);
                    }
                }
                OP_ADD => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Number(a + b));
                    } else {
                        return self.runtime_error(
                            "+ operands must be both numbers or both strings",
                            chunk,
                        );
                    }
                }
                OP_SUBTRACT => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Number(a - b));
                    } else {
                        return self.runtime_error("- operands must be both numbers", chunk);
                    }
                }
                OP_MULTIPLY => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Number(a * b));
                    } else {
                        return self.runtime_error("* operands must be both numbers", chunk);
                    }
                }
                OP_DIVIDE => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Number(a / b));
                    } else {
                        return self.runtime_error("/ operands must be both numbers", chunk);
                    }
                }
                OP_NIL => {
                    self.push(Value::Nil);
                }
                OP_TRUE => {
                    self.push(Value::Bool(true));
                }
                OP_FALSE => {
                    self.push(Value::Bool(false));
                }
                OP_NOT => {
                    let a = self.pop();
                    self.push(Value::Bool(!a.as_bool()));
                }
                OP_EQUAL => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a.equal(&b)));
                }
                OP_GREATER => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Bool(a > b));
                    } else {
                        return self.runtime_error("< operands must be both numbers", chunk);
                    }
                }
                OP_LESS => {
                    let b = self.peek(0);
                    let a = self.peek(1);
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        let _ = self.pop();
                        let _ = self.pop();
                        self.push(Value::Bool(a < b));
                    } else {
                        return self.runtime_error("> operands must be both numbers", chunk);
                    }
                }
                OP_POP => {
                    self.pop();
                }
                OP_PRINT => {
                    let value = self.pop();
                    println!("{}", value);
                }
                _ => unreachable!("Unknown opcode while executing chunk: 0x{:02x}", opcode),
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        unsafe {
            let ret = *self.ip;
            self.ip = self.ip.add(1);
            ret
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, depth: usize) -> Value {
        self.stack[self.stack.len() - depth - 1].clone()
    }

    fn runtime_error(&self, message: &str, chunk: &Chunk) -> Result<(), GravloxError> {
        let line = chunk.get_line(self.ip);

        Err(GravloxError::RuntimeError(format!(
            "[line {}] Error: {}",
            line, message
        )))
    }
}
