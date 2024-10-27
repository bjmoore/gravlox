use std::ptr;

use crate::chunk::Chunk;
use crate::op::*;
use crate::value::Value;

pub struct GravloxVM {
    ip: *const u8,
    stack: Vec<Value>,
}

impl GravloxVM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null(),
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) {
        self.ip = chunk.get_ip();
        self.run(chunk);
    }

    fn run(&mut self, chunk: &Chunk) {
        loop {
            let opcode = self.read_byte();

            match opcode {
                OP_RETURN => {
                    println!("{}", self.pop());
                    return;
                }
                OP_CONSTANT => {
                    let const_idx = self.read_byte() as usize;
                    self.push(*chunk.get_constant(const_idx));
                }
                OP_CONSTANT_LONG => {
                    let const_idx = (self.read_byte() as usize)
                        << 16 + (self.read_byte() as usize)
                        << 8 + (self.read_byte() as usize);
                    self.push(*chunk.get_constant(const_idx));
                }
                OP_NEGATE => {
                    let value = self.peek(0);
                    if let Value::Number(value) = value {
                        let _ = self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        // emit an error if we get here
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
                        // emit an error here
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
                        // emit an error here
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
                        // emit an error here
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
                        // emit an error here
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
                    self.push(Value::Bool(a.is_falsey()));
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
                        // emit an error here
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
                        // emit an error here
                    }
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
        self.stack[self.stack.len() - depth - 1]
    }

    fn runtime_error() {}
}
