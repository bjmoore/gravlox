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
                    let value = self.pop();
                    self.push(-value);
                }
                OP_ADD => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a + b);
                }
                OP_SUBTRACT => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a - b);
                }
                OP_MULTIPLY => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a * b);
                }
                OP_DIVIDE => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a / b);
                }
                _ => unreachable!("Unknown opcode: 0x{:02x}", opcode),
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
}
