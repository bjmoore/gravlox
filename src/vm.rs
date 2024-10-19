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
                    println!("{}", self.stack.pop().unwrap());
                    return;
                }
                OP_CONSTANT => {
                    let const_idx = self.read_byte() as usize;
                    self.stack.push(*chunk.get_constant(const_idx));
                }
                OP_CONSTANT_LONG => {
                    let const_idx = (self.read_byte() as usize)
                        << 16 + (self.read_byte() as usize)
                        << 8 + (self.read_byte() as usize);
                    self.stack.push(*chunk.get_constant(const_idx));
                }
                OP_NEGATE => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(-value);
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
}
