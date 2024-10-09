use std::fmt::{Display, Error, Formatter};

use crate::op::*;
use crate::value::Value;

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lineinfo: Vec<(u32, u32)>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lineinfo: Vec::new(),
        }
    }

    pub fn add_op(&mut self, opcode: u8, line_idx: u32) {
        self.code.push(opcode);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len()
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let line_number = 0;
        let mut state = State::Fetch;
        let mut data: Vec<u8> = Vec::new();
        for (byte_idx, byte) in self.code.iter().enumerate() {
            match state {
                State::Fetch => match *byte {
                    OP_RETURN => write!(f, "{:0>4} {:>4} {:>4}", byte_idx, line_number, "ret"),
                    OP_CONSTANT => {
                        state = State::LoadData(1);
                        println!("bytes_needed: {}", 1);
                        Ok(())
                    }
                    _ => panic!("unknown opcode"),
                },
                State::LoadData(mut bytes_needed) => {
                    bytes_needed -= 1;
                    println!("bytes_needed: {}", bytes_needed);
                    if bytes_needed == 0 {
                        state = State::DisplayExtendedInstruction(OP_CONSTANT);
                    }
                    Ok(())
                }
            }?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}

enum State {
    Fetch,
    LoadData(usize),
    DisplayExtendedInstruction(u8),
}
