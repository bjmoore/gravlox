use std::fmt::{Display, Error, Formatter};

use crate::op::*;
use crate::value::Value;

pub struct Chunk {
    name: String,
    code: Vec<u8>,
    constants: Vec<Value>,
    lineinfo: Vec<(u32, u32)>,
}

impl Chunk {
    pub fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
            code: Vec::new(),
            constants: Vec::new(),
            lineinfo: Vec::new(),
        }
    }

    pub fn add_op(&mut self, opcode: Op, line_idx: u32) {
        self.code.push(opcode as u8);
    }

    pub fn add_data(&mut self, data: u8) {
        self.code.push(data);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let mut line_number = 0;
        let mut idx = 0;

        writeln!(f, "==== {} ====", self.name)?;

        while idx < self.code.len() {
            match self.code[idx].into() {
                Op::Return => {
                    writeln!(f, "{} {} {}", idx, line_number, "ret")?;
                }
                Op::Constant => {
                    let const_idx = self.code[idx + 1] as usize;
                    idx += 1;
                    writeln!(
                        f,
                        "{} {} {} {}",
                        idx, line_number, "const", self.constants[const_idx]
                    )?;
                }
            }

            idx += 1;
        }

        Ok(())
    }
}
