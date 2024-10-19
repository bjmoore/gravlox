use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::op::*;
use crate::value::Value;

const MAX_CONSTANTS: usize = 2usize.pow(24);

#[derive(Debug)]
pub struct MaxConstantsError;

impl Error for MaxConstantsError {}

impl Display for MaxConstantsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Too many constants in a single chunk (maximum: {})",
            MAX_CONSTANTS
        )
    }
}

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

    pub fn get_ip(&self) -> *const u8 {
        self.code.as_ptr()
    }

    pub fn add_code(&mut self, opcode: u8, line_number: u32) {
        self.code.push(opcode as u8);

        if let Some(line) = self.lineinfo.last_mut() {
            if line.0 == line_number {
                line.1 += 1;
            } else {
                self.lineinfo.push((line_number, 1));
            }
        } else {
            self.lineinfo.push((line_number, 1));
        }
    }

    pub fn add_constant(
        &mut self,
        value: Value,
        line_number: u32,
    ) -> Result<(), MaxConstantsError> {
        if self.constants.len() == MAX_CONSTANTS {
            return Err(MaxConstantsError);
        }
        self.constants.push(value);
        let const_idx = self.constants.len() - 1;
        if const_idx < 256 {
            self.add_code(OP_CONSTANT, line_number);
            self.add_code(const_idx as u8, line_number);
        } else {
            self.add_code(OP_CONSTANT_LONG, line_number);
            self.add_code((const_idx >> 16) as u8, line_number);
            self.add_code((const_idx >> 8) as u8, line_number);
            self.add_code(const_idx as u8, line_number);
        }
        Ok(())
    }

    pub fn get_constant(&self, const_idx: usize) -> &Value {
        &self.constants[const_idx]
    }

    fn print_simple_instr(
        &self,
        f: &mut Formatter<'_>,
        byte_index: usize,
        line_number: u32,
        name: &str,
    ) -> std::fmt::Result {
        writeln!(f, "{:04} {:>4} {}", byte_index, line_number, name)
    }

    fn print_const_instr(
        &self,
        f: &mut Formatter<'_>,
        byte_index: usize,
        line_number: u32,
        name: &str,
        value: Value,
    ) -> std::fmt::Result {
        writeln!(f, "{:04} {:>4} {} {}", byte_index, line_number, name, value)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut idx = 0;
        let mut line_iter = self.lineinfo.iter();
        let mut current_line_idx = 0;
        let mut current_line = &(0, 0);
        writeln!(f, "==== {} ====", self.name)?;

        while idx < self.code.len() {
            if current_line_idx == current_line.1 {
                current_line = line_iter.next().unwrap_or(&(0, 0));
            }
            match self.code[idx] {
                OP_RETURN => {
                    self.print_simple_instr(f, idx, current_line.0, "ret")?;
                }
                OP_CONSTANT => {
                    let const_idx = self.code[idx + 1] as usize;
                    self.print_const_instr(
                        f,
                        idx,
                        current_line.0,
                        "const",
                        self.constants[const_idx],
                    )?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_CONSTANT_LONG => {
                    let const_idx = (self.code[idx + 1] as usize)
                        << 16 + (self.code[idx + 2] as usize)
                        << 8 + (self.code[idx + 3] as usize);
                    self.print_const_instr(
                        f,
                        idx,
                        current_line.0,
                        "const_long",
                        self.constants[const_idx],
                    )?;
                    idx += 3;
                    current_line_idx += 3;
                }
                OP_NEGATE => {
                    self.print_simple_instr(f, idx, current_line.0, "neg")?;
                }
                OP_ADD => {
                    self.print_simple_instr(f, idx, current_line.0, "add")?;
                }
                OP_SUBTRACT => {
                    self.print_simple_instr(f, idx, current_line.0, "sub")?;
                }
                OP_MULTIPLY => {
                    self.print_simple_instr(f, idx, current_line.0, "mul")?;
                }
                OP_DIVIDE => {
                    self.print_simple_instr(f, idx, current_line.0, "div")?;
                }
                _ => unreachable!("Unknown opcode: 0x{:02x}", self.code[idx]),
            };

            idx += 1;
            current_line_idx += 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_overflow_to_constant_long() {
        let mut test_chunk = Chunk::new("test");
        for _ in 0..=256 {
            let _ = test_chunk.add_constant(1.234, 0);
        }
        assert_eq!(test_chunk.code.len(), 256 * 2 + 4, "code: {}", test_chunk);
        assert_eq!(test_chunk.code[512..], [OP_CONSTANT_LONG, 0, 1, 0]);
    }
}
