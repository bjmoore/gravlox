use std::borrow::Borrow;
use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::error::GravloxError;
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

impl std::fmt::Debug for Chunk {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("Chunk")
            .field("name", &self.name)
            .field("code", &self.code)
            .field("constants", &self.constants)
            .finish()
    }
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

    pub fn add_code(&mut self, code: u8, line_number: u32) {
        self.code.push(code as u8);

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

    pub fn add_constant(&mut self, value: Value, line_number: u32) -> Result<usize, GravloxError> {
        if self.constants.len() == MAX_CONSTANTS {
            return Err(GravloxError::CompileError(
                "Too many constants in a single chunk.",
            ));
        }
        self.constants.push(value);
        let const_idx = self.constants.len() - 1;
        Ok(const_idx)
    }

    pub fn get_constant(&self, const_idx: usize) -> Value {
        self.constants[const_idx].clone()
    }

    pub fn get_line(&self, ip: *const u8) -> u32 {
        let byte_index = unsafe { ip.offset_from(self.get_ip()) };

        let mut current_line = &(0, 0);
        let mut current_line_idx = 0;
        let mut lineinfo_iter = self.lineinfo.iter();
        for _ in 0..byte_index {
            if current_line_idx == current_line.1 {
                current_line = lineinfo_iter.next().unwrap_or(&(0, 0));
                current_line_idx = 0;
            } else {
                current_line_idx += 1;
            }
        }

        current_line.0
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn patch_byte(&mut self, offset: usize, byte: u8) {
        self.code[offset] = byte;
    }
}

fn print_simple_instr(
    f: &mut Formatter<'_>,
    byte_index: usize,
    line_display: &str,
    name: &str,
) -> std::fmt::Result {
    writeln!(f, "{:04} {:>4} {}", byte_index, line_display, name)
}

fn print_const_instr(
    f: &mut Formatter<'_>,
    byte_index: usize,
    line_display: &str,
    name: &str,
    value: &Value,
) -> std::fmt::Result {
    writeln!(
        f,
        "{:04} {:>4} {} {}",
        byte_index, line_display, name, value
    )
}

fn print_byte_instr(
    f: &mut Formatter<'_>,
    byte_index: usize,
    line_display: &str,
    name: &str,
    slot: usize,
) -> std::fmt::Result {
    writeln!(f, "{:04} {:>4} {} {}", byte_index, line_display, name, slot)
}

fn print_jump_instr(
    f: &mut Formatter<'_>,
    byte_index: usize,
    line_display: &str,
    name: &str,
    jump_size: u16,
) -> std::fmt::Result {
    writeln!(
        f,
        "{:04} {:>4} {} {}",
        byte_index, line_display, name, jump_size
    )
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut idx = 0;
        let mut lineinfo_iter = self.lineinfo.iter();
        let mut current_line_idx = 0;
        let mut current_line = &(0, 0);
        let mut line_display;
        writeln!(f, "==== {} ====", self.name)?;

        while idx < self.code.len() {
            if current_line_idx == current_line.1 {
                current_line = lineinfo_iter.next().unwrap_or(&(0, 0));
                current_line_idx = 0;
                line_display = format!("{:>4}", current_line.0);
            } else {
                line_display = String::from("   |");
            }
            match self.code[idx] {
                OP_RETURN => {
                    print_simple_instr(f, idx, &line_display, "ret")?;
                }
                OP_CONSTANT => {
                    let const_idx = self.code[idx + 1] as usize;
                    print_const_instr(
                        f,
                        idx,
                        &line_display,
                        "const",
                        self.constants[const_idx].borrow(),
                    )?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_CONSTANT_LONG => {
                    #[rustfmt::skip]
                    let const_idx = ((self.code[idx + 1] as usize) << 16)
			          + ((self.code[idx + 2] as usize) << 8)
			          + ((self.code[idx + 3] as usize));
                    print_const_instr(
                        f,
                        idx,
                        &line_display,
                        "const_long",
                        self.constants[const_idx].borrow(),
                    )?;
                    idx += 3;
                    current_line_idx += 3;
                }
                OP_NEGATE => {
                    print_simple_instr(f, idx, &line_display, "neg")?;
                }
                OP_ADD => {
                    print_simple_instr(f, idx, &line_display, "add")?;
                }
                OP_SUBTRACT => {
                    print_simple_instr(f, idx, &line_display, "sub")?;
                }
                OP_MULTIPLY => {
                    print_simple_instr(f, idx, &line_display, "mul")?;
                }
                OP_DIVIDE => {
                    print_simple_instr(f, idx, &line_display, "div")?;
                }
                OP_NIL => {
                    print_simple_instr(f, idx, &line_display, "nil")?;
                }
                OP_TRUE => {
                    print_simple_instr(f, idx, &line_display, "true")?;
                }
                OP_FALSE => {
                    print_simple_instr(f, idx, &line_display, "false")?;
                }
                OP_NOT => {
                    print_simple_instr(f, idx, &line_display, "not")?;
                }
                OP_EQUAL => {
                    print_simple_instr(f, idx, &line_display, "equal")?;
                }
                OP_GREATER => {
                    print_simple_instr(f, idx, &line_display, "greater")?;
                }
                OP_LESS => {
                    print_simple_instr(f, idx, &line_display, "less")?;
                }
                OP_POP => {
                    print_simple_instr(f, idx, &line_display, "pop")?;
                }
                OP_PRINT => {
                    print_simple_instr(f, idx, &line_display, "print")?;
                }
                OP_DEFINE_GLOBAL => {
                    let const_idx = self.code[idx + 1] as usize;
                    print_const_instr(
                        f,
                        idx,
                        &line_display,
                        "def_global",
                        self.constants[const_idx].borrow(),
                    )?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_GET_GLOBAL => {
                    let const_idx = self.code[idx + 1] as usize;
                    print_const_instr(
                        f,
                        idx,
                        &line_display,
                        "get_global",
                        self.constants[const_idx].borrow(),
                    )?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_SET_GLOBAL => {
                    let const_idx = self.code[idx + 1] as usize;
                    print_const_instr(
                        f,
                        idx,
                        &line_display,
                        "set_global",
                        self.constants[const_idx].borrow(),
                    )?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_GET_LOCAL => {
                    let slot = self.code[idx + 1] as usize;
                    print_byte_instr(f, idx, &line_display, "get_local", slot)?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_SET_LOCAL => {
                    let slot = self.code[idx + 1] as usize;
                    print_byte_instr(f, idx, &line_display, "set_local", slot)?;
                    idx += 1;
                    current_line_idx += 1;
                }
                OP_JUMP_IF_FALSE => {
                    #[rustfmt::skip]
		    let jump_size = ((self.code[idx + 1] as u16) << 8)
			          + ((self.code[idx + 2] as u16));
                    print_jump_instr(f, idx, &line_display, "jump_false", jump_size)?;
                    idx += 2;
                    current_line_idx += 2;
                }
                OP_JUMP => {
                    #[rustfmt::skip]
		    let jump_size = ((self.code[idx + 1] as u16) << 8)
			          + ((self.code[idx + 2] as u16));
                    print_jump_instr(f, idx, &line_display, "jump", jump_size)?;
                    idx += 2;
                    current_line_idx += 2;
                }
                OP_LOOP => {
                    #[rustfmt::skip]
		    let jump_size = ((self.code[idx + 1] as u16) << 8)
			          + ((self.code[idx + 2] as u16));
                    print_jump_instr(f, idx, &line_display, "loop", jump_size)?;
                    idx += 2;
                    current_line_idx += 2;
                }
                _ => unreachable!(
                    "Unknown opcode while printing chunk: 0x{:02x}",
                    self.code[idx]
                ),
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
            let _ = test_chunk.add_constant(Value::Number(1.234), 0);
        }
        assert_eq!(test_chunk.code.len(), 256 * 2 + 4, "code: {}", test_chunk);
        assert_eq!(test_chunk.code[512..], [OP_CONSTANT_LONG, 0, 1, 0]);
    }
}
