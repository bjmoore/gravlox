#[repr(u8)]
pub enum Op {
    Return = 0x00,
    Constant = 0x01,
}

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        match value {
            0x00 => Op::Return,
            0x01 => Op::Constant,
            _ => unreachable!("Unknown opcode: {}", value),
        }
    }
}
