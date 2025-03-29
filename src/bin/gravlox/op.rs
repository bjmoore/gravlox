#[rustfmt::skip]
mod ops {
    pub const OP_RETURN:        u8 = 0x00;
    pub const OP_CONSTANT:      u8 = 0x01;
    pub const OP_CONSTANT_LONG: u8 = 0x02;
    pub const OP_NEGATE:        u8 = 0x03;
    pub const OP_ADD:           u8 = 0x04;
    pub const OP_SUBTRACT:      u8 = 0x05;
    pub const OP_MULTIPLY:      u8 = 0x06;
    pub const OP_DIVIDE:        u8 = 0x07;
    pub const OP_NIL:           u8 = 0x08;
    pub const OP_TRUE:          u8 = 0x09;
    pub const OP_FALSE:         u8 = 0x0A;
    pub const OP_NOT:           u8 = 0x0B;
    pub const OP_EQUAL:         u8 = 0x0C;
    pub const OP_GREATER:       u8 = 0x0D;
    pub const OP_LESS:          u8 = 0x0E;
    pub const OP_POP:           u8 = 0x0F;
    pub const OP_PRINT:         u8 = 0x10;
}

pub use ops::*;
