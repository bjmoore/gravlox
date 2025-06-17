#[rustfmt::skip]
mod ops {
    pub const OP_RETURN:              u8 = 0x00;
    pub const OP_CONSTANT:            u8 = 0x01;
    pub const OP_CONSTANT_LONG:       u8 = 0x02;
    pub const OP_NEGATE:              u8 = 0x03;
    pub const OP_ADD:                 u8 = 0x04;
    pub const OP_SUBTRACT:            u8 = 0x05;
    pub const OP_MULTIPLY:            u8 = 0x06;
    pub const OP_DIVIDE:              u8 = 0x07;
    pub const OP_NIL:                 u8 = 0x08;
    pub const OP_TRUE:                u8 = 0x09;
    pub const OP_FALSE:               u8 = 0x0A;
    pub const OP_NOT:                 u8 = 0x0B;
    pub const OP_EQUAL:               u8 = 0x0C;
    pub const OP_GREATER:             u8 = 0x0D;
    pub const OP_LESS:                u8 = 0x0E;
    pub const OP_POP:                 u8 = 0x0F;
    pub const OP_PRINT:               u8 = 0x10;
    pub const OP_DEFINE_GLOBAL:       u8 = 0x11;
    pub const OP_DEFINE_CONST_GLOBAL: u8 = 0x12;
    pub const OP_GET_GLOBAL:          u8 = 0x13;
    pub const OP_SET_GLOBAL:          u8 = 0x14;
    pub const OP_GET_LOCAL:           u8 = 0x15;
    pub const OP_SET_LOCAL:           u8 = 0x16;
    pub const OP_GET_UPVALUE:         u8 = 0x17;
    pub const OP_SET_UPVALUE:         u8 = 0x18;
    pub const OP_JUMP_IF_FALSE:       u8 = 0x19;
    pub const OP_JUMP:                u8 = 0x1A;
    pub const OP_LOOP:                u8 = 0x1B;
    pub const OP_CALL:                u8 = 0x1C;
    pub const OP_CLOSURE:             u8 = 0x1D;
    pub const OP_CLOSURE_LONG:        u8 = 0x1E;
    pub const OP_CLOSE_UPVALUE:       u8 = 0x1F;
}

pub use ops::*;
