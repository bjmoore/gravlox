use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::GravloxError;
use crate::op::*;
use crate::value::FunctionPtr;
use crate::value::Value;

const NULL_FRAME_MSG: &'static str = "Current frame should not be None";

pub struct GravloxVM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    // The book uses a fixed-size array of CallFrames, but I'm just going to use a vec for now to keep it simple.
    frames: Vec<CallFramePtr>,
    current_frame: Option<CallFramePtr>,
}

impl GravloxVM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            frames: Vec::new(),
            current_frame: None,
        }
    }

    pub fn interpret(&mut self, func: FunctionPtr) -> Result<(), GravloxError> {
        self.current_frame = Some(self.new_frame_ptr(func));
        self.run()
    }

    fn run(&mut self) -> Result<(), GravloxError> {
        loop {
            let opcode = self.read_byte();

            match opcode {
                OP_RETURN => {
                    return Ok(());
                }
                OP_CONSTANT => {
                    let const_idx = self.read_byte() as usize;
                    self.push(self.read_constant(const_idx));
                }
                OP_CONSTANT_LONG => {
                    #[rustfmt::skip]
                    let const_idx = (self.read_byte() as usize) << 16
                                  + (self.read_byte() as usize) << 8
                                  + (self.read_byte() as usize);
                    self.push(self.read_constant(const_idx));
                }
                OP_NEGATE => {
                    let value = self.peek(0);
                    if let Value::Number(value) = value {
                        let _ = self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        return self.runtime_error("Cannot negate non-number value");
                    }
                }
                OP_ADD => {
                    let b = self.peek(0);
                    let a = self.peek(1);

                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.pop();
                            self.pop();
                            self.push(Value::Number(a + b));
                        }
                        (Value::StringRef(a), Value::StringRef(b)) => {
                            self.pop();
                            self.pop();
                            let obj_a = a.borrow().clone();
                            let obj_b = b.borrow().clone();
                            let combined = Rc::new(RefCell::new(obj_a + &obj_b));
                            self.push(Value::StringRef(combined));
                        }
                        _ => {
                            return self
                                .runtime_error("+ operands must be both numbers or both strings");
                        }
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
                        return self.runtime_error("- operands must be both numbers");
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
                        return self.runtime_error("* operands must be both numbers");
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
                        return self.runtime_error("/ operands must be both numbers");
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
                    self.push(Value::Bool(!a.as_bool()));
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
                        return self.runtime_error("< operands must be both numbers");
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
                        return self.runtime_error("> operands must be both numbers");
                    }
                }
                OP_POP => {
                    self.pop();
                }
                OP_PRINT => {
                    let value = self.pop();
                    println!("{}", value);
                }
                OP_DEFINE_GLOBAL => {
                    let const_idx = self.read_byte() as usize;
                    let name = match self.read_constant(const_idx) {
                        Value::StringRef(s) => s.borrow().clone(),
                        _ => unreachable!(),
                    };
                    let value = self.peek(0);

                    self.globals.insert(name, value);
                    self.pop();
                }
                OP_GET_GLOBAL => {
                    let const_idx = self.read_byte() as usize;
                    let name = match self.read_constant(const_idx) {
                        Value::StringRef(obj) => obj.borrow().clone(),
                        _ => unreachable!(),
                    };
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            return self.runtime_error("Undefined variable");
                        }
                    };

                    self.push(value);
                }
                OP_SET_GLOBAL => {
                    let const_idx = self.read_byte() as usize;
                    let name = match self.read_constant(const_idx) {
                        Value::StringRef(obj) => obj.borrow().clone(),
                        _ => unreachable!(),
                    };
                    if self.globals.contains_key(&name) {
                        self.globals.insert(name, self.peek(0));
                    } else {
                        return self.runtime_error("Undefined variable");
                    }
                }
                OP_GET_LOCAL => {
                    let slot = self.read_byte() as usize + self.current_frame().stack_offset;
                    self.push(self.stack[slot].clone());
                }
                OP_SET_LOCAL => {
                    let slot = self.read_byte() as usize + self.current_frame().stack_offset;
                    self.stack[slot] = self.peek(0).clone();
                }
                OP_JUMP_IF_FALSE => {
                    #[rustfmt::skip]
		    let distance = ((self.read_byte() as usize) << 8)
			         + ((self.read_byte() as usize));
                    let condition = self.peek(0);
                    if !condition.as_bool() {
                        self.jump(distance);
                    }
                }
                OP_JUMP => {
                    #[rustfmt::skip]
		    let distance = ((self.read_byte() as usize) << 8)
			         + ((self.read_byte() as usize));
                    self.jump(distance);
                }
                OP_LOOP => {
                    #[rustfmt::skip]
		    let distance = ((self.read_byte() as usize) << 8)
			         + ((self.read_byte() as usize));
                    self.jump_back(distance);
                }
                _ => unreachable!("Unknown opcode while executing chunk: 0x{:02x}", opcode),
            }
        }
    }

    #[allow(dead_code)]
    // Utility method for debugging.
    fn peek_byte(&mut self) -> u8 {
        let current_frame = self.current_frame();
        unsafe { *current_frame.ip }
    }

    fn read_byte(&mut self) -> u8 {
        let mut current_frame = self.current_frame_mut();
        unsafe {
            let ret = *current_frame.ip;
            current_frame.ip = current_frame.ip.add(1);
            ret
        }
    }

    fn read_constant(&self, const_idx: usize) -> Value {
        let current_frame = self.current_frame();
        let ret = current_frame
            .func()
            .borrow()
            .chunk()
            .borrow()
            .get_constant(const_idx);
        ret
    }

    fn jump(&mut self, distance: usize) {
        let mut current_frame = self.current_frame_mut();
        unsafe {
            current_frame.ip = current_frame.ip.add(distance);
        }
    }

    fn jump_back(&mut self, distance: usize) {
        let mut current_frame = self.current_frame_mut();
        unsafe {
            current_frame.ip = current_frame.ip.sub(distance);
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, depth: usize) -> Value {
        self.stack[self.stack.len() - depth - 1].clone()
    }

    fn runtime_error(&self, message: &str) -> Result<(), GravloxError> {
        let ip = self.current_frame().ip;
        let line = self
            .current_frame()
            .func()
            .borrow()
            .chunk()
            .borrow()
            .get_line(ip);

        Err(GravloxError::RuntimeError(format!(
            "[line {}] Error: {}",
            line, message
        )))
    }

    fn new_frame_ptr(&self, func: FunctionPtr) -> CallFramePtr {
        Rc::new(RefCell::new(CallFrame {
            func: func.clone(),
            ip: func.borrow().chunk().borrow().get_ip(),
            stack_offset: self.stack.len(),
        }))
    }

    fn current_frame(&self) -> Ref<CallFrame> {
        self.current_frame.as_ref().expect(NULL_FRAME_MSG).borrow()
    }

    fn current_frame_mut(&self) -> RefMut<CallFrame> {
        self.current_frame
            .as_ref()
            .expect(NULL_FRAME_MSG)
            .borrow_mut()
    }
}

struct CallFrame {
    func: FunctionPtr,
    ip: *const u8,
    stack_offset: usize,
}

type CallFramePtr = Rc<RefCell<CallFrame>>;

impl CallFrame {
    pub fn func(&self) -> FunctionPtr {
        self.func.clone()
    }
}
