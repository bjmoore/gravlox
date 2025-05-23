use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::error::RuntimeError;
use crate::obj::{make_obj, Obj};
use crate::op::*;
use crate::value::Function;
use crate::value::Native;
use crate::value::Value;

const NULL_FRAME_MSG: &'static str = "Current frame should not be None";
const FRAMES_MAX: usize = 255;

pub struct GravloxVM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    // The book uses a fixed-size array of CallFrames, but I'm just going to use a vec for now to keep it simple.
    frames: Vec<CallFrame>,
}

impl GravloxVM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            frames: Vec::new(),
        };

        vm.define_native("time", time, 0);

        vm
    }

    pub fn interpret(&mut self, func: Obj<Function>) {
        self.push(Value::FunctionRef(func.clone()));
        self.call(func, 0).expect("Failed calling VM entry point");
        if let Err(e) = self.run() {
            let ip = self.current_frame().ip;
            let line = self
                .current_frame()
                .func()
                .borrow()
                .chunk()
                .borrow()
                .get_line(ip);

            eprintln!("[line: {}] error: {}", line, e);
            for frame in self.frames.iter().rev() {
                let ip = self.current_frame().ip;
                let line = self
                    .current_frame()
                    .func()
                    .borrow()
                    .chunk()
                    .borrow()
                    .get_line(ip);
                eprintln!(
                    "in {}() at line {}",
                    frame
                        .func()
                        .borrow()
                        .name
                        .as_ref()
                        .map_or("unknown", |s| &s),
                    line
                );
            }
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let opcode = self.read_byte();

            match opcode {
                OP_RETURN => {
                    let result = self.pop();
                    let exiting_frame = self.frames.pop().expect("Call stack underflow");
                    if self.frames.len() == 0 {
                        let _main = self.pop();
                        return Ok(());
                    }

                    // We saved the stack height before entering this call frame, so reset the stack to that height afterward
                    self.stack.truncate(exiting_frame.stack_offset);
                    self.push(result);
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
                        return Err(RuntimeError::TypeError("-", "number"));
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
                            return Err(RuntimeError::TypeError("+", "numbers or strings"));
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
                        return Err(RuntimeError::TypeError("-", "numbers"));
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
                        return Err(RuntimeError::TypeError("*", "numbers"));
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
                        return Err(RuntimeError::TypeError("/", "numbers"));
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
                        return Err(RuntimeError::TypeError(">", "numbers"));
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
                        return Err(RuntimeError::TypeError("<", "numbers"));
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
                            return Err(RuntimeError::UndefinedVariable(name));
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
                        return Err(RuntimeError::UndefinedVariable(name));
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
                OP_CALL => {
                    let arg_count = self.read_byte() as usize;
                    self.call_value(self.peek(arg_count), arg_count)?;
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
        let current_frame = self.current_frame_mut();
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
        let current_frame = self.current_frame_mut();
        unsafe {
            current_frame.ip = current_frame.ip.add(distance);
        }
    }

    fn jump_back(&mut self, distance: usize) {
        let current_frame = self.current_frame_mut();
        unsafe {
            current_frame.ip = current_frame.ip.sub(distance);
        }
    }

    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(val) => val,
            None => {
                panic!("Stack underflow");
            }
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, depth: usize) -> Value {
        self.stack[self.stack.len() - depth - 1].clone()
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect(NULL_FRAME_MSG)
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect(NULL_FRAME_MSG)
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), RuntimeError> {
        match callee {
            Value::FunctionRef(f) => self.call(f, arg_count),
            Value::NativeRef(f) => {
                let args = &self.stack[self.stack.len() - arg_count..];
                let result = (f.borrow().func)(args);
                self.stack.truncate(self.stack.len() - arg_count - 1);
                self.push(result);
                Ok(())
            }
            _ => Err(RuntimeError::NonCallableValue),
        }
    }

    fn call(&mut self, callee: Obj<Function>, arg_count: usize) -> Result<(), RuntimeError> {
        if arg_count as usize != callee.borrow().arity {
            return Err(RuntimeError::ArityMismatch {
                expected: callee.borrow().arity,
                actual: arg_count,
            });
        }

        if self.frames.len() == FRAMES_MAX {
            return Err(RuntimeError::StackOverflow);
        }

        let ip = callee.borrow().chunk().borrow().get_ip();
        let new_frame = CallFrame {
            func: callee,
            ip,
            stack_offset: self.stack.len() - arg_count - 1,
        };
        self.frames.push(new_frame);

        Ok(())
    }

    fn define_native(&mut self, name: &'static str, nat: fn(&[Value]) -> Value, arity: usize) {
        self.push(Value::StringRef(make_obj(String::from(name))));
        self.push(Value::NativeRef(make_obj(Native { func: nat, arity })));
        self.globals
            .insert(String::from(name), self.stack[1].clone());
        self.pop();
        self.pop();
    }
}

struct CallFrame {
    func: Obj<Function>,
    ip: *const u8,
    stack_offset: usize,
}

impl CallFrame {
    pub fn func(&self) -> Obj<Function> {
        self.func.clone()
    }
}

fn time(_args: &[Value]) -> Value {
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    Value::Number(now.as_secs_f32() as f64)
}
