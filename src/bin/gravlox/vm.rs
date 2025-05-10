use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::GravloxError;
use crate::op::*;
use crate::value::Obj;
use crate::value::Value;

pub struct GravloxVM {
    ip: *const u8,
    stack: Vec<Value>,
    heap: Vec<Rc<RefCell<Obj>>>,
    globals: HashMap<String, Value>,
}

impl GravloxVM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null(),
            stack: Vec::new(),
            heap: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), GravloxError> {
        self.ip = chunk.get_ip();
        self.run(chunk)
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), GravloxError> {
        loop {
            let opcode = self.read_byte();

            match opcode {
                OP_RETURN => {
                    return Ok(());
                }
                OP_CONSTANT => {
                    let const_idx = self.read_byte() as usize;
                    self.push(chunk.get_constant(const_idx).clone());
                }
                OP_CONSTANT_LONG => {
                    #[rustfmt::skip]
                    let const_idx = (self.read_byte() as usize) << 16
                                  + (self.read_byte() as usize) << 8
                                  + (self.read_byte() as usize);
                    self.push(chunk.get_constant(const_idx).clone());
                }
                OP_NEGATE => {
                    let value = self.peek(0);
                    if let Value::Number(value) = value {
                        let _ = self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        println!("[line {}]: Error: {}", chunk.get_line(self.ip), "asdf");
                        return self.runtime_error("Cannot negate non-number value", chunk);
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
                        (Value::ObjRef(a), Value::ObjRef(b))
                            if a.borrow().is_string() && b.borrow().is_string() =>
                        {
                            self.pop();
                            self.pop();
                            let obj_a = a.borrow().clone();
                            let obj_b = b.borrow().clone();
                            match (obj_a, obj_b) {
                                (Obj::String(a), Obj::String(b)) => {
                                    let heap_obj = Rc::new(RefCell::new(Obj::String(a + &b)));
                                    self.heap.push(heap_obj.clone());
                                    self.push(Value::ObjRef(heap_obj));
                                }
                            }
                        }
                        _ => {
                            return self.runtime_error(
                                "+ operands must be both numbers or both strings",
                                chunk,
                            );
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
                        return self.runtime_error("- operands must be both numbers", chunk);
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
                        return self.runtime_error("* operands must be both numbers", chunk);
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
                        return self.runtime_error("/ operands must be both numbers", chunk);
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
                        return self.runtime_error("< operands must be both numbers", chunk);
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
                        return self.runtime_error("> operands must be both numbers", chunk);
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
                    let name = match chunk.get_constant(const_idx) {
                        Value::ObjRef(obj) => match obj.borrow().clone() {
                            Obj::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    let value = self.peek(0);

                    self.globals.insert(name, value);
                    self.pop();
                }
                OP_GET_GLOBAL => {
                    let const_idx = self.read_byte() as usize;
                    let name = match chunk.get_constant(const_idx) {
                        Value::ObjRef(obj) => match obj.borrow().clone() {
                            Obj::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    let value = match self.globals.get(&name) {
                        Some(value) => value.clone(),
                        None => {
                            return self.runtime_error("Undefined variable", chunk);
                        }
                    };

                    self.push(value);
                }
                OP_SET_GLOBAL => {
                    let const_idx = self.read_byte() as usize;
                    let name = match chunk.get_constant(const_idx) {
                        Value::ObjRef(obj) => match obj.borrow().clone() {
                            Obj::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    if self.globals.contains_key(&name) {
                        self.globals.insert(name, self.peek(0));
                    } else {
                        return self.runtime_error("Undefined variable", chunk);
                    }
                }
                OP_GET_LOCAL => {
                    let slot = self.read_byte() as usize;
                    self.push(self.stack[slot].clone());
                }
                OP_SET_LOCAL => {
                    let slot = self.read_byte() as usize;
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
        unsafe {
            let ret = *self.ip;
            ret
        }
    }

    fn read_byte(&mut self) -> u8 {
        unsafe {
            let ret = *self.ip;
            self.ip = self.ip.add(1);
            ret
        }
    }

    fn jump(&mut self, distance: usize) {
        unsafe {
            self.ip = self.ip.add(distance);
        }
    }

    fn jump_back(&mut self, distance: usize) {
        unsafe {
            self.ip = self.ip.sub(distance);
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

    fn runtime_error(&self, message: &str, chunk: &Chunk) -> Result<(), GravloxError> {
        let line = chunk.get_line(self.ip);

        Err(GravloxError::RuntimeError(format!(
            "[line {}] Error: {}",
            line, message
        )))
    }
}
