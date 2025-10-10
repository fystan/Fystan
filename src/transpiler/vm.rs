use super::bytecode_generator::Opcode;
use std::collections::HashMap;

pub struct VM {
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
    pc: usize,
    bytecode: Vec<u8>,
    strings: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Value>),
}

impl VM {
    /// Creates a new VM instance with bytecode and strings.
    pub fn new(bytecode: Vec<u8>, strings: Vec<String>) -> Self {
        Self {
            stack: Vec::new(),
            variables: HashMap::new(),
            pc: 0,
            bytecode,
            strings,
        }
    }

    /// Runs the bytecode in the VM.
    pub fn run(&mut self) -> Result<(), String> {
        while self.pc < self.bytecode.len() {
            let opcode = self.read_opcode()?;
            match opcode {
                Opcode::LoadConst(val) => self.stack.push(Value::Int(val)),
                Opcode::LoadFloat(val) => self.stack.push(Value::Float(val)),
                Opcode::LoadString(idx) => {
                    if let Some(s) = self.strings.get(idx) {
                        self.stack.push(Value::Str(s.clone()));
                    } else {
                        return Err("Invalid string index".to_string());
                    }
                }
                Opcode::LoadBool(b) => self.stack.push(Value::Bool(b)),
                Opcode::LoadVar(name) => {
                    if let Some(val) = self.variables.get(&name) {
                        self.stack.push(val.clone());
                    } else {
                        // Initialize undefined variables to 0 for simplicity
                        self.stack.push(Value::Int(0));
                    }
                }
                Opcode::StoreVar(name) => {
                    if let Some(val) = self.stack.pop() {
                        self.variables.insert(name, val);
                    } else {
                        return Err("Stack underflow".to_string());
                    }
                }
                Opcode::Add => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Int(l + r)),
                        _ => return Err("Type error in add".to_string()),
                    }
                }
                Opcode::Sub => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Int(l - r)),
                        _ => return Err("Type error in sub".to_string()),
                    }
                }
                Opcode::Mul => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Int(l * r)),
                        _ => return Err("Type error in mul".to_string()),
                    }
                }
                Opcode::Div => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Int(l / r)),
                        _ => return Err("Type error in div".to_string()),
                    }
                }
                Opcode::Mod => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Int(l % r)),
                        _ => return Err("Type error in mod".to_string()),
                    }
                }
                Opcode::Eq => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(left == right));
                }
                Opcode::NotEq => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(left != right));
                }
                Opcode::Lt => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Bool(l < r)),
                        _ => return Err("Type error in lt".to_string()),
                    }
                }
                Opcode::Gt => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Int(l), Value::Int(r)) => self.stack.push(Value::Bool(l > r)),
                        _ => return Err("Type error in gt".to_string()),
                    }
                }
                Opcode::And => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Bool(l), Value::Bool(r)) => self.stack.push(Value::Bool(l && r)),
                        _ => return Err("Type error in and".to_string()),
                    }
                }
                Opcode::Or => {
                    let right = self.stack.pop().ok_or("Stack underflow")?;
                    let left = self.stack.pop().ok_or("Stack underflow")?;
                    match (left, right) {
                        (Value::Bool(l), Value::Bool(r)) => self.stack.push(Value::Bool(l || r)),
                        _ => return Err("Type error in or".to_string()),
                    }
                }
                Opcode::Not => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    match val {
                        Value::Bool(b) => self.stack.push(Value::Bool(!b)),
                        _ => return Err("Type error in not".to_string()),
                    }
                }
                Opcode::Neg => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    match val {
                        Value::Int(i) => self.stack.push(Value::Int(-i)),
                        _ => return Err("Type error in neg".to_string()),
                    }
                }
                Opcode::Jump(addr) => self.pc = addr,
                Opcode::JumpIfFalse(addr) => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    if let Value::Bool(false) = val {
                        self.pc = addr;
                    }
                }
                Opcode::Call(name, argc) => {
                    if name == "print" {
                        for _ in 0..argc {
                            if let Some(val) = self.stack.pop() {
                                match val {
                                    Value::Int(i) => print!("{}", i),
                                    Value::Float(f) => print!("{}", f),
                                    Value::Str(s) => print!("{}", s),
                                    Value::Bool(b) => print!("{}", b),
                                    Value::List(_) => print!("[List]"),
                                }
                            }
                        }
                        println!();
                    }
                }
                Opcode::Return => {
                    // For simplicity, ignore return values
                }
                Opcode::Print => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::Int(i) => println!("{}", i),
                            Value::Float(f) => println!("{}", f),
                            Value::Str(s) => println!("{}", s),
                            Value::Bool(b) => println!("{}", b),
                            Value::List(_) => println!("[List]"),
                        }
                    }
                }
                Opcode::Halt => break,
                Opcode::BuildList(size) => {
                    let mut list = Vec::with_capacity(size);
                    for _ in 0..size {
                        list.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    list.reverse();
                    self.stack.push(Value::List(list));
                }
            }
        }
        Ok(())
    }

    fn read_opcode(&mut self) -> Result<Opcode, String> {
        if self.pc >= self.bytecode.len() {
            return Err("Unexpected end of bytecode".to_string());
        }
        let op_byte = self.bytecode[self.pc];
        self.pc += 1;
        match op_byte {
            0 => {
                let val = self.read_i64()?;
                Ok(Opcode::LoadConst(val))
            }
            1 => {
                let idx = self.read_i64()? as usize;
                Ok(Opcode::LoadString(idx))
            }
            2 => {
                let b = self.bytecode[self.pc] != 0;
                self.pc += 1;
                Ok(Opcode::LoadBool(b))
            }
            3 => {
                let len = self.bytecode[self.pc] as usize;
                self.pc += 1;
                let name = String::from_utf8_lossy(&self.bytecode[self.pc..self.pc + len]).to_string();
                self.pc += len;
                Ok(Opcode::LoadVar(name))
            }
            4 => {
                let len = self.bytecode[self.pc] as usize;
                self.pc += 1;
                let name = String::from_utf8_lossy(&self.bytecode[self.pc..self.pc + len]).to_string();
                self.pc += len;
                Ok(Opcode::StoreVar(name))
            }
            5 => Ok(Opcode::Add),
            6 => Ok(Opcode::Sub),
            7 => Ok(Opcode::Mul),
            8 => Ok(Opcode::Div),
            9 => Ok(Opcode::Mod),
            10 => Ok(Opcode::Eq),
            11 => Ok(Opcode::NotEq),
            12 => Ok(Opcode::Lt),
            13 => Ok(Opcode::Gt),
            14 => Ok(Opcode::And),
            15 => Ok(Opcode::Or),
            16 => Ok(Opcode::Not),
            17 => Ok(Opcode::Neg),
            18 => {
                let addr = self.read_i64()? as usize;
                Ok(Opcode::Jump(addr))
            }
            19 => {
                let addr = self.read_i64()? as usize;
                Ok(Opcode::JumpIfFalse(addr))
            }
            20 => {
                let len = self.bytecode[self.pc] as usize;
                self.pc += 1;
                let name = String::from_utf8_lossy(&self.bytecode[self.pc..self.pc + len]).to_string();
                self.pc += len;
                let argc = self.bytecode[self.pc];
                self.pc += 1;
                Ok(Opcode::Call(name, argc as usize))
            }
            21 => Ok(Opcode::Return),
            22 => Ok(Opcode::Print),
            23 => Ok(Opcode::Halt),
            24 => {
                let val = self.read_f64()?;
                Ok(Opcode::LoadFloat(val))
            }
            25 => {
                let size = self.read_i64()? as usize;
                Ok(Opcode::BuildList(size))
            }
            _ => Err("Unknown opcode".to_string()),
        }
    }

    fn read_i64(&mut self) -> Result<i64, String> {
        if self.pc + 8 > self.bytecode.len() {
            return Err("Not enough bytes for i64".to_string());
        }
        let bytes: [u8; 8] = self.bytecode[self.pc..self.pc + 8].try_into().unwrap();
        self.pc += 8;
        Ok(i64::from_le_bytes(bytes))
    }

    fn read_f64(&mut self) -> Result<f64, String> {
        if self.pc + 8 > self.bytecode.len() {
            return Err("Not enough bytes for f64".to_string());
        }
        let bytes: [u8; 8] = self.bytecode[self.pc..self.pc + 8].try_into().unwrap();
        self.pc += 8;
        Ok(f64::from_le_bytes(bytes))
    }
}