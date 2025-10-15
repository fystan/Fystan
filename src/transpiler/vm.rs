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
    None,
    Function(String, usize, Vec<u8>), // name, arity, bytecode
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
                                    Value::None => print!("None"),
                                    Value::Function(..) => print!("[Function]"),
                                }
                            }
                        }
                        println!();
                    } else {
                        if let Some(Value::Function(_name, _arity, body)) = self.variables.get(&name).cloned() {
                            let mut func_vm = VM::new(body, self.strings.clone());
                            for _ in 0..argc {
                                func_vm.stack.push(self.stack.pop().unwrap());
                            }
                            func_vm.run()?;
                            if let Some(return_value) = func_vm.stack.pop() {
                                self.stack.push(return_value);
                            }
                        } else {
                            return Err(format!("Undefined function: {}", name));
                        }
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
                            Value::None => println!("None"),
                            Value::Function(..) => println!("[Function]"),
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
                Opcode::LoadNone => self.stack.push(Value::None),
                Opcode::DefFn(name, arity, body) => {
                    self.variables.insert(name.clone(), Value::Function(name, arity, body));
                }
                Opcode::GetItem => {
                    let index = self.stack.pop().ok_or("Stack underflow for index")?;
                    let list = self.stack.pop().ok_or("Stack underflow for list")?;
                    match (list, index) {
                        (Value::List(l), Value::Int(i)) => {
                            if i < 0 || i as usize >= l.len() {
                                return Err("Index out of bounds".to_string());
                            }
                            self.stack.push(l[i as usize].clone());
                        }
                        _ => return Err("Type error in get_item".to_string()),
                    }
                }
             Opcode::PrintStr => {
                 if let Some(Value::Int(index)) = self.stack.pop() {
                     if let Some(s) = self.strings.get(index as usize) {
                         println!("{}", s);
                     } else {
                         return Err("Invalid string index".to_string());
                     }
                 } else {
                     return Err("PrintStr expects an integer index on the stack".to_string());
                 }
             }
             Opcode::SetItem => {
                 let index = self.stack.pop().unwrap();
                 let list = self.stack.pop().unwrap();
                 let value = self.stack.pop().unwrap();
                 if let Value::List(mut l) = list {
                     if let Value::Int(i) = index {
                         l[i as usize] = value;
                         self.stack.push(Value::List(l));
                     } else {
                         return Err("Index is not an integer".to_string());
                     }
                 } else {
                     return Err("Can only set item on a list".to_string());
                 }
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
            26 => Ok(Opcode::LoadNone),
            27 => {
                let name_len = self.bytecode[self.pc] as usize;
                self.pc += 1;
                let name = String::from_utf8_lossy(&self.bytecode[self.pc..self.pc + name_len]).to_string();
                self.pc += name_len;
                let arity = self.bytecode[self.pc] as usize;
                self.pc += 1;
                let body_len = self.read_u32()? as usize;
                let body = self.bytecode[self.pc..self.pc + body_len].to_vec();
                self.pc += body_len;
                Ok(Opcode::DefFn(name, arity, body))
            }
             28 => Ok(Opcode::GetItem),
             29 => Ok(Opcode::PrintStr),
             30 => Ok(Opcode::SetItem),
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

    fn read_u32(&mut self) -> Result<u32, String> {
        if self.pc + 4 > self.bytecode.len() {
            return Err("Not enough bytes for u32".to_string());
        }
        let bytes: [u8; 4] = self.bytecode[self.pc..self.pc + 4].try_into().unwrap();
        self.pc += 4;
        Ok(u32::from_le_bytes(bytes))
    }
}