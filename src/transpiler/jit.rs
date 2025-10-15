use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::transpiler::string_allocator::StringAllocator;

thread_local! {
    static OUTPUT_BUFFER: RefCell<Option<Rc<RefCell<String>>>> = RefCell::new(None);
    static STRING_ALLOCATOR: RefCell<Option<Rc<StringAllocator>>> = RefCell::new(None);
}

#[derive(Debug, Clone)]
pub enum Opcode {
    LoadConst(i64),
    LoadFloat(f64),
    LoadString(usize),
    LoadBool(bool),
    LoadVar(String),
    StoreVar(String),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    And,
    Or,
    Not,
    Neg,
    Jump(usize),
    JumpIfFalse(usize),
    Call(String, usize), // Function name and arg count
    Return,
    Print,
    PrintStr,
    Halt,
    BuildList(usize),
    LoadNone,
    DefFn(String, usize, Vec<u8>),
    GetItem,
    SetItem,
}

pub struct JITCompiler {
    module: JITModule,
    ctx: codegen::Context,
    functions: HashMap<String, codegen::ir::FuncRef>,
}

fn print(val: i64) -> i32 {
    OUTPUT_BUFFER.with(|buffer| {
        if let Some(buf_rc) = buffer.borrow().as_ref() {
            buf_rc.borrow_mut().push_str(&format!("{}
", val));
        } else {
            println!("{}", val);
        }
    });
    0
}

fn print_str(index: i64) -> i32 {
    STRING_ALLOCATOR.with(|allocator| {
        if let Some(alloc) = allocator.borrow().as_ref() {
            let s = (**alloc).get(index as usize).unwrap_or("ERROR: Invalid string index");
            OUTPUT_BUFFER.with(|buffer| {
                if let Some(buf_rc) = buffer.borrow().as_ref() {
                    buf_rc.borrow_mut().push_str(&format!("{}\n", s));
                }
            });
        }
    });
    0
}

impl JITCompiler {
    pub fn new() -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())
            .expect("Failed to create JITBuilder");
        builder.symbol("print", print as *const u8);
        builder.symbol("print_str", print_str as *const u8);

        let mut module = JITModule::new(builder);
        let ctx = module.make_context();

        // Signature for print(i64)
        let mut sig_i64 = module.make_signature();
        sig_i64.params.push(AbiParam::new(types::I64));
        sig_i64.returns.push(AbiParam::new(types::I32));
        module
            .declare_function("print", Linkage::Import, &sig_i64)
            .unwrap();

        // Signature for print_str(i64)
        let mut sig_str = module.make_signature();
        sig_str.params.push(AbiParam::new(types::I64));
        sig_str.returns.push(AbiParam::new(types::I32));
        module
            .declare_function("print_str", Linkage::Import, &sig_str)
            .unwrap();


        Self {
            module,
            ctx,
            functions: HashMap::new(),
        }
    }

    pub fn set_output_buffer(buffer: Rc<RefCell<String>>) {
        OUTPUT_BUFFER.with(|buf| {
            *buf.borrow_mut() = Some(buffer);
        });
    }

    pub fn clear_output_buffer() {
        OUTPUT_BUFFER.with(|buf| {
            *buf.borrow_mut() = None;
        });
    }

    pub fn set_string_allocator(allocator: Rc<StringAllocator>) {
        STRING_ALLOCATOR.with(|alloc| {
            *alloc.borrow_mut() = Some(allocator);
        });
    }

    pub fn clear_string_allocator() {
        STRING_ALLOCATOR.with(|alloc| {
            *alloc.borrow_mut() = None;
        });
    }

    pub fn compile_and_run(&mut self, bytecode: &[u8], string_allocator: &StringAllocator) -> Result<i32, String> {
        let opcodes = self.deserialize_bytecode(bytecode)?;

        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));
        let func_id = self.module
            .declare_function("main", Linkage::Local, &sig)
            .map_err(|e| e.to_string())?;

        self.ctx.func.signature = sig;

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut builder_context);

        let mut blocks: HashMap<usize, Block> = HashMap::new();
        let mut stack: Vec<Value> = Vec::new();
        let mut variables: HashMap<String, Variable> = HashMap::new();

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        blocks.insert(0, entry_block);

        // Create blocks for all other potential instruction addresses,
        // including one past the end which can be a jump target.
        for i in 1..=opcodes.len() {
            let block = builder.create_block();
            blocks.insert(i, block);
        }

        let mut i = 0;
        while i < opcodes.len() {
            let pc = i;
            let opcode = &opcodes[i];

            builder.switch_to_block(blocks[&pc]);

            translate_opcode(&mut builder, &mut self.module, &mut stack, &mut blocks, &mut variables, opcode, pc)?;

            let is_terminator = matches!(opcode, Opcode::Jump(_) | Opcode::JumpIfFalse(_) | Opcode::Return | Opcode::Halt);
            if !is_terminator && (pc + 1) < opcodes.len() {
                let next_block = blocks[&(pc + 1)];
                builder.ins().jump(next_block, &[]);
            }
            
            i += 1;
        }

        builder.seal_all_blocks();
        builder.finalize();

        self.module.define_function(func_id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().map_err(|e| e.to_string())?;

        let code = self.module.get_finalized_function(func_id);
        let code_fn = unsafe { std::mem::transmute::<_, fn() -> i32>(code) };

        Ok(code_fn())
    }

    fn deserialize_bytecode(&self, bytecode: &[u8]) -> Result<Vec<Opcode>, String> {
        let mut opcodes = Vec::new();
        let mut i = 0;
        while i < bytecode.len() {
            let opcode = bytecode[i];
            i += 1;
            match opcode {
                0 => {
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::LoadConst(i64::from_le_bytes(bytes)));
                    i += 8;
                }
                1 => { // LoadString
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::LoadString(i64::from_le_bytes(bytes) as usize));
                    i += 8;
                }
                2 => {
                    opcodes.push(Opcode::LoadBool(bytecode[i] != 0));
                    i += 1;
                }
                3 => {
                    let len = bytecode[i] as usize;
                    i += 1;
                    let name = String::from_utf8_lossy(&bytecode[i..i + len]).to_string();
                    opcodes.push(Opcode::LoadVar(name));
                    i += len;
                }
                4 => {
                    let len = bytecode[i] as usize;
                    i += 1;
                    let name = String::from_utf8_lossy(&bytecode[i..i + len]).to_string();
                    opcodes.push(Opcode::StoreVar(name));
                    i += len;
                }
                5 => opcodes.push(Opcode::Add),
                6 => opcodes.push(Opcode::Sub),
                7 => opcodes.push(Opcode::Mul),
                8 => opcodes.push(Opcode::Div),
                9 => opcodes.push(Opcode::Mod),
                10 => opcodes.push(Opcode::Eq),
                11 => opcodes.push(Opcode::NotEq),
                12 => opcodes.push(Opcode::Lt),
                13 => opcodes.push(Opcode::Gt),
                14 => opcodes.push(Opcode::And),
                15 => opcodes.push(Opcode::Or),
                16 => opcodes.push(Opcode::Not),
                17 => opcodes.push(Opcode::Neg),
                18 => {
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::Jump(i64::from_le_bytes(bytes) as usize));
                    i += 8;
                }
                19 => {
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::JumpIfFalse(i64::from_le_bytes(bytes) as usize));
                    i += 8;
                }
                20 => {
                    let len = bytecode[i] as usize;
                    i += 1;
                    let name = String::from_utf8_lossy(&bytecode[i..i + len]).to_string();
                    i += len;
                    let argc = bytecode[i] as usize;
                    i += 1;
                    opcodes.push(Opcode::Call(name, argc));
                }
                21 => opcodes.push(Opcode::Return),
                22 => opcodes.push(Opcode::Print),
                23 => opcodes.push(Opcode::Halt),
                24 => { // LoadFloat
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::LoadFloat(f64::from_le_bytes(bytes)));
                    i += 8;
                }
                25 => { // BuildList
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::BuildList(i64::from_le_bytes(bytes) as usize));
                    i += 8;
                }
                26 => opcodes.push(Opcode::LoadNone),
                27 => { // DefFn
                    let name_len = bytecode[i] as usize;
                    i += 1;
                    let name = String::from_utf8_lossy(&bytecode[i..i + name_len]).to_string();
                    i += name_len;
                    let arity = bytecode[i] as usize;
                    i += 1;
                    let mut body_len_bytes = [0; 4];
                    body_len_bytes.copy_from_slice(&bytecode[i..i + 4]);
                    let body_len = u32::from_le_bytes(body_len_bytes) as usize;
                    i += 4;
                    let body = bytecode[i..i + body_len].to_vec();
                    i += body_len;
                    opcodes.push(Opcode::DefFn(name, arity, body));
                }
                28 => opcodes.push(Opcode::GetItem),
                29 => opcodes.push(Opcode::PrintStr),
                30 => opcodes.push(Opcode::SetItem),
                _ => return Err(format!("Unknown opcode: {}", opcode)),
            }
        }
        Ok(opcodes)
    }
}

fn translate_opcode(builder: &mut FunctionBuilder, module: &mut JITModule, stack: &mut Vec<Value>, blocks: &mut HashMap<usize, Block>, variables: &mut HashMap<String, Variable>, opcode: &Opcode, pc: usize) -> Result<(), String> {
    match opcode {
        Opcode::LoadConst(val) => {
            let const_val = builder.ins().iconst(types::I64, *val);
            stack.push(const_val);
        }
        Opcode::LoadFloat(val) => {
            let const_val = builder.ins().f64const(*val);
            stack.push(const_val);
        }
        Opcode::LoadString(idx) => {
            let const_val = builder.ins().iconst(types::I64, *idx as i64);
            stack.push(const_val);
        }
        Opcode::LoadBool(val) => {
            let const_val = builder.ins().iconst(types::I8, if *val { 1 } else { 0 });
            stack.push(const_val);
        }
        Opcode::LoadVar(name) => {
            let var = variables.get(name).unwrap();
            let val = builder.use_var(*var);
            stack.push(val);
        }
        Opcode::StoreVar(name) => {
            let val = stack.pop().unwrap();
            let len = variables.len();
            let var = variables.entry(name.clone()).or_insert_with(|| {
                let var = Variable::new(len);
                builder.declare_var(var, types::I64);
                var
            });
            builder.def_var(*var, val);
        }
        Opcode::Add => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().iadd(left, right);
            stack.push(res);
        }
        Opcode::Sub => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().isub(left, right);
            stack.push(res);
        }
        Opcode::Mul => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().imul(left, right);
            stack.push(res);
        }
        Opcode::Div => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().sdiv(left, right);
            stack.push(res);
        }
        Opcode::Mod => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().srem(left, right);
            stack.push(res);
        }
        Opcode::Eq => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().icmp(IntCC::Equal, left, right);
            stack.push(res);
        }
        Opcode::NotEq => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().icmp(IntCC::NotEqual, left, right);
            stack.push(res);
        }
        Opcode::Lt => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().icmp(IntCC::SignedLessThan, left, right);
            stack.push(res);
        }
        Opcode::Gt => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
            stack.push(res);
        }
        Opcode::And => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().band(left, right);
            stack.push(res);
        }
        Opcode::Or => {
            let right = stack.pop().unwrap();
            let left = stack.pop().unwrap();
            let res = builder.ins().bor(left, right);
            stack.push(res);
        }
        Opcode::Not => {
            let val = stack.pop().unwrap();
            let res = builder.ins().bnot(val);
            stack.push(res);
        }
        Opcode::Neg => {
            let val = stack.pop().unwrap();
            let res = builder.ins().fneg(val);
            stack.push(res);
        }
        Opcode::Call(name, argc) => {
            if name == "print" {
                let mut sig = module.make_signature();
                sig.params.push(AbiParam::new(types::I64));
                sig.returns.push(AbiParam::new(types::I32));
                let func = module.declare_function("print", Linkage::Import, &sig).unwrap();
                let local_callee = module.declare_func_in_func(func, &mut builder.func);

                let mut args = Vec::new();
                for _ in 0..*argc {
                    args.push(stack.pop().unwrap());
                }
                builder.ins().call(local_callee, &args);
            }
        }
        Opcode::Print => {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I32));
            let func = module.declare_function("print", Linkage::Import, &sig).unwrap();
            let local_callee = module.declare_func_in_func(func, &mut builder.func);
            let arg = stack.pop().unwrap();
            builder.ins().call(local_callee, &[arg]);
        }
        Opcode::PrintStr => {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I32));
            let func = module.declare_function("print_str", Linkage::Import, &sig).unwrap();
            let local_callee = module.declare_func_in_func(func, &mut builder.func);
            let arg = stack.pop().unwrap();
            builder.ins().call(local_callee, &[arg]);
        }
        Opcode::Jump(addr) => {
            let block = blocks[addr];
            builder.ins().jump(block, &[]);
        }
        Opcode::JumpIfFalse(addr) => {
            let val = stack.pop().unwrap();
            let true_block = blocks[&(pc + 1)];
            let false_block = blocks[addr];
            builder.ins().brif(val, true_block, &[], false_block, &[]);
        }
        Opcode::Return => {
            let val = stack.pop().unwrap();
            let val_32 = builder.ins().ireduce(types::I32, val);
            builder.ins().return_(&[val_32]);
        }
        Opcode::Halt => {
            let zero = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[zero]);
        }
        Opcode::LoadNone => {
            unimplemented!();
        }
        Opcode::DefFn(_, _, _) => {
            unimplemented!();
        }
        Opcode::GetItem => {
            unimplemented!();
        }
        Opcode::SetItem => {
            unimplemented!();
        }
        Opcode::BuildList(_) => {
            unimplemented!();
        }
    }
    Ok(())
}
