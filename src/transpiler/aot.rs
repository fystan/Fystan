use cranelift::prelude::*;
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::fs::File;
use std::io::Write;
use std::collections::HashMap;
use crate::target::Target;
use std::mem;

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
}

pub struct AOTCompiler {
    module: ObjectModule,
    ctx: codegen::Context,
    functions: HashMap<String, codegen::ir::FuncRef>,
}

impl AOTCompiler {
    pub fn new(target: &Target) -> Self {
        let triple = target.to_c_triple();
        let mut shared_flags_builder = settings::builder();
        shared_flags_builder.set("opt_level", "speed_and_size").unwrap();
        let shared_flags = settings::Flags::new(shared_flags_builder);

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(shared_flags).unwrap();

        let builder = ObjectBuilder::new(
            isa,
            "fystan_executable",
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let mut module = ObjectModule::new(builder);
        let ctx = module.make_context();

        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        module.declare_function("print", Linkage::Import, &sig).unwrap();

        Self {
            module,
            ctx,
            functions: HashMap::new(),
        }
    }

    pub fn compile_and_save_executable(
        &mut self,
        bytecode: &[u8],
        output_path: &str,
        target: &Target,
    ) -> Result<(), String> {
        let opcodes = self.deserialize_bytecode(bytecode)?;

        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));
        let func_id = self.module
            .declare_function("main", Linkage::Export, &sig)
            .map_err(|e| e.to_string())?;

        self.ctx.func.signature = sig;

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut builder_context);

        let mut blocks: HashMap<usize, Block> = HashMap::new();
        let mut stack: Vec<Value> = Vec::new();
        let mut variables: HashMap<String, Variable> = HashMap::new();

        let entry_block = builder.create_block();
        blocks.insert(0, entry_block);

        for (i, opcode) in opcodes.iter().enumerate() {
            if let Opcode::Jump(addr) = opcode {
                if !blocks.contains_key(addr) {
                    let block = builder.create_block();
                    blocks.insert(*addr, block);
                }
            }
            if let Opcode::JumpIfFalse(addr) = opcode {
                if !blocks.contains_key(addr) {
                    let block = builder.create_block();
                    blocks.insert(*addr, block);
                }
            }
        }

        let mut i = 0;
        while i < opcodes.len() {
            let pc = i;
            let opcode = &opcodes[i];

            if !blocks.contains_key(&pc) {
                i += 1;
                continue;
            }
            builder.switch_to_block(blocks[&pc]);

            translate_opcode(&mut builder, &mut self.module, &mut stack, &mut blocks, &mut variables, opcode, pc)?;
            i += 1;
        }

        builder.seal_all_blocks();
        builder.finalize();

        self.module.define_function(func_id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);

        let new_module = {
            let triple = target.to_c_triple();
            let mut shared_flags_builder = settings::builder();
            shared_flags_builder.set("opt_level", "speed_and_size").unwrap();
            let shared_flags = settings::Flags::new(shared_flags_builder);
    
            let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
                panic!("host machine is not supported: {}", msg);
            });
            let isa = isa_builder.finish(shared_flags).unwrap();
    
            let builder = ObjectBuilder::new(
                isa,
                "fystan_executable",
                cranelift_module::default_libcall_names(),
            )
            .unwrap();
            ObjectModule::new(builder)
        };

        let old_module = mem::replace(&mut self.module, new_module);
        let product = old_module.finish();
        let obj_bytes = product.emit().map_err(|e| e.to_string())?;

        let mut file = File::create(output_path).map_err(|e| e.to_string())?;
        file.write_all(&obj_bytes).map_err(|e| e.to_string())?;

        println!("AOT mode: Executable object file saved to {}", output_path);
        Ok(())
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
                1 => {
                    let mut bytes = [0; 8];
                    bytes.copy_from_slice(&bytecode[i..i + 8]);
                    opcodes.push(Opcode::LoadFloat(f64::from_le_bytes(bytes)));
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
                23 => opcodes.push(Opcode::Halt),
                _ => return Err(format!("Unknown opcode: {}", opcode)),
            }
        }
        Ok(opcodes)
    }
}

fn translate_opcode(builder: &mut FunctionBuilder, module: &mut ObjectModule, stack: &mut Vec<Value>, blocks: &mut HashMap<usize, Block>, variables: &mut HashMap<String, Variable>, opcode: &Opcode, pc: usize) -> Result<(), String> {
    match opcode {
        Opcode::LoadConst(val) => {
            let const_val = builder.ins().iconst(types::I64, *val);
            stack.push(const_val);
        }
        Opcode::LoadFloat(val) => {
            let const_val = builder.ins().f64const(*val);
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
            let res = builder.ins().ineg(val);
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
        Opcode::Jump(addr) => {
            let block = blocks[addr];
            builder.ins().jump(block, &[]);
        }
        Opcode::JumpIfFalse(addr) => {
            let val = stack.pop().unwrap();
            let true_block = blocks[&(pc + 1)];
            let false_block = blocks[addr];
            builder.ins().brif(val, false_block, &[], true_block, &[]);
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
        Opcode::PrintStr => todo!(),
        Opcode::LoadNone => todo!(),
        Opcode::DefFn(_, _, _) => todo!(),
        Opcode::GetItem => todo!(),
        Opcode::SetItem => todo!(),
        Opcode::SetupExcept(_) => todo!(),
        Opcode::PopExcept => todo!(),
        Opcode::Raise => todo!(),
        Opcode::BuildList(_) => todo!(),
        _ => return Err(format!("Opcode not implemented: {:?}", opcode)),
    }
    Ok(())
}
