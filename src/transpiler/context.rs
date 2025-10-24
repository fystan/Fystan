use cranelift::prelude::*;
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_jit::{JITBuilder, JITModule};
use std::collections::HashMap;


use crate::target::Target;
use crate::builtins;

#[derive(Clone)]
pub enum CompilationMode {
    AOT,
    JIT,
}

pub enum CompilationResult {
    AOT(ObjectModule),
    JIT(i32), // Return value from JIT execution
}

pub enum CompilerModule {
    AOT(ObjectModule),
    JIT(JITModule),
}

pub struct Compiler {
    pub module: CompilerModule,
    pub ctx: codegen::Context,
    pub variables: HashMap<String, (Variable, types::Type)>,
    pub print_func_id: cranelift_module::FuncId,
    pub malloc_func_id: cranelift_module::FuncId,
}

impl Compiler {
    pub fn new(_target: &Target, mode: CompilationMode) -> Self {
        let mut shared_flags_builder = settings::builder();
        shared_flags_builder.set("opt_level", "speed_and_size").unwrap();
        shared_flags_builder.set("enable_verifier", "true").unwrap();
        let shared_flags = settings::Flags::new(shared_flags_builder);

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(shared_flags).unwrap();

        let (mut module, ctx) = match mode {
            CompilationMode::AOT => {
                let builder = ObjectBuilder::new(
                    isa,
                    "fystan_executable",
                    cranelift_module::default_libcall_names(),
                ).unwrap();
                let m = ObjectModule::new(builder);
                let c = m.make_context();
                (CompilerModule::AOT(m), c)
            }
            CompilationMode::JIT => {
                let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
                builder.symbol("puts", builtins::puts as *const u8);
                let m = JITModule::new(builder);
                let c = m.make_context();
                (CompilerModule::JIT(m), c)
            }
        };

        // Declare 'puts' function
        let (print_func_id, malloc_func_id) = match &mut module {
            CompilerModule::AOT(m) => {
                let mut sig_print = m.make_signature();
                sig_print.params.push(AbiParam::new(types::I64));
                sig_print.returns.push(AbiParam::new(types::I32));
                let print_id = m.declare_function("puts", Linkage::Import, &sig_print).unwrap();

                let mut sig_malloc = m.make_signature();
                sig_malloc.params.push(AbiParam::new(types::I64));
                sig_malloc.returns.push(AbiParam::new(types::I64));
                let malloc_id = m.declare_function("malloc", Linkage::Import, &sig_malloc).unwrap();
                (print_id, malloc_id)
            }
            CompilerModule::JIT(m) => {
                let mut sig_print = m.make_signature();
                sig_print.params.push(AbiParam::new(types::I64));
                sig_print.returns.push(AbiParam::new(types::I32));
                let print_id = m.declare_function("puts", Linkage::Import, &sig_print).unwrap();

                let mut sig_malloc = m.make_signature();
                sig_malloc.params.push(AbiParam::new(types::I64));
                sig_malloc.returns.push(AbiParam::new(types::I64));
                let malloc_id = m.declare_function("malloc", Linkage::Import, &sig_malloc).unwrap();
                (print_id, malloc_id)
            }
        };

        Self {
            module,
            ctx,
            variables: HashMap::new(),
            print_func_id,
            malloc_func_id,
        }
    }
}
