use cranelift::prelude::*;
use cranelift_module::{Module, Linkage};
use std::collections::HashMap;

use crate::ast::Program;
use super::context::{Compiler, CompilationResult, CompilerModule};
use super::statements::compile_statement;

impl Compiler {
    pub fn compile(mut self, program: Program) -> Result<CompilationResult, String> {
        let (func_id, module_ref, sig) = match &mut self.module {
            CompilerModule::AOT(m) => {
                let mut sig = m.make_signature();
                sig.returns.push(AbiParam::new(types::I32));
                let id = m.declare_function("main", Linkage::Export, &sig).map_err(|e| e.to_string())?;
                (id, m as &mut dyn Module, sig)
            }
            CompilerModule::JIT(m) => {
                let mut sig = m.make_signature();
                sig.returns.push(AbiParam::new(types::I32));
                let id = m.declare_function("main", Linkage::Export, &sig).map_err(|e| e.to_string())?;
                (id, m as &mut dyn Module, sig)
            }
        };

        self.ctx.func.signature = sig;

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        let mut variables = self.variables;

        for stmt in program.statements {
            compile_statement(&mut builder, &mut variables, stmt, module_ref, self.print_func_id, self.malloc_func_id)?;
        }

        // Finalize the function
        let zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero]);
        builder.seal_all_blocks();
        builder.finalize();

        module_ref.define_function(func_id, &mut self.ctx).map_err(|e| e.to_string())?;
        module_ref.clear_context(&mut self.ctx);

        match self.module {
            CompilerModule::AOT(m) => Ok(CompilationResult::AOT(m)),
            CompilerModule::JIT(mut m) => {
                // For JIT, finalize and run
                m.finalize_definitions().map_err(|e| e.to_string())?;
                let main_func = m.get_finalized_function(func_id);
                let main_ptr = unsafe { std::mem::transmute::<_, fn() -> i32>(main_func) };
                let result = main_ptr();
                Ok(CompilationResult::JIT(result))
            }
        }
    }
}
