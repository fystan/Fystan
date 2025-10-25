use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift_module::Module;

use crate::ast::{Statement};
use super::expressions::compile_expression;

pub fn compile_statement(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, stmt: Statement, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<(), String> {
    match stmt {
        Statement::Expression(expr_stmt) => {
            compile_expression(builder, variables, expr_stmt.expression, module, print_func_id, malloc_func_id)?;
            Ok(())
        }
        Statement::If(if_stmt) => {
            let condition_value = compile_expression(builder, variables, if_stmt.condition, module, print_func_id, malloc_func_id)?;

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            let merge_block = builder.create_block();

            builder.ins().brif(condition_value, then_block, &[], else_block, &[]);

            builder.switch_to_block(then_block);
            builder.seal_block(then_block);
            for s in if_stmt.consequence.statements {
                compile_statement(builder, variables, s, module, print_func_id, malloc_func_id)?;
            }
            builder.ins().jump(merge_block, &[]);

            builder.switch_to_block(else_block);
            builder.seal_block(else_block);
            if let Some(alternative) = if_stmt.alternative {
                for s in alternative.statements {
                    compile_statement(builder, variables, s, module, print_func_id, malloc_func_id)?;
                }
            }
            builder.ins().jump(merge_block, &[]);

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);

            Ok(())
        }
        Statement::Return(return_stmt) => {
            let return_value = compile_expression(builder, variables, return_stmt.return_value, module, print_func_id, malloc_func_id)?;
            builder.ins().return_(&[return_value]);
            Ok(())
        }
        Statement::Class(class_stmt) => {
            // For now, just compile the body as statements
            for s in class_stmt.body.statements {
                compile_statement(builder, variables, s, module, print_func_id, malloc_func_id)?;
            }
            Ok(())
        }
        _ => Err("Unsupported statement".to_string()),
    }
}
