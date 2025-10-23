use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift_module::Module;

use crate::ast::{Statement, ExpressionEnum};
use super::expressions::compile_expression;

pub fn compile_statement(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, stmt: Statement, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<(), String> {
    match stmt {
        Statement::Expression(expr_stmt) => {
            compile_expression(builder, variables, expr_stmt.expression, module, print_func_id, malloc_func_id)?;
            Ok(())
        }
        _ => Err("Unsupported statement".to_string()),
    }
}
