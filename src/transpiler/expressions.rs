use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift_module::{Module, Linkage};

use crate::ast::{ExpressionEnum, InfixOperator};



fn compile_infix_expression(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, infix_expr: crate::ast::InfixExpression, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<Value, String> {
    if infix_expr.operator == InfixOperator::Assign {
        let val = compile_expression(builder, variables, *infix_expr.right, module, print_func_id, malloc_func_id)?;
        if let ExpressionEnum::Identifier(ident) = *infix_expr.left {
            let val_type = builder.func.dfg.value_type(val);
            let new_var_index = variables.len();
            let variable = variables.entry(ident.value.clone()).or_insert_with(|| {
                let var = Variable::new(new_var_index);
                builder.declare_var(var, val_type);
                (var, val_type)
            });
            builder.def_var(variable.0, val);
            return Ok(val);
        } else if let ExpressionEnum::IndexExpression(index_expr) = *infix_expr.left {
            let list_ptr = compile_expression(builder, variables, *index_expr.left, module, print_func_id, malloc_func_id)?;
            let index = compile_expression(builder, variables, *index_expr.index, module, print_func_id, malloc_func_id)?;

            let data_ptr = builder.ins().load(types::I64, MemFlags::new(), list_ptr, 16);
            let eight = builder.ins().iconst(types::I64, 8);
            let offset = builder.ins().imul(index, eight);
            let addr = builder.ins().iadd(data_ptr, offset);
            builder.ins().store(MemFlags::new(), val, addr, 0);
            return Ok(val);
        } else {
            return Err("Invalid assignment target".to_string());
        }
    }

    let left = compile_expression(builder, variables, *infix_expr.left, module, print_func_id, malloc_func_id)?;
    let right = compile_expression(builder, variables, *infix_expr.right, module, print_func_id, malloc_func_id)?;

    match infix_expr.operator {
        InfixOperator::Plus => Ok(builder.ins().iadd(left, right)),
        InfixOperator::Minus => Ok(builder.ins().isub(left, right)),
        InfixOperator::Multiply => Ok(builder.ins().imul(left, right)),
        InfixOperator::Divide => Ok(builder.ins().sdiv(left, right)),
        InfixOperator::Eq => Ok(builder.ins().icmp(IntCC::Equal, left, right)),
        InfixOperator::NotEq => Ok(builder.ins().icmp(IntCC::NotEqual, left, right)),
        InfixOperator::Lt => Ok(builder.ins().icmp(IntCC::SignedLessThan, left, right)),
        InfixOperator::Gt => Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left, right)),
        _ => Err("Unsupported infix operator".to_string()),
    }
}

fn compile_call_expression(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, call_expr: crate::ast::CallExpression, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<Value, String> {
    if let ExpressionEnum::Identifier(ident) = *call_expr.function {
        if ident.value == "print" {
            if call_expr.arguments.len() != 1 {
                return Err("print() takes exactly one argument".to_string());
            }
            let arg = compile_expression(builder, variables, call_expr.arguments[0].clone(), module, print_func_id, malloc_func_id)?;
            let local_callee = module.declare_func_in_func(print_func_id, &mut builder.func);
            builder.ins().call(local_callee, &[arg]);
            return Ok(arg);
        }
    }
    Err("Unsupported function call".to_string())
}

fn compile_array_literal(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, arr_lit: crate::ast::ArrayLiteral, module: &mut dyn Module, malloc_func_id: cranelift_module::FuncId, print_func_id: cranelift_module::FuncId) -> Result<Value, String> {
    let local_malloc_func = module.declare_func_in_func(malloc_func_id, &mut builder.func);

    let num_elements = arr_lit.elements.len();
    let struct_size = builder.ins().iconst(types::I64, 24);
    let list_ptr_call = builder.ins().call(local_malloc_func, &[struct_size]);
    let list_ptr = builder.inst_results(list_ptr_call)[0];

    let data_size_val = builder.ins().iconst(types::I64, (num_elements as i64) * 8);
    let data_ptr_call = builder.ins().call(local_malloc_func, &[data_size_val]);
    let data_ptr = builder.inst_results(data_ptr_call)[0];

    let size_val = builder.ins().iconst(types::I64, num_elements as i64);
    builder.ins().store(MemFlags::new(), size_val, list_ptr, 0);
    builder.ins().store(MemFlags::new(), size_val, list_ptr, 8);
    builder.ins().store(MemFlags::new(), data_ptr, list_ptr, 16);

    for (i, elem_expr) in arr_lit.elements.iter().enumerate() {
        let element_val = compile_expression(builder, variables, elem_expr.clone(), module, print_func_id, malloc_func_id)?;
        let offset = builder.ins().iconst(types::I64, (i as i64) * 8);
        let addr = builder.ins().iadd(data_ptr, offset);
        builder.ins().store(MemFlags::new(), element_val, addr, 0);
    }

    Ok(list_ptr)
}

fn compile_index_expression(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, index_expr: crate::ast::IndexExpression, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<Value, String> {
    let list_ptr = compile_expression(builder, variables, *index_expr.left, module, print_func_id, malloc_func_id)?;
    let index = compile_expression(builder, variables, *index_expr.index, module, print_func_id, malloc_func_id)?;

    let data_ptr = builder.ins().load(types::I64, MemFlags::new(), list_ptr, 16);
    let eight = builder.ins().iconst(types::I64, 8);
    let offset = builder.ins().imul(index, eight);
    let addr = builder.ins().iadd(data_ptr, offset);
    let value = builder.ins().load(types::I64, MemFlags::new(), addr, 0);
    Ok(value)
}

pub fn compile_expression(builder: &mut FunctionBuilder, variables: &mut HashMap<String, (Variable, types::Type)>, expr: ExpressionEnum, module: &mut dyn Module, print_func_id: cranelift_module::FuncId, malloc_func_id: cranelift_module::FuncId) -> Result<Value, String> {
    match expr {
        ExpressionEnum::IntegerLiteral(lit) => {
            Ok(builder.ins().iconst(types::I64, lit.value))
        }
        ExpressionEnum::StringLiteral(lit) => {
            let mut data = lit.value.as_bytes().to_vec();
            data.push(0); // Null terminator
            let data_id = module.declare_data(&format!(".str.{}", lit.value), Linkage::Local, true, false).unwrap();
            module.define_data(data_id, &data).unwrap();
            let local_id = module.declare_data_in_func(data_id, &mut builder.func);
            Ok(builder.ins().global_value(types::I64, local_id))
        }
        ExpressionEnum::Boolean(b) => {
            Ok(builder.ins().iconst(types::I8, if b.value { 1 } else { 0 }))
        }
        ExpressionEnum::Infix(infix_expr) => compile_infix_expression(builder, variables, infix_expr, module, print_func_id, malloc_func_id),
        ExpressionEnum::Call(call_expr) => compile_call_expression(builder, variables, call_expr, module, print_func_id, malloc_func_id),
        ExpressionEnum::ArrayLiteral(arr_lit) => compile_array_literal(builder, variables, arr_lit, module, malloc_func_id, print_func_id),
        ExpressionEnum::IndexExpression(index_expr) => compile_index_expression(builder, variables, index_expr, module, print_func_id, malloc_func_id),
        ExpressionEnum::Identifier(ident) => {
            if let Some((var, _)) = variables.get(&ident.value) {
                Ok(builder.use_var(*var))
            } else {
                Err(format!("Undefined variable: {}", ident.value))
            }
        }
        _ => Err("Unsupported expression".to_string()),
    }
}
