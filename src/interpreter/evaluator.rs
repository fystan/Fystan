use crate::ast::*;
use crate::transpiler::string_allocator::StringAllocator;
use std::collections::HashMap;

pub struct Evaluator {
    variables: HashMap<String, Value>,
    string_allocator: StringAllocator,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Str(usize), // Index into string arena
    Bool(bool),
    Void,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            string_allocator: StringAllocator::new(),
        }
    }

    pub fn evaluate(&mut self, program: Program) -> Result<(), String> {
        for stmt in program.statements {
            self.evaluate_statement(stmt)?;
        }
        Ok(())
    }

    fn evaluate_statement(&mut self, stmt: Statement) -> Result<(), String> {
        match stmt {
            Statement::Expression(expr_stmt) => {
                self.evaluate_expression_enum(expr_stmt.expression)?;
                Ok(())
            }
            _ => Ok(()), // For now, handle basic cases
        }
    }

    fn evaluate_expression_enum(&mut self, expr: ExpressionEnum) -> Result<Value, String> {
        match expr {
            ExpressionEnum::IntegerLiteral(int_lit) => Ok(Value::Int(int_lit.value)),
            ExpressionEnum::StringLiteral(str_lit) => {
                let index = self.string_allocator.allocate(&str_lit.value);
                Ok(Value::Str(index))
            }
            ExpressionEnum::Boolean(bool_lit) => Ok(Value::Bool(bool_lit.value)),
            ExpressionEnum::Identifier(id) => {
                if let Some(val) = self.variables.get(&id.value) {
                    Ok(val.clone())
                } else {
                    Err(format!("Undefined variable: {}", id.value))
                }
            }
            ExpressionEnum::Infix(infix) => {
                let left_val = self.evaluate_expression_enum(*infix.left)?;
                let right_val = self.evaluate_expression_enum(*infix.right)?;
                match (left_val, right_val) {
                    (Value::Int(l), Value::Int(r)) => match infix.operator {
                        InfixOperator::Plus => Ok(Value::Int(l + r)),
                        InfixOperator::Minus => Ok(Value::Int(l - r)),
                        InfixOperator::Multiply => Ok(Value::Int(l * r)),
                        InfixOperator::Divide => Ok(Value::Int(l / r)),
                        _ => Err("Unsupported operation".to_string()),
                    },
                    _ => Err("Type mismatch".to_string()),
                }
            }
            _ => Err("Unsupported expression".to_string()),
        }
    }
}