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
    Float(f64),
    Str(usize), // Index into string arena
    Bool(bool),
    List(Vec<Value>),
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
            Statement::Return(ret_stmt) => {
                self.evaluate_expression_enum(ret_stmt.return_value)?;
                Ok(())
            }
            _ => Ok(()), // For now, handle basic cases
        }
    }

    fn evaluate_block(&mut self, block: BlockStatement) -> Result<Value, String> {
        for stmt in block.statements {
            self.evaluate_statement(stmt)?;
        }
        Ok(Value::Void)
    }

    fn print_value(&self, val: &Value) {
        match val {
            Value::Int(i) => print!("{}", i),
            Value::Float(f) => print!("{}", f),
            Value::Str(idx) => {
                if let Some(s) = self.string_allocator.get_strings().get(*idx) {
                    print!("{}", s);
                }
            }
            Value::Bool(b) => print!("{}", b),
            Value::List(_) => print!("[List]"),
            Value::Void => {},
        }
    }

    fn evaluate_expression_enum(&mut self, expr: ExpressionEnum) -> Result<Value, String> {
        match expr {
            ExpressionEnum::IntegerLiteral(int_lit) => Ok(Value::Int(int_lit.value)),
            ExpressionEnum::FloatLiteral(float_lit) => Ok(Value::Float(float_lit.value)),
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
                if infix.operator == InfixOperator::Assign {
                    if let ExpressionEnum::Identifier(ident) = *infix.left {
                        let right_val = self.evaluate_expression_enum(*infix.right)?;
                        self.variables.insert(ident.value, right_val.clone());
                        Ok(right_val)
                    } else {
                        Err("Invalid assignment target".to_string())
                    }
                } else {
                    let left_val = self.evaluate_expression_enum(*infix.left)?;
                    let right_val = self.evaluate_expression_enum(*infix.right)?;
                    match (left_val, right_val) {
                        (Value::Int(l), Value::Int(r)) => match infix.operator {
                            InfixOperator::Plus => Ok(Value::Int(l + r)),
                            InfixOperator::Minus => Ok(Value::Int(l - r)),
                            InfixOperator::Multiply => Ok(Value::Int(l * r)),
                            InfixOperator::Divide => Ok(Value::Int(l / r)),
                            InfixOperator::Mod => Ok(Value::Int(l % r)),
                            InfixOperator::Eq => Ok(Value::Bool(l == r)),
                            InfixOperator::NotEq => Ok(Value::Bool(l != r)),
                            InfixOperator::Lt => Ok(Value::Bool(l < r)),
                            InfixOperator::Gt => Ok(Value::Bool(l > r)),
                            _ => Err("Unsupported operation".to_string()),
                        },
                        (Value::Float(l), Value::Float(r)) => match infix.operator {
                            InfixOperator::Plus => Ok(Value::Float(l + r)),
                            InfixOperator::Minus => Ok(Value::Float(l - r)),
                            InfixOperator::Multiply => Ok(Value::Float(l * r)),
                            InfixOperator::Divide => Ok(Value::Float(l / r)),
                            InfixOperator::Eq => Ok(Value::Bool(l == r)),
                            InfixOperator::NotEq => Ok(Value::Bool(l != r)),
                            InfixOperator::Lt => Ok(Value::Bool(l < r)),
                            InfixOperator::Gt => Ok(Value::Bool(l > r)),
                            _ => Err("Unsupported operation".to_string()),
                        },
                        (Value::Bool(l), Value::Bool(r)) => match infix.operator {
                            InfixOperator::And => Ok(Value::Bool(l && r)),
                            InfixOperator::Or => Ok(Value::Bool(l || r)),
                            InfixOperator::Eq => Ok(Value::Bool(l == r)),
                            InfixOperator::NotEq => Ok(Value::Bool(l != r)),
                            _ => Err("Unsupported operation".to_string()),
                        },
                        _ => Err("Type mismatch".to_string()),
                    }
                }
            }
            ExpressionEnum::Prefix(prefix) => {
                let val = self.evaluate_expression_enum(*prefix.right)?;
                match prefix.operator {
                    PrefixOperator::Not => match val {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err("Type mismatch".to_string()),
                    },
                    PrefixOperator::Minus => match val {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        Value::Float(f) => Ok(Value::Float(-f)),
                        _ => Err("Type mismatch".to_string()),
                    },
                }
            }
            ExpressionEnum::Call(call) => {
                if let ExpressionEnum::Identifier(ident) = *call.function {
                    if ident.value == "print" {
                        for arg in &call.arguments {
                            let val = self.evaluate_expression_enum(arg.clone())?;
                            self.print_value(&val);
                        }
                        println!();
                        Ok(Value::Void)
                    } else {
                        Err("Unsupported function call".to_string())
                    }
                } else {
                    Err("Unsupported function call".to_string())
                }
            }
            ExpressionEnum::If(if_expr) => {
                let cond = self.evaluate_expression_enum(*if_expr.condition)?;
                if let Value::Bool(true) = cond {
                    self.evaluate_block(if_expr.consequence)
                } else if let Some(alt) = if_expr.alternative {
                    self.evaluate_block(alt)
                } else {
                    Ok(Value::Void)
                }
            }
            ExpressionEnum::While(while_expr) => {
                loop {
                    let cond = self.evaluate_expression_enum(*while_expr.condition.clone())?;
                    if let Value::Bool(false) = cond {
                        break;
                    }
                    self.evaluate_block(while_expr.body.clone())?;
                }
                Ok(Value::Void)
            }
            ExpressionEnum::For(for_expr) => {
                if let ExpressionEnum::Call(call) = *for_expr.iterable {
                    if let ExpressionEnum::Identifier(ident) = *call.function {
                        if ident.value == "range" {
                            if call.arguments.len() == 1 {
                                let stop_val = self.evaluate_expression_enum(call.arguments[0].clone())?;
                                if let Value::Int(stop) = stop_val {
                                    let var_name = for_expr.element.value;
                                    for i in 0..stop {
                                        self.variables.insert(var_name.clone(), Value::Int(i));
                                        self.evaluate_block(for_expr.body.clone())?;
                                    }
                                    Ok(Value::Void)
                                } else {
                                    Err("range argument must be int".to_string())
                                }
                            } else {
                                Err("range takes one argument".to_string())
                            }
                        } else {
                            Err("Unsupported for iterable".to_string())
                        }
                    } else {
                        Err("Unsupported for iterable".to_string())
                    }
                } else {
                    Err("Unsupported for iterable".to_string())
                }
            }
            ExpressionEnum::ArrayLiteral(arr) => {
                let mut list = Vec::new();
                for elem in arr.elements {
                    list.push(self.evaluate_expression_enum(elem)?);
                }
                Ok(Value::List(list))
            }
            _ => Err("Unsupported expression".to_string()),
        }
    }
}