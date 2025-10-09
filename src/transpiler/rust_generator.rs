use crate::ast::{BlockStatement, ExpressionEnum, ForExpression, InfixOperator, PrefixOperator, Program, Statement};
use crate::transpiler::scope::ScopeManager;
use crate::transpiler::string_allocator::StringAllocator;
use std::fmt::Write;

pub struct Generator {
    scopes: ScopeManager,
    temp_counter: usize,
    string_allocator: StringAllocator,
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            scopes: ScopeManager::new(),
            temp_counter: 0,
            string_allocator: StringAllocator::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Result<String, String> {
        let mut function_defs = String::new();
        let mut main_statements = String::new();
        let mut has_user_main = false;

        // First, generate all statement and function code.
        // This will populate the string_allocator.
        for statement in &program.statements {
            if let Statement::Expression(expr_stmt) = statement {
                if let ExpressionEnum::Function(func_lit) = &expr_stmt.expression {
                    if func_lit.name.value == "main" {
                        has_user_main = true;
                    }
                    let func_code = self.generate_function_definition(func_lit.clone())?;
                    function_defs.push_str(&func_code);
                    function_defs.push('\n');
                } else {
                    let stmt_code = self.generate_statement(statement.clone())?;
                    writeln!(main_statements, "    {}", stmt_code).unwrap();
                }
            } else {
                let stmt_code = self.generate_statement(statement.clone())?;
                writeln!(main_statements, "    {}", stmt_code).unwrap();
            }
        }

        // Now that the allocator is populated, assemble the final code.
        let mut final_code = String::new();
        writeln!(final_code, "#[macro_use]\nextern crate lazy_static;").unwrap();
        writeln!(final_code, "// Auto-generated Rust code from Fystan program").unwrap();

        // Get the initialization code for the now-populated string arena.
        let init_code = self.string_allocator.get_initialization_code();
        final_code.push_str(&init_code);
        final_code.push_str(&function_defs);

        if !has_user_main {
            writeln!(final_code, "fn main() {{").unwrap();
            final_code.push_str(&main_statements);
            writeln!(final_code, "}}").unwrap();
        }

        Ok(final_code)
    }

    fn generate_function_definition(&mut self, func_lit: crate::ast::FunctionLiteral) -> Result<String, String> {
        let name = func_lit.name.value;
        let mut params = Vec::new();

        for param in &func_lit.parameters {
            params.push(format!("{}: i64", param.value));
        }

        self.scopes.push_scope();
        for param in &func_lit.parameters {
            self.scopes.add_variable(param.value.clone());
        }

        let body = self.generate_block_statement(func_lit.body)?;

        self.scopes.pop_scope();

        if name == "main" {
            let processed_body = if body.contains("return ") {
                body.replace("return 0;", "")
                    .replace("return", "// return")
            } else {
                body
            };

            Ok(format!(
                "fn main() {{ {} }}",
                processed_body
            ))
        } else {
            if !body.contains("return ") {
                Ok(format!(
                    "fn {}({}) -> i64 {{ {} return 0; }}",
                    name,
                    params.join(", "),
                    body
                ))
            } else {
                Ok(format!(
                    "fn {}({}) -> i64 {{ {} }}",
                    name,
                    params.join(", "),
                    body
                ))
            }
        }
    }

    fn generate_statement(&mut self, statement: Statement) -> Result<String, String> {
        match statement {
            Statement::Expression(expr_stmt) => {
                let expr_code = self.generate_expression(expr_stmt.expression)?;
                Ok(format!("{};", expr_code))
            },
            Statement::Return(ret_stmt) => {
                let value = self.generate_expression(ret_stmt.return_value)?;
                Ok(format!("return {};", value))
            },
            Statement::Break(_) => Ok("break;".to_string()),
            Statement::Continue(_) => Ok("continue;".to_string()),
            Statement::Pass(_) => Ok("/* pass */".to_string()),
            Statement::Try(try_stmt) => {
                Ok(format!("{{ {} }}", self.generate_block_statement(try_stmt.body)?))
            },
        }
    }

    fn generate_expression(&mut self, expression: ExpressionEnum) -> Result<String, String> {
        match expression {
            ExpressionEnum::IntegerLiteral(lit) => Ok(lit.value.to_string()),
            ExpressionEnum::FloatLiteral(lit) => Ok(lit.value.to_string()),
            ExpressionEnum::StringLiteral(lit) => {
                let index = self.string_allocator.allocate(&lit.value);
                Ok(format!("STRING_ARENA[{}]", index))
            },
            ExpressionEnum::Boolean(b) => Ok(b.value.to_string()),
            ExpressionEnum::None(_) => Ok("None".to_string()),
            ExpressionEnum::Identifier(ident) => {
                if self.scopes.is_variable_in_scope(&ident.value) {
                    return Ok(ident.value.clone());
                }
                Ok(ident.value)
            },
            ExpressionEnum::Prefix(prefix_expr) => {
                let right = self.generate_expression(*prefix_expr.right)?;
                let op_str = match prefix_expr.operator {
                    PrefixOperator::Not => "!",
                    PrefixOperator::Minus => "-",
                };
                Ok(format!("({} {})", op_str, right))
            },
            ExpressionEnum::Infix(infix_expr) => {
                if infix_expr.operator == InfixOperator::Assign {
                    if let ExpressionEnum::Identifier(ident) = *infix_expr.left {
                        let is_new_var = !self.scopes.is_variable_in_scope(&ident.value);
                        self.scopes.add_variable(ident.value.clone());

                        let value = self.generate_expression(*infix_expr.right)?;
                        if is_new_var {
                            Ok(format!("let mut {} = {};", ident.value, value))
                        } else {
                            Ok(format!("{} = {};", ident.value, value))
                        }
                    } else {
                        Err("Destination of assignment must be an identifier".to_string())
                    }
                } else {
                    let left = self.generate_expression(*infix_expr.left)?;
                    let right = self.generate_expression(*infix_expr.right)?;
                    let op_str = match infix_expr.operator {
                        InfixOperator::Plus => "+",
                        InfixOperator::Minus => "-",
                        InfixOperator::Multiply => "*",
                        InfixOperator::Divide => "/",
                        InfixOperator::Mod => "%",
                        InfixOperator::Eq => "==",
                        InfixOperator::NotEq => "!=",
                        InfixOperator::Is => "==",
                        InfixOperator::IsNot => "!=",
                        InfixOperator::Lt => "<",
                        InfixOperator::Gt => ">",
                        InfixOperator::And => "&&",
                        InfixOperator::Or => "||",
                        InfixOperator::Assign => "=",
                        InfixOperator::PlusEq => "+=",
                        InfixOperator::MinusEq => "-=",
                        InfixOperator::AsteriskEq => "*=",
                        InfixOperator::SlashEq => "/=",
                    };
                    Ok(format!("{} {} {}", left, op_str, right))
                }
            },
            ExpressionEnum::If(if_expr) => {
                let condition = self.generate_expression(*if_expr.condition)?;
                let consequence = self.generate_block_statement(if_expr.consequence)?;
                let alternative = if let Some(alt) = if_expr.alternative {
                    format!(" else {{ {} }}", self.generate_block_statement(alt)?)
                } else {
                    String::new()
                };
                Ok(format!("if {} {{ {} }}{}", condition, consequence, alternative))
            },
            ExpressionEnum::While(while_expr) => {
                let condition = self.generate_expression(*while_expr.condition)?;
                let body = self.generate_block_statement(while_expr.body)?;
                Ok(format!("while {} {{ {} }}", condition, body))
            },
            ExpressionEnum::For(for_expr) => self.generate_for_loop(for_expr),
            ExpressionEnum::Call(call_expr) => self.generate_call_expression(call_expr),
            ExpressionEnum::ArrayLiteral(arr_lit) => {
                let mut elements = Vec::new();
                for element in arr_lit.elements {
                    elements.push(self.generate_expression(element)?);
                }
                Ok(format!("vec![{}]", elements.join(", ")))
            },
            ExpressionEnum::IndexExpression(idx_expr) => {
                let array = self.generate_expression(*idx_expr.left)?;
                let index = self.generate_expression(*idx_expr.index)?;
                Ok(format!("{}[{}]", array, index))
            },
            ExpressionEnum::HashLiteral(hash_lit) => {
                let mut code = "{ let mut map = std::collections::HashMap::new(); ".to_string();
                for (key, value) in hash_lit.pairs {
                    let value_code = self.generate_expression(value)?;
                    code.push_str(&format!("map.insert(\"{}\".to_string(), {}); ", key, value_code));
                }
                code.push_str("map }");
                Ok(code)
            },
            ExpressionEnum::Function(_) => Ok("/* function definition */".to_string()),
        }
    }

    fn generate_block_statement(&mut self, block: BlockStatement) -> Result<String, String> {
        self.scopes.push_scope();
        let mut result = String::new();
        for stmt in block.statements {
            result.push_str(&self.generate_statement(stmt)?);
            result.push_str("\n        ");
        }
        self.scopes.pop_scope();
        Ok(result)
    }

    fn generate_for_loop(&mut self, for_expr: ForExpression) -> Result<String, String> {
        let element = for_expr.element.value;
        let iterable = self.generate_expression(*for_expr.iterable)?;

        self.scopes.push_scope();
        self.scopes.add_variable(element.clone());
        let body = self.generate_block_statement(for_expr.body)?;
        self.scopes.pop_scope();

        Ok(format!("for {} in {} {{ {} }}", element, iterable, body))
    }

    fn new_temp_var(&mut self) -> String {
        let i = self.temp_counter;
        self.temp_counter += 1;
        format!("temp_{}", i)
    }

    fn generate_call_expression(&mut self, call_expr: crate::ast::CallExpression) -> Result<String, String> {
        if let ExpressionEnum::Identifier(ident) = *call_expr.function {
            let name = &ident.value;
            let arguments = call_expr.arguments;

            match name.as_str() {
                "print" => {
                    if arguments.len() != 1 {
                        return Err("print() takes exactly one argument".to_string());
                    }
                    let arg = self.generate_expression(arguments[0].clone())?;
                    Ok(format!("println!(\"{{}}\", {})", arg))
                }
                "len" => {
                    if arguments.len() != 1 {
                        return Err("len() takes exactly one argument".to_string());
                    }
                    let arg = self.generate_expression(arguments[0].clone())?;
                    Ok(format!("{}.len() as i64", arg))
                }
                "read_line" => {
                    if !arguments.is_empty() {
                        return Err("read_line() takes no arguments".to_string());
                    }
                    let temp_var = self.new_temp_var();
                    Ok(format!(
                        "{{ let mut {} = String::new(); std::io::stdin().read_line(&mut {}).unwrap(); {}.trim().to_string() }}",
                        temp_var, temp_var, temp_var
                    ))
                }
                "range" => {
                    let args_len = arguments.len();
                    if args_len < 1 || args_len > 3 {
                        return Err("range() takes 1 to 3 arguments".to_string());
                    }

                    let start = if args_len >= 2 {
                        self.generate_expression(arguments[0].clone())?
                    } else {
                        "0".to_string()
                    };

                    let stop = if args_len >= 2 {
                        self.generate_expression(arguments[1].clone())?
                    } else {
                        self.generate_expression(arguments[0].clone())?
                    };

                    if args_len == 3 {
                        let step = self.generate_expression(arguments[2].clone())?;
                        Ok(format!("({}..{}).step_by({} as usize)", start, stop, step))
                    } else {
                        Ok(format!("({}..{})", start, stop))
                    }
                }
                _ => {
                    // User-defined function
                    let mut args = Vec::new();
                    for arg in arguments {
                        args.push(self.generate_expression(arg)?);
                    }
                    Ok(format!("{}({})", name, args.join(", ")))
                }
            }
        } else {
            Err("Function name must be an identifier".to_string())
        }
    }
}