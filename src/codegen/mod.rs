use crate::ast::{BlockStatement, ExpressionEnum, ForExpression, InfixOperator, PrefixOperator, Program, Statement};
use std::collections::HashMap;
use std::fmt::Write;

pub struct Compiler {
    // A stack to keep track of variable scopes
    scopes: Vec<HashMap<String, bool>>,
    // Counter for generating unique names for temporary values
    temp_counter: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            scopes: vec![HashMap::new()], // Global scope
            temp_counter: 0,
        }
    }

    pub fn run_from_source(source: &str, output_filename: &str) -> Result<(), String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }
        
        let mut compiler = Compiler::new();
        let rust_code = compiler.compile(program)?;
        
        // Write the generated Rust code to a temporary file (without .exe extension)
        let temp_rs_name = output_filename.trim_end_matches(".exe");
        let temp_rs_file = format!("{}.rs", temp_rs_name);
        std::fs::write(&temp_rs_file, rust_code)
            .map_err(|e| format!("Failed to write temporary file: {}", e))?;
        
        // Compile the Rust code to an executable using rustc
        let output = std::process::Command::new("rustc")
            .arg(&temp_rs_file)
            .arg("-o")
            .arg(output_filename)
            .output()
            .map_err(|e| format!("Failed to run rustc: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("Rust compilation failed: {}", String::from_utf8_lossy(&output.stderr)));
        }
        
        // Clean up the temporary Rust file
        std::fs::remove_file(&temp_rs_file)
            .map_err(|e| format!("Failed to clean up temporary file: {}", e))?;
        
        Ok(())
    }

    pub fn compile(&mut self, program: Program) -> Result<String, String> {
        let mut result = String::new();
        let mut function_defs = String::new();
        let mut main_statements = String::new();
        let mut has_user_main = false;
        
        // Add necessary imports and standard definitions
        writeln!(result, "// Auto-generated Rust code from Fystan program").unwrap();
        
        // First pass: collect function definitions and main statements separately
        for statement in &program.statements {
            if let Statement::Expression(expr_stmt) = statement {
                if let ExpressionEnum::Function(func_lit) = &expr_stmt.expression {
                    // Check if it's a user-defined main function
                    if func_lit.name.value == "main" {
                        has_user_main = true;
                    }
                    // Handle all function definitions (including main)
                    let func_code = self.compile_function_definition(func_lit.clone())?;
                    function_defs.push_str(&func_code);
                    function_defs.push('\n');
                } else {
                    // Non-function expressions go to main
                    let stmt_code = self.compile_statement(statement.clone())?;
                    writeln!(main_statements, "    {}", stmt_code).unwrap();
                }
            } else {
                // Other statement types go to main
                let stmt_code = self.compile_statement(statement.clone())?;
                writeln!(main_statements, "    {}", stmt_code).unwrap();
            }
        }
        
        // Write function definitions (excluding main if user defined it)
        result.push_str(&function_defs);
        
        // Only write default main function if there's no user-defined main
        if !has_user_main {
            writeln!(result, "fn main() {{").unwrap();
            result.push_str(&main_statements);
            writeln!(result, "}}").unwrap();
        } else {
            // If user defined main, don't add additional main statements
            // The user's main function will be the only main function
        }
        
        Ok(result)
    }

    fn compile_function_definition(&mut self, func_lit: crate::ast::FunctionLiteral) -> Result<String, String> {
        let name = func_lit.name.value;
        let mut params = Vec::new();
        
        // Collect parameter names before moving them
        for param in &func_lit.parameters {
            params.push(param.value.clone());
        }
        
        // Add function parameters to a new scope
        self.scopes.push(HashMap::new());
        for param in &func_lit.parameters {
            self.scopes.last_mut().unwrap().insert(param.value.clone(), true);
        }
        
        let body = self.compile_block_statement(func_lit.body)?;
        
        // Remove the function scope
        self.scopes.pop();
        
        // Special handling for user-defined main function - make it the actual main function
        if name == "main" {
            // For user main, remove any return statements that return values since Rust main returns ()
            let processed_body = if body.contains("return ") {
                // Replace return statements that return values with just the value evaluation or empty returns
                body.replace("return 0;", "")
                    .replace("return", "// return")  // Comment out other returns
            } else {
                body
            };
            
            Ok(format!(
                "fn main() {{ {} }}",
                processed_body
            ))
        } else {
            // Check if the function body already contains a return statement
            // If not, add a default return 0;
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

    fn compile_statement(&mut self, statement: Statement) -> Result<String, String> {
        match statement {
            Statement::Expression(expr_stmt) => {
                let expr_code = self.compile_expression(expr_stmt.expression)?;
                // For expressions that result in values (not assignments or calls with side effects),
                // we may want to use them differently than just as statements.
                // For now, add a semicolon to make it a statement
                Ok(format!("{};", expr_code))
            },
            Statement::Return(ret_stmt) => {
                let value = self.compile_expression(ret_stmt.return_value)?;
                Ok(format!("return {};", value))
            },
            Statement::Break(_) => {
                Ok("break;".to_string())
            },
            Statement::Continue(_) => {
                Ok("continue;".to_string())
            },
            Statement::Pass(_) => {
                // Pass statement does nothing
                Ok("/* pass */".to_string())
            },
            Statement::Try(try_stmt) => {
                // For now, simplify try-catch to just execute the try block
                // More sophisticated exception handling would require significant changes
                Ok(format!("{{ {} }}", self.compile_block_statement(try_stmt.body)?))
            },
        }
    }

    fn compile_expression(&mut self, expression: ExpressionEnum) -> Result<String, String> {
        match expression {
            ExpressionEnum::IntegerLiteral(lit) => Ok(lit.value.to_string()),
            ExpressionEnum::FloatLiteral(lit) => Ok(lit.value.to_string()),
            ExpressionEnum::StringLiteral(lit) => Ok(format!("\"{}\"", lit.value)),
            ExpressionEnum::Boolean(b) => Ok(b.value.to_string()),
            ExpressionEnum::None(_) => Ok("None".to_string()),
            ExpressionEnum::Identifier(ident) => {
                // Check if the identifier exists in any scope
                for scope in self.scopes.iter().rev() {
                    if scope.contains_key(&ident.value) {
                        return Ok(ident.value.clone());
                    }
                }
                // If not found in any scope, treat as variable
                Ok(ident.value)
            },
            ExpressionEnum::Prefix(prefix_expr) => {
                let right = self.compile_expression(*prefix_expr.right)?;
                let op_str = match prefix_expr.operator {
                    PrefixOperator::Not => "!",
                    PrefixOperator::Minus => "-",
                };
                Ok(format!("({} {})", op_str, right))
            },
            ExpressionEnum::Infix(infix_expr) => {
                if infix_expr.operator == InfixOperator::Assign {
                    if let ExpressionEnum::Identifier(ident) = *infix_expr.left {
                        let scope_depth = self.scopes.len();
                        let is_new_var = {
                            let current_scope = &self.scopes[scope_depth - 1];
                            !current_scope.contains_key(&ident.value)
                        };
                        
                        // Add the variable to the current scope
                        self.scopes.last_mut().unwrap().insert(ident.value.clone(), true);
                        
                        let value = self.compile_expression(*infix_expr.right)?;
                        if is_new_var {
                            Ok(format!("let {} = {};", ident.value, value))
                        } else {
                            Ok(format!("{} = {};", ident.value, value))
                        }
                    } else {
                        return Err("Destination of assignment must be an identifier".to_string());
                    }
                } else {
                    let left = self.compile_expression(*infix_expr.left)?;
                    let right = self.compile_expression(*infix_expr.right)?;
                    let op_str = match infix_expr.operator {
                        InfixOperator::Plus => "+",
                        InfixOperator::Minus => "-",
                        InfixOperator::Multiply => "*",
                        InfixOperator::Divide => "/",
                        InfixOperator::Mod => "%",
                        InfixOperator::Eq => "==",
                        InfixOperator::NotEq => "!=",
                        InfixOperator::Is => "==",  // Simplification
                        InfixOperator::IsNot => "!=", // Simplification
                        InfixOperator::Lt => "<",
                        InfixOperator::Gt => ">",
                        InfixOperator::And => "&&",
                        InfixOperator::Or => "||",
                        InfixOperator::Assign => "=", // This should be handled above
                        InfixOperator::PlusEq => "+=",
                        InfixOperator::MinusEq => "-=",
                        InfixOperator::AsteriskEq => "*=",
                        InfixOperator::SlashEq => "/=",
                    };
                    Ok(format!("{} {} {}", left, op_str, right))
                }
            },
            ExpressionEnum::If(if_expr) => {
                let condition = self.compile_expression(*if_expr.condition)?;
                let consequence = self.compile_block_statement(if_expr.consequence)?;
                let alternative = if let Some(alt) = if_expr.alternative {
                    format!(" else {{ {} }}", self.compile_block_statement(alt)?)
                } else {
                    String::new()
                };
                Ok(format!("if {} {{ {} }}{}", condition, consequence, alternative))
            },
            ExpressionEnum::While(while_expr) => {
                let condition = self.compile_expression(*while_expr.condition)?;
                let body = self.compile_block_statement(while_expr.body)?;
                Ok(format!("while {} {{ {} }}", condition, body))
            },
            ExpressionEnum::For(for_expr) => {
                let element = for_expr.element.value;
                let iterable = self.compile_expression(*for_expr.iterable)?;
                let body = self.compile_block_statement(for_expr.body)?;
                
                // Add the loop variable to the current scope
                self.scopes.last_mut().unwrap().insert(element.clone(), true);
                
                Ok(format!("for {} in {} {{ {} }}", element, iterable, body))
            },
            ExpressionEnum::Call(call_expr) => {
                if let ExpressionEnum::Identifier(ident) = *call_expr.function {
                    match ident.value.as_str() {
                        "print" => {
                            if call_expr.arguments.len() != 1 {
                                return Err("print() takes exactly one argument".to_string());
                            }
                            let arg = self.compile_expression(call_expr.arguments[0].clone())?;
                            Ok(format!("println!(\"{{}}\", {})", arg))
                        },
                        "len" => {
                            if call_expr.arguments.len() != 1 {
                                return Err("len() takes exactly one argument".to_string());
                            }
                            let arg = self.compile_expression(call_expr.arguments[0].clone())?;
                            Ok(format!("{}.len() as i64", arg))
                        },
                        "read_line" => {
                            if !call_expr.arguments.is_empty() {
                                return Err("read_line() takes no arguments".to_string());
                            }
                            let temp_var = format!("temp_input_{}", self.temp_counter);
                            self.temp_counter += 1;
                            Ok(format!(
                                "{{ let mut {} = String::new(); std::io::stdin().read_line(&mut {}).unwrap(); {}.trim().to_string() }}", 
                                temp_var, temp_var, temp_var
                            ))
                        },
                        "range" => {
                            let args_len = call_expr.arguments.len();
                            if args_len < 1 || args_len > 3 {
                                return Err("range() takes 1 to 3 arguments".to_string());
                            }
                            
                            let start = if args_len >= 2 {
                                self.compile_expression(call_expr.arguments[0].clone())?
                            } else {
                                "0".to_string()
                            };
                            
                            let stop = if args_len >= 2 {
                                self.compile_expression(call_expr.arguments[1].clone())?
                            } else {
                                self.compile_expression(call_expr.arguments[0].clone())?
                            };
                            
                            if args_len == 3 {
                                let step = self.compile_expression(call_expr.arguments[2].clone())?;
                                Ok(format!("({}..{}).step_by({} as usize)", start, stop, step))
                            } else {
                                Ok(format!("({}..{})", start, stop))
                            }
                        },
                        _ => {
                            // For other function calls, assume it's a user-defined function
                            let mut args = Vec::new();
                            for arg in call_expr.arguments {
                                args.push(self.compile_expression(arg)?);
                            }
                            Ok(format!("{}({})", ident.value, args.join(", ")))
                        }
                    }
                } else {
                    Err("Function name must be an identifier".to_string())
                }
            },
            ExpressionEnum::ArrayLiteral(arr_lit) => {
                let mut elements = Vec::new();
                for element in arr_lit.elements {
                    elements.push(self.compile_expression(element)?);
                }
                Ok(format!("vec![{}]", elements.join(", ")))
            },
            ExpressionEnum::IndexExpression(idx_expr) => {
                let array = self.compile_expression(*idx_expr.left)?;
                let index = self.compile_expression(*idx_expr.index)?;
                Ok(format!("{}[{}]", array, index))
            },
            ExpressionEnum::HashLiteral(hash_lit) => {
                let mut code = "{ let mut map = std::collections::HashMap::new(); ".to_string();
                for (key, value) in hash_lit.pairs {
                    let value_code = self.compile_expression(value)?;
                    code.push_str(&format!("map.insert(\"{}\".to_string(), {}); ", key, value_code));
                }
                code.push_str("map }");
                Ok(code)
            },
            ExpressionEnum::Function(_) => {
                // Function expressions should be handled at the statement level
                Ok("/* function definition */".to_string())
            },
        }
    }

    fn compile_block_statement(&mut self, block: BlockStatement) -> Result<String, String> {
        // Create a new scope
        self.scopes.push(HashMap::new());
        
        let mut result = String::new();
        for stmt in block.statements {
            result.push_str(&self.compile_statement(stmt)?);
            result.push_str("\n        ");
        }
        
        // Remove the scope
        self.scopes.pop();
        
        Ok(result)
    }
}