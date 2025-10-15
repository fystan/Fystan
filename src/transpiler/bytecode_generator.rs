use crate::ast::{ExpressionEnum, InfixOperator, PrefixOperator, Program, Statement};
use crate::transpiler::string_allocator::StringAllocator;

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
    LoadNone,
    DefFn(String, usize, Vec<u8>),
    GetItem,
    SetItem,
}

pub struct BytecodeGenerator {
    instructions: Vec<Opcode>,
    pub string_allocator: StringAllocator,
}

impl BytecodeGenerator {
    /// Creates a new bytecode generator.
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            string_allocator: StringAllocator::new(),
        }
    }

    /// Generates bytecode from a Fystan program.
    pub fn generate(&mut self, program: Program) -> Result<Vec<u8>, String> {
        for stmt in program.statements {
            self.generate_statement(stmt)?;
        }
        self.instructions.push(Opcode::Halt);
        Ok(self.serialize_bytecode())
    }

    fn generate_statement(&mut self, stmt: Statement) -> Result<(), String> {
        match stmt {
            Statement::Expression(expr_stmt) => {
                self.generate_expression(expr_stmt.expression)?;
                Ok(())
            }
            Statement::Return(ret_stmt) => {
                self.generate_expression(ret_stmt.return_value)?;
                self.instructions.push(Opcode::Return);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn generate_block_statement(&mut self, block: crate::ast::BlockStatement) -> Result<(), String> {
        for stmt in block.statements {
            self.generate_statement(stmt)?;
        }
        Ok(())
    }

    fn generate_expression(&mut self, expr: ExpressionEnum) -> Result<(), String> {
        match expr {
            ExpressionEnum::IntegerLiteral(lit) => {
                self.instructions.push(Opcode::LoadConst(lit.value));
            }
            ExpressionEnum::FloatLiteral(lit) => {
                self.instructions.push(Opcode::LoadFloat(lit.value));
            }
            ExpressionEnum::StringLiteral(lit) => {
                let index = self.string_allocator.allocate(&lit.value);
                self.instructions.push(Opcode::LoadString(index));
            }
            ExpressionEnum::Boolean(b) => {
                self.instructions.push(Opcode::LoadBool(b.value));
            }
            ExpressionEnum::Identifier(ident) => {
                self.instructions.push(Opcode::LoadVar(ident.value));
            }
            ExpressionEnum::Infix(infix_expr) => {
                let op = infix_expr.operator;
                let left_expr = *infix_expr.left;
                let right_expr = *infix_expr.right;

                // Check if it's any kind of assignment
                if op == InfixOperator::Assign || op == InfixOperator::PlusEq || op == InfixOperator::MinusEq || op == InfixOperator::AsteriskEq || op == InfixOperator::SlashEq {
                    if let ExpressionEnum::Identifier(ident) = left_expr {
                        match op {
                            InfixOperator::Assign => {
                                // RHS -> Store
                                self.generate_expression(right_expr)?;
                                self.instructions.push(Opcode::StoreVar(ident.value));
                            }
                            InfixOperator::PlusEq => {
                                // LHS_Load -> RHS -> Add -> Store
                                self.instructions.push(Opcode::LoadVar(ident.value.clone()));
                                self.generate_expression(right_expr)?;
                                self.instructions.push(Opcode::Add);
                                self.instructions.push(Opcode::StoreVar(ident.value));
                            }
                            InfixOperator::MinusEq => {
                                self.instructions.push(Opcode::LoadVar(ident.value.clone()));
                                self.generate_expression(right_expr)?;
                                self.instructions.push(Opcode::Sub);
                                self.instructions.push(Opcode::StoreVar(ident.value));
                            }
                            InfixOperator::AsteriskEq => {
                                self.instructions.push(Opcode::LoadVar(ident.value.clone()));
                                self.generate_expression(right_expr)?;
                                self.instructions.push(Opcode::Mul);
                                self.instructions.push(Opcode::StoreVar(ident.value));
                            }
                            InfixOperator::SlashEq => {
                                self.instructions.push(Opcode::LoadVar(ident.value.clone()));
                                self.generate_expression(right_expr)?;
                                self.instructions.push(Opcode::Div);
                                self.instructions.push(Opcode::StoreVar(ident.value));
                            }
                            _ => unreachable!(), // Should not happen
                        }
                    } else {
                        // Handle list assignments etc. later
                        return Err("Invalid assignment target".to_string());
                    }
                } else {
                    // For all other infix operators, evaluate left then right.
                    self.generate_expression(left_expr)?;
                    self.generate_expression(right_expr)?;
                    match op {
                        InfixOperator::Plus => self.instructions.push(Opcode::Add),
                        InfixOperator::Minus => self.instructions.push(Opcode::Sub),
                        InfixOperator::Multiply => self.instructions.push(Opcode::Mul),
                        InfixOperator::Divide => self.instructions.push(Opcode::Div),
                        InfixOperator::Mod => self.instructions.push(Opcode::Mod),
                        InfixOperator::Eq => self.instructions.push(Opcode::Eq),
                        InfixOperator::NotEq => self.instructions.push(Opcode::NotEq),
                        InfixOperator::Lt => self.instructions.push(Opcode::Lt),
                        InfixOperator::Gt => self.instructions.push(Opcode::Gt),
                        InfixOperator::And => self.instructions.push(Opcode::And),
                        InfixOperator::Or => self.instructions.push(Opcode::Or),
                        _ => return Err(format!("Unsupported operator {:?}", op)),
                    }
                }
            }
            ExpressionEnum::Prefix(prefix_expr) => {
                self.generate_expression(*prefix_expr.right)?;
                match prefix_expr.operator {
                    PrefixOperator::Not => self.instructions.push(Opcode::Not),
                    PrefixOperator::Minus => self.instructions.push(Opcode::Neg),
                }
            }
            ExpressionEnum::Call(call_expr) => {
                if let ExpressionEnum::Identifier(ident) = call_expr.function.as_ref() {
                    // Special case for print() to handle different types
                    if ident.value == "print" && call_expr.arguments.len() == 1 {
                        let arg = &call_expr.arguments[0];
                        self.generate_expression(arg.clone())?;
                        if matches!(arg, ExpressionEnum::StringLiteral(_)) {
                            self.instructions.push(Opcode::PrintStr);
                        } else {
                            self.instructions.push(Opcode::Print);
                        }
                    } else {
                        for arg in &call_expr.arguments {
                            self.generate_expression(arg.clone())?;
                        }
                        self.instructions.push(Opcode::Call(ident.value.clone(), call_expr.arguments.len()));
                    }
                } else {
                    return Err("Unsupported function call".to_string());
                }
            }
            ExpressionEnum::If(if_expr) => {
                self.generate_expression(*if_expr.condition)?;
                let jump_if_false_pos = self.instructions.len();
                self.instructions.push(Opcode::JumpIfFalse(0)); // Placeholder
                self.generate_block_statement(if_expr.consequence)?;
                let jump_pos = self.instructions.len();
                self.instructions.push(Opcode::Jump(0)); // Placeholder
                let alternative_pos = self.instructions.len();
                self.instructions[jump_if_false_pos] = Opcode::JumpIfFalse(alternative_pos);
                if let Some(alternative) = if_expr.alternative {
                    self.generate_block_statement(alternative)?;
                }
                let end_pos = self.instructions.len();
                self.instructions[jump_pos] = Opcode::Jump(end_pos);
            }
            ExpressionEnum::While(while_expr) => {
                let loop_start_pos = self.instructions.len();
                self.generate_expression(*while_expr.condition)?;
                let jump_if_false_pos = self.instructions.len();
                self.instructions.push(Opcode::JumpIfFalse(0)); // Placeholder
                self.generate_block_statement(while_expr.body)?;
                self.instructions.push(Opcode::Jump(loop_start_pos));
                let loop_end_pos = self.instructions.len();
                self.instructions[jump_if_false_pos] = Opcode::JumpIfFalse(loop_end_pos);
            }
            ExpressionEnum::For(for_expr) => {
                if let ExpressionEnum::Call(call_expr) = *for_expr.iterable {
                    if let ExpressionEnum::Identifier(ident) = *call_expr.function {
                        if ident.value == "range" {
                            // Special case for range()
                            let stop_expr = call_expr.arguments[0].clone();
                            let loop_var = for_expr.element.value;

                            // Initialize loop variable to 0
                            self.instructions.push(Opcode::LoadConst(0));
                            self.instructions.push(Opcode::StoreVar(loop_var.clone()));

                            let loop_start_pos = self.instructions.len();

                            // Condition: loop_var < stop_expr
                            self.instructions.push(Opcode::LoadVar(loop_var.clone()));
                            self.generate_expression(stop_expr)?;
                            self.instructions.push(Opcode::Lt);

                            let jump_if_false_pos = self.instructions.len();
                            self.instructions.push(Opcode::JumpIfFalse(0)); // Placeholder

                            // Body
                            self.generate_block_statement(for_expr.body)?;

                            // Increment: loop_var = loop_var + 1
                            self.instructions.push(Opcode::LoadVar(loop_var.clone()));
                            self.instructions.push(Opcode::LoadConst(1));
                            self.instructions.push(Opcode::Add);
                            self.instructions.push(Opcode::StoreVar(loop_var.clone()));

                            self.instructions.push(Opcode::Jump(loop_start_pos));

                            let loop_end_pos = self.instructions.len();
                            self.instructions[jump_if_false_pos] = Opcode::JumpIfFalse(loop_end_pos);

                        } else {
                            return Err("Unsupported for loop iterable".to_string());
                        }
                    } else {
                        return Err("Unsupported for loop iterable".to_string());
                    }
                }
            }
            ExpressionEnum::ArrayLiteral(arr_lit) => {
                for elem in &arr_lit.elements {
                    self.generate_expression(elem.clone())?;
                }
                self.instructions.push(Opcode::BuildList(arr_lit.elements.len()));
            }
            ExpressionEnum::None(_) => {
                self.instructions.push(Opcode::LoadNone);
            }
            ExpressionEnum::Function(func) => {
                let mut func_generator = BytecodeGenerator::new();
                func_generator.generate_block_statement(func.body)?;
                func_generator.instructions.push(Opcode::LoadNone);
                func_generator.instructions.push(Opcode::Return);
                let func_bytecode = func_generator.serialize_bytecode();
                self.instructions.push(Opcode::DefFn(
                    func.name.value,
                    func.parameters.len(),
                    func_bytecode,
                ));
            }
            ExpressionEnum::IndexExpression(expr) => {
                self.generate_expression(*expr.left)?;
                self.generate_expression(*expr.index)?;
                self.instructions.push(Opcode::GetItem);
            }
            _ => return Err("Unsupported expression".to_string()),
        }
        Ok(())
    }

    fn serialize_bytecode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        for op in &self.instructions {
            match op {
                Opcode::LoadConst(val) => {
                    bytecode.push(0);
                    bytecode.extend_from_slice(&val.to_le_bytes());
                }
                Opcode::LoadString(idx) => {
                    bytecode.push(1);
                    bytecode.extend_from_slice(&(*idx as i64).to_le_bytes());
                }
                Opcode::LoadBool(b) => {
                    bytecode.push(2);
                    bytecode.push(if *b { 1 } else { 0 });
                }
                Opcode::LoadVar(name) => {
                    bytecode.push(3);
                    let name_bytes = name.as_bytes();
                    bytecode.push(name_bytes.len() as u8);
                    bytecode.extend_from_slice(name_bytes);
                }
                Opcode::StoreVar(name) => {
                    bytecode.push(4);
                    let name_bytes = name.as_bytes();
                    bytecode.push(name_bytes.len() as u8);
                    bytecode.extend_from_slice(name_bytes);
                }
                Opcode::Add => bytecode.push(5),
                Opcode::Sub => bytecode.push(6),
                Opcode::Mul => bytecode.push(7),
                Opcode::Div => bytecode.push(8),
                Opcode::Mod => bytecode.push(9),
                Opcode::Eq => bytecode.push(10),
                Opcode::NotEq => bytecode.push(11),
                Opcode::Lt => bytecode.push(12),
                Opcode::Gt => bytecode.push(13),
                Opcode::And => bytecode.push(14),
                Opcode::Or => bytecode.push(15),
                Opcode::Not => bytecode.push(16),
                Opcode::Neg => bytecode.push(17),
                Opcode::Jump(addr_index) => {
                    bytecode.push(18);
                    bytecode.extend_from_slice(&(*addr_index as i64).to_le_bytes());
                }
                Opcode::JumpIfFalse(addr_index) => {
                    bytecode.push(19);
                    bytecode.extend_from_slice(&(*addr_index as i64).to_le_bytes());
                }
                Opcode::Call(name, argc) => {
                    bytecode.push(20);
                    let name_bytes = name.as_bytes();
                    bytecode.push(name_bytes.len() as u8);
                    bytecode.extend_from_slice(name_bytes);
                    bytecode.push(*argc as u8);
                }
                Opcode::Return => bytecode.push(21),
                Opcode::Print => bytecode.push(22),
                Opcode::Halt => bytecode.push(23),
                Opcode::LoadFloat(val) => {
                    bytecode.push(24);
                    bytecode.extend_from_slice(&val.to_le_bytes());
                }
                Opcode::BuildList(size) => {
                    bytecode.push(25);
                    bytecode.extend_from_slice(&(*size as i64).to_le_bytes());
                }
                Opcode::LoadNone => bytecode.push(26),
                Opcode::DefFn(name, arity, body) => {
                    bytecode.push(27);
                    bytecode.push(name.len() as u8);
                    bytecode.extend_from_slice(name.as_bytes());
                    bytecode.push(*arity as u8);
                    bytecode.extend_from_slice(&(body.len() as u32).to_le_bytes());
                    bytecode.extend_from_slice(body);
                }
                Opcode::GetItem => bytecode.push(28),
                Opcode::PrintStr => bytecode.push(29),
                Opcode::SetItem => bytecode.push(30),
            }
        }

        bytecode
    }
}