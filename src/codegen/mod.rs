
use std::collections::HashMap;
use std::path::Path;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType, PointerType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::{ExpressionEnum, InfixOperator, Node, Program, Statement, PrefixOperator};
use crate::parser::Parser;

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,
    variables: HashMap<String, PointerValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("fystan_module");

        let fpm = PassManager::create(&module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.initialize();

        Self {
            context,
            builder,
            module,
            fpm,
            variables: HashMap::new(),
            current_function: None,
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<(), String> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn compile_statement(&mut self, stmt: Statement) -> Result<(), String> {
        match stmt {
            Statement::Expression(expr_stmt) => {
                self.compile_expression(&expr_stmt.expression)?;
                Ok(())
            }
            Statement::Let(let_stmt) => {
                let name = let_stmt.name.value;
                let initial_value = self.compile_expression(&let_stmt.value)?;

                let alloca = self.create_entry_block_alloca(initial_value.get_type(), &name);
                self.builder.build_store(alloca, initial_value);

                self.variables.insert(name, alloca);
                Ok(())
            }
            Statement::Return(ret_stmt) => {
                let value = self.compile_expression(&ret_stmt.return_value)?;
                self.builder.build_return(Some(&value));
                Ok(())
            }
        }
    }

    fn compile_expression(&mut self, expr: &ExpressionEnum) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            ExpressionEnum::IntegerLiteral(lit) => {
                Ok(self.context.i64_type().const_int(lit.value as u64, false).into())
            }
            ExpressionEnum::FloatLiteral(lit) => {
                Ok(self.context.f64_type().const_float(lit.value).into())
            }
            ExpressionEnum::Boolean(lit) => {
                Ok(self.context.bool_type().const_int(if lit.value { 1 } else { 0 }, false).into())
            }
            ExpressionEnum::StringLiteral(lit) => {
                Ok(self.builder.build_global_string_ptr(&lit.value, ".str").as_basic_value_enum())
            }
            ExpressionEnum::Identifier(ident) => {
                match self.variables.get(&ident.value) {
                    Some(var) => Ok(self.builder.build_load(*var, &ident.value).unwrap()),
                    None => Err(format!("Undefined variable: {}", ident.value)),
                }
            }
            ExpressionEnum::Prefix(prefix) => {
                let right = self.compile_expression(&prefix.right)?;
                match prefix.operator {
                    PrefixOperator::Bang => {
                        let i1_val = self.builder.build_int_cast(right.into_int_value(), self.context.bool_type(), "boolcast").unwrap();
                        let inverted = self.builder.build_not(i1_val, "inverttmp").unwrap();
                        Ok(self.builder.build_int_cast(inverted, self.context.i64_type(), "intcast").unwrap().into())
                    }
                    PrefixOperator::Minus => {
                        if right.is_int_value() {
                            Ok(self.builder.build_int_neg(right.into_int_value(), "negtmp").unwrap().into())
                        } else if right.is_float_value() {
                            Ok(self.builder.build_float_neg(right.into_float_value(), "negtmp").unwrap().into())
                        } else {
                            Err("Unary minus operator can only be applied to numbers".to_string())
                        }
                    }
                }
            }
            ExpressionEnum::Infix(infix) => {
                let left = self.compile_expression(&infix.left)?;
                let right = self.compile_expression(&infix.right)?;

                if left.get_type() != right.get_type() {
                    return Err("Infix operator requires operands of the same type".to_string());
                }

                match infix.operator {
                    InfixOperator::Plus => self.compile_add(left, right),
                    InfixOperator::Minus => self.compile_sub(left, right),
                    InfixOperator::Multiply => self.compile_mul(left, right),
                    InfixOperator::Divide => self.compile_div(left, right),
                    InfixOperator::Lt => self.compile_comparison(inkwell::IntPredicate::SLT, inkwell::FloatPredicate::OLT, left, right),
                    InfixOperator::Gt => self.compile_comparison(inkwell::IntPredicate::SGT, inkwell::FloatPredicate::OGT, left, right),
                    InfixOperator::Eq => self.compile_comparison(inkwell::IntPredicate::EQ, inkwell::FloatPredicate::OEQ, left, right),
                    InfixOperator::NotEq => self.compile_comparison(inkwell::IntPredicate::NE, inkwell::FloatPredicate::ONE, left, right),
                    _ => Err(format!("Unsupported infix operator: {}", infix.operator)),
                }
            }
            ExpressionEnum::If(if_expr) => self.compile_if_expression(if_expr),
            ExpressionEnum::Function(func_lit) => {
                let function = self.compile_prototype(func_lit)?;
                let entry = self.context.append_basic_block(function, "entry");

                let previous_function = self.current_function;
                self.current_function = Some(function);
                let previous_vars = self.variables.clone();
                self.variables.clear();

                self.builder.position_at_end(entry);

                for (i, arg) in function.get_param_iter().enumerate() {
                    let arg_name = &func_lit.parameters[i].value;
                    let alloca = self.create_entry_block_alloca(arg.get_type(), arg_name);
                    self.builder.build_store(alloca, arg);
                    self.variables.insert(arg_name.clone(), alloca);
                }

                self.compile_block_statement(&func_lit.body)?;

                // Add implicit return if block doesn't have one
                if entry.get_terminator().is_none() {
                     if function.get_type().get_return_type().is_some() {
                        self.builder.build_return(Some(&function.get_type().get_return_type().unwrap().const_zero()));
                     } else {
                        self.builder.build_return(None);
                     }
                }

                if function.verify(true) {
                    self.fpm.run_on(&function);
                } else {
                    // For debugging: Print the invalid function's IR
                    // function.print_to_stderr();
                    return Err(format!("Invalid function generated: {}", func_lit.name.value));
                }

                self.current_function = previous_function;
                self.variables = previous_vars;

                Ok(function.as_global_value().as_basic_value_enum())
            }
            ExpressionEnum::Call(call_expr) => {
                let function_name = call_expr.function.to_string();
                let function = self.get_function(&function_name)
                    .ok_or_else(|| format!("Unknown function: {}", function_name))?;

                let mut args = Vec::new();
                for arg_expr in &call_expr.arguments {
                    args.push(self.compile_expression(arg_expr)?.into());
                }

                let call = self.builder.build_call(function, &args, "calltmp").unwrap();
                Ok(call.try_as_basic_value().left().unwrap())
            }
            _ => Err(format!("Unsupported expression: {}", expr)),
        }
    }

    fn compile_prototype(&mut self, func: &crate::ast::FunctionLiteral) -> Result<FunctionValue<'ctx>, String> {
        let param_types: Vec<BasicTypeEnum<'ctx>> = func.parameters
            .iter()
            .map(|_| self.context.i64_type().into()) // Assume i64 for now
            .collect();

        // Assume i64 return type for now
        let func_type = self.context.i64_type().fn_type(&param_types, false);
        let function = self.module.add_function(&func.name.value, func_type, None);

        for (i, arg) in function.get_param_iter().enumerate() {
            arg.set_name(&func.parameters[i].value);
        }

        Ok(function)
    }

    fn compile_block_statement(&mut self, block: &crate::ast::BlockStatement) -> Result<(), String> {
        for stmt in &block.statements {
            // Stop compiling if the block is already terminated
            if self.builder.get_insert_block().unwrap().get_terminator().is_some() {
                break;
            }
            self.compile_statement(stmt.clone())?;
        }
        Ok(())
    }

    fn compile_if_expression(&mut self, if_expr: &crate::ast::IfExpression) -> Result<BasicValueEnum<'ctx>, String> {
        let condition = self.compile_expression(&if_expr.condition)?;
        let i1_cond = self.builder.build_int_compare(
            inkwell::IntPredicate::NE,
            condition.into_int_value(),
            self.context.i64_type().const_zero(),
            "ifcond",
        ).unwrap();

        let function = self.current_function.ok_or("If expression outside of a function")?;

        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "ifcont");

        self.builder.build_conditional_branch(i1_cond, then_bb, else_bb);

        // Build then block
        self.builder.position_at_end(then_bb);
        self.compile_block_statement(&if_expr.consequence)?;
        if then_bb.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb);
        }
        let then_val = self.builder.get_insert_block().unwrap(); // Value is last instruction

        // Build else block
        self.builder.position_at_end(else_bb);
        if let Some(alt) = &if_expr.alternative {
            self.compile_block_statement(alt)?;
        }
        if else_bb.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb);
        }
        let else_val = self.builder.get_insert_block().unwrap();

        // Build merge block
        self.builder.position_at_end(merge_bb);

        // For now, if expressions don't return a value. We'll return a dummy 0.
        // A real implementation would use a PHI node to merge values from both branches.
        Ok(self.context.i64_type().const_zero().into())
    }

    fn compile_add(&self, left: BasicValueEnum<'ctx>, right: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        if left.is_int_value() {
            Ok(self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "addtmp").unwrap().into())
        } else {
            Ok(self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap().into())
        }
    }

    fn compile_sub(&self, left: BasicValueEnum<'ctx>, right: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        if left.is_int_value() {
            Ok(self.builder.build_int_sub(left.into_int_value(), right.into_int_value(), "subtmp").unwrap().into())
        } else {
            Ok(self.builder.build_float_sub(left.into_float_value(), right.into_float_value(), "subtmp").unwrap().into())
        }
    }

    fn compile_mul(&self, left: BasicValueEnum<'ctx>, right: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        if left.is_int_value() {
            Ok(self.builder.build_int_mul(left.into_int_value(), right.into_int_value(), "multmp").unwrap().into())
        } else {
            Ok(self.builder.build_float_mul(left.into_float_value(), right.into_float_value(), "multmp").unwrap().into())
        }
    }

    fn compile_div(&self, left: BasicValueEnum<'ctx>, right: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        if left.is_int_value() {
            Ok(self.builder.build_int_signed_div(left.into_int_value(), right.into_int_value(), "divtmp").unwrap().into())
        } else {
            Ok(self.builder.build_float_div(left.into_float_value(), right.into_float_value(), "divtmp").unwrap().into())
        }
    }

    fn compile_comparison(&self, int_pred: inkwell::IntPredicate, float_pred: inkwell::FloatPredicate, left: BasicValueEnum<'ctx>, right: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        let cmp = if left.is_int_value() {
            self.builder.build_int_compare(int_pred, left.into_int_value(), right.into_int_value(), "cmptmp").unwrap()
        } else {
            self.builder.build_float_compare(float_pred, left.into_float_value(), right.into_float_value(), "cmptmp").unwrap()
        };
        Ok(self.builder.build_int_cast(cmp, self.context.i64_type(), "booltmp").unwrap().into())
    }

    fn create_entry_block_alloca<T: BasicType<'ctx>>(&self, ty: T, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.current_function.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).unwrap()
    }

    pub fn write_to_object_file(&self, target_triple: &str, output_path: &Path) -> Result<(), String> {
        Target::initialize_all(&InitializationConfig::default());
        let target = Target::from_triple(target_triple).map_err(|e| e.to_string())?;
        let target_machine = target
            .create_target_machine(
                target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .ok_or_else(|| "Unable to create target machine".to_string())?;

        self.module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        self.module.set_triple(target_triple);

        target_machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, output_path)
            .map_err(|e| e.to_string())
    }

    pub fn run_from_source(src: &str, target_triple: &str, output_filename: &str) -> Result<(), String> {
        let l = crate::lexer::Lexer::new(src);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if !p.errors().is_empty() {
            return Err(format!("Parser errors: {:?}", p.errors()));
        }

        let context = Context::create();
        let mut compiler = Compiler::new(&context);

        // Create a main function to act as the entry point
        let main_fn_type = compiler.context.i64_type().fn_type(&[], false);
        let main_fn = compiler.module.add_function("_fystan_entry_point", main_fn_type, None);
        let entry_block = compiler.context.append_basic_block(main_fn, "entry");
        compiler.builder.position_at_end(entry_block);
        compiler.current_function = Some(main_fn);

        compiler.compile(program)?;

        // Ensure the main function has a return
        if compiler.builder.get_insert_block().unwrap().get_terminator().is_none() {
            // If the last expression was the return value, it's already handled.
            // Otherwise, return 0.
            compiler.builder.build_return(Some(&compiler.context.i64_type().const_int(0, false)));
        }

        // compiler.module.print_to_stderr(); // For debugging LLVM IR

        let obj_path = Path::new(&output_filename).with_extension("obj");
        compiler.write_to_object_file(target_triple, &obj_path)?;

        // Link the object file
        let output = std::process::Command::new("clang")
            .arg(obj_path)
            .arg("-o")
            .arg(output_filename)
            .output()
            .map_err(|e| format!("Failed to execute linker: {}", e))?;

        if !output.status.success() {
            return Err(format!(
                "Linker failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        // Clean up the temporary object file
        std::fs::remove_file(obj_path).map_err(|e| format!("Failed to remove object file: {}", e))?;

        Ok(())
    }
}
