use crate::ast::{self, ExpressionEnum, InfixOperator, PrefixOperator, Statement};
use crate::builtins::BuiltinRegistry; // Import new items
use crate::parser::Parser;
use cranelift::codegen::isa;
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncId, Init, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

/// The main struct responsible for compiling Fystan code.
pub struct CodegenEnvironment {
    store: HashMap<String, (Variable, Type)>,
    outer: Option<Rc<RefCell<CodegenEnvironment>>>, 
}

impl CodegenEnvironment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<CodegenEnvironment>>) -> Self {
        let mut env = CodegenEnvironment::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<(Variable, Type)> {
        match self.store.get(name) {
            Some(val) => Some(*val),
            None => self.outer.as_ref().and_then(|o| o.borrow().get(name)),
        }
    }

    pub fn set(&mut self, name: String, var: Variable, ty: Type) {
        self.store.insert(name, (var, ty));
    }
}

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile_to_object_file(
        &self,
        src: &str,
        user_target_str: &str,
    ) -> Result<Vec<u8>, String> {
        let l = crate::lexer::Lexer::new(src);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors().is_empty() {
            return Err(format!("Parser errors: {:?}", p.errors()));
        }

        let target_triple_str = map_user_target_to_triple(user_target_str)?;
        let triple = target_lexicon::Triple::from_str(&target_triple_str)
            .map_err(|e| e.to_string())?;
        let shared_flags_builder = settings::builder();
        let shared_flags = settings::Flags::new(shared_flags_builder);
        let isa_builder = isa::lookup(triple.clone()).map_err(|e| e.to_string())?;
        let isa = isa_builder
            .finish(shared_flags)
            .map_err(|e| e.to_string())?;

        let object_builder = ObjectBuilder::new(
            isa,
            "fystan_output",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| e.to_string())?;
        let mut module = ObjectModule::new(object_builder);
        let mut ctx = module.make_context();
        let mut builder_context = FunctionBuilderContext::new();
        let environment = Rc::new(RefCell::new(CodegenEnvironment::new()));
        let mut function_ids: HashMap<String, FuncId> = HashMap::new();

        // Main function signature (entry point)
        ctx.func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);

        // Compile the program's statements
        let ptr_type = module.target_config().pointer_type();

        // Initialize the built-in function registry
        let builtin_registry = BuiltinRegistry::new(ptr_type);

        // Declare all built-in functions
        for (name, builtin) in &builtin_registry.functions {
            let func_id = module
                .declare_function(name, Linkage::Import, &builtin.cranelift_signature)
                .map_err(|e| e.to_string())?;
            function_ids.insert(name.to_string(), func_id);
        }

        // Declare the 'free' function for CTMM
        let mut free_sig = Signature::new(CallConv::SystemV);
        free_sig.params.push(AbiParam::new(ptr_type));
        let free_func_id = module
            .declare_function("free", Linkage::Import, &free_sig)
            .map_err(|e| e.to_string())?;

        let mut compiler_instance = FunctionCompiler {
            module: &mut module,
            function_ids: &mut function_ids,
            builtin_registry: &builtin_registry,
            environment: environment.clone(),
            free_func_id,
            heap_allocations: Vec::new(),
        };
        compiler_instance.compile_program_statements(&mut builder, &program)?;

        println!("{}", builder.func);
                // builder.seal_block(init_block);

        let id = module
            .declare_function("_fystan_entry_point", Linkage::Export, &ctx.func.signature)
            .map_err(|e| e.to_string())?;
        module
            .define_function(id, &mut ctx)
            .map_err(|e| e.to_string())?;

        let product = module.finish();
        product.emit().map_err(|e| e.to_string())
    }
}

fn map_user_target_to_triple(user_target: &str) -> Result<String, String> {
    let parts: Vec<&str> = user_target.split(':').collect();
    if parts.len() != 2 {
        return Err(format!(
            "Invalid target format: {}. Expected OS:architecture (e.g., windows:amd64)",
            user_target
        ));
    }

    let os = parts[0].to_lowercase();
    let arch = parts[1].to_lowercase();

    match (os.as_str(), arch.as_str()) {
        ("windows", "amd64") => Ok("x86_64-pc-windows-msvc".to_string()),
        ("linux", "amd64") => Ok("x86_64-unknown-linux-gnu".to_string()),
        ("darwin", "amd64") => Ok("x86_64-apple-darwin".to_string()),
        ("android", "arm64") => Ok("aarch64-linux-android".to_string()),
        ("windows", "x86") => Ok("i686-pc-windows-msvc".to_string()),
        ("linux", "x86") => Ok("i686-unknown-linux-gnu".to_string()),
        ("linux", "arm") => Ok("armv7-unknown-linux-gnueabihf".to_string()),
        ("linux", "arm64") => Ok("aarch64-unknown-linux-gnu".to_string()),
        ("darwin", "arm64") => Ok("aarch64-apple-darwin".to_string()),
        ("wasm", "wasm32") => Ok("wasm32-unknown-unknown".to_string()),
        _ => Err(format!("Unsupported target: {}:{}", os, arch)),
    }
}

// Represents the type of heap allocation for CTMM
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AllocationType {
    Array,
    HashTable,
}

// Helper struct to manage compilation state for a single function
struct FunctionCompiler<'a, M: Module> {
    module: &'a mut M,
    function_ids: &'a mut HashMap<String, FuncId>, // Store FuncId directly
    builtin_registry: &'a BuiltinRegistry,       // Add registry
    environment: Rc<RefCell<CodegenEnvironment>>,
    free_func_id: FuncId, // For CTMM
    heap_allocations: Vec<(Value, AllocationType)>, // For CTMM
}

impl<'a, M: Module> FunctionCompiler<'a, M> {
    fn compile_program_statements(
        &mut self,
        builder: &mut FunctionBuilder,
        program: &ast::Program,
    ) -> Result<(), String> {
        let mut last_expr_val: Option<(Value, Type)> = None;
        let mut terminated_by_return = false; // New flag

        for stmt in &program.statements {
            if builder.is_unreachable() {
                break; // Block is already terminated, no more instructions can be added
            }
            match stmt {
                Statement::Expression(expr_stmt) => {
                    last_expr_val =
                        Some(self.compile_expression(builder, &expr_stmt.expression)?);
                }
                Statement::Let(let_stmt) => {
                    let name = let_stmt.name.value.clone();
                    let (value, ty) = self.compile_expression(builder, &let_stmt.value)?;

                    if builder.is_unreachable() {
                        // If the expression compilation terminated the block (e.g. an if that returns),
                        // we can't define the variable.
                        return Ok(());
                    }

                    if self.environment.borrow().store.contains_key(&name) {
                        return Err(format!(
                            "Variable '{}' is already declared in this scope.",
                            name
                        ));
                    }

                    let var = builder.declare_var(ty);
                    self.environment.borrow_mut().set(name, var, ty);
                    builder.def_var(var, value);
                }
                Statement::Return(ret_stmt) => {
                    let (value, ty) = self.compile_expression(builder, &ret_stmt.return_value)?;

                    if builder.is_unreachable() {
                        // The expression part of the return statement (e.g., an if-else that returns)
                        // has already terminated the block. We just need to mark that a return happened.
                        terminated_by_return = true;
                    } else {
                        // The main function must return I64. This might need more sophisticated handling
                        // for functions with different return types later.
                        if ty != types::I64 {
                            return Err(format!(
                                "Cannot return type {} from a function that returns I64.",
                                ty
                            ));
                        }
                        builder.ins().return_(&[value]);
                        terminated_by_return = true; // Set flag
                    }
                }
            }
        }

        // Free all heap allocations at the end of the function in reverse order of allocation
        if !builder.is_unreachable() {
            let free_func_ref = self.module.declare_func_in_func(self.free_func_id, builder.func);
            // Clone allocations to avoid borrowing issues with `self` inside the loop.
            let allocations = self.heap_allocations.clone();
            for (ptr, alloc_type) in allocations.iter().rev() {
                match alloc_type {
                    AllocationType::HashTable => {
                        self.free_hash_table(builder, *ptr)?;
                    }
                    AllocationType::Array => {
                        builder.ins().call(free_func_ref, &[*ptr]);
                    }
                }
            }
        }

        // Add a return if the block is not terminated by a return statement
        // and the builder itself hasn't marked the block as unreachable (e.g., by an implicit return from last expr)
        if !terminated_by_return && !builder.is_unreachable() {
            let return_val = match last_expr_val {
                Some((val, ty)) if ty == types::I64 => val,
                _ => builder.ins().iconst(types::I64, 0), // Default return 0
            };
            builder.ins().return_(&[return_val]);
        }
        Ok(())
    }

    fn compile_block_statement(
        &mut self,
        builder: &mut FunctionBuilder,
        block: &ast::BlockStatement,
    ) -> Result<Option<(Value, Type)>, String> {
        let mut last_val: Option<(Value, Type)> = None;

        for stmt in &block.statements {
            if builder.is_unreachable() {
                break;
            }
            match stmt {
                Statement::Expression(expr_stmt) => {
                    last_val = Some(self.compile_expression(builder, &expr_stmt.expression)?);
                }
                Statement::Let(let_stmt) => {
                    let name = let_stmt.name.value.clone();
                    let (value, ty) = self.compile_expression(builder, &let_stmt.value)?;
                    if self.environment.borrow().store.contains_key(&name) {
                        return Err(format!(
                            "Variable '{}' is already declared in this scope.",
                            name
                        ));
                    }
                    let var = builder.declare_var(ty);
                    self.environment.borrow_mut().set(name, var, ty);
                    builder.def_var(var, value);
                    last_val = None;
                }
                Statement::Return(ret_stmt) => {
                    let (value, ty) = self.compile_expression(builder, &ret_stmt.return_value)?;
                    if ty != types::I64 {
                        return Err(format!(
                            "Cannot return type {} from a function that returns I64.",
                            ty
                        ));
                    }
                    builder.ins().return_(&[value]);
                    return Ok(None); // Block terminated, return None for the value.
                }
            }
        }

        if builder.is_unreachable() {
            Ok(None)
        } else {
            // If the block is reachable and didn't end on an expression, it defaults to 0.
            Ok(Some(
                last_val.unwrap_or_else(|| (builder.ins().iconst(types::I64, 0), types::I64)),
            ))
        }
    }

    fn compile_expression(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &ExpressionEnum,
    ) -> Result<(Value, Type), String> {
        println!("Compiling expression: {:?}", expr);
        match expr {
            ExpressionEnum::IntegerLiteral(int_lit) => {
                Ok((builder.ins().iconst(types::I64, int_lit.value), types::I64))
            }
            ExpressionEnum::Boolean(boolean) => Ok((
                builder
                    .ins()
                    .iconst(types::I64, if boolean.value { 1 } else { 0 }),
                types::I64,
            )),
            ExpressionEnum::Identifier(ident) => {
                if let Some((var, ty)) = self.environment.borrow().get(&ident.value) {
                    Ok((builder.use_var(var), ty))
                } else {
                    Err(format!("Undefined variable: {}", ident.value))
                }
            }
            ExpressionEnum::Infix(infix_expr) => {
                if infix_expr.operator == InfixOperator::Assign {
                    let (right_value, right_ty) =
                        self.compile_expression(builder, &infix_expr.right)?;
                    if let ExpressionEnum::Identifier(ident) = &*infix_expr.left {
                        let (var, var_ty) = self
                            .environment
                            .borrow()
                            .get(&ident.value)
                            .ok_or_else(|| format!("Undefined variable: {}", ident.value))?;
                        if var_ty != right_ty {
                            return Err(format!("Mismatched types for assignment: variable {} has type {} but expression has type {}", ident.value, var_ty, right_ty));
                        }
                        builder.def_var(var, right_value);
                        Ok((right_value, right_ty))
                    } else {
                        Err("The left-hand side of an assignment must be an identifier".to_string())
                    }
                } else {
                    let (left_val, left_ty) = self.compile_expression(builder, &infix_expr.left)?;
                    if builder.is_unreachable() {
                        return Ok((builder.ins().iconst(types::I64, 0), types::I64));
                    }
                    let (right_val, right_ty) =
                        self.compile_expression(builder, &infix_expr.right)?;
                    if builder.is_unreachable() {
                        return Ok((builder.ins().iconst(types::I64, 0), types::I64));
                    }

                    if left_ty != right_ty {
                        return Err(format!(
                            "Mismatched types for infix operation: {} and {}",
                            left_ty, right_ty
                        ));
                    }

                    let one_const = builder.ins().iconst(types::I64, 1);
                    let zero_const = builder.ins().iconst(types::I64, 0);

                    match left_ty {
                        types::I64 => match infix_expr.operator {
                            InfixOperator::Plus => {
                                Ok((builder.ins().iadd(left_val, right_val), types::I64))
                            }
                            InfixOperator::Minus => {
                                Ok((builder.ins().isub(left_val, right_val), types::I64))
                            }
                            InfixOperator::Multiply => {
                                Ok((builder.ins().imul(left_val, right_val), types::I64))
                            }
                            InfixOperator::Divide => {
                                Ok((builder.ins().sdiv(left_val, right_val), types::I64))
                            }
                            InfixOperator::Eq => {
                                let cmp = builder.ins().icmp(IntCC::Equal, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::NotEq => {
                                let cmp = builder.ins().icmp(IntCC::NotEqual, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::Lt => {
                                let cmp = builder
                                    .ins()
                                    .icmp(IntCC::SignedLessThan, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::Gt => {
                                let cmp = builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThan, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::And => {
                                let result_type = types::I64;
                                let result_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, result_type.bytes(), 8));

                                let then_block = builder.create_block(); // For when left_val is true
                                let else_block = builder.create_block(); // For when left_val is false
                                let merge_block = builder.create_block();

                                builder.ins().brif(left_val, then_block, &[], else_block, &[]);

                                builder.switch_to_block(then_block);
                                let (right_val, _) = self.compile_expression(builder, &infix_expr.right)?;
                                let right_is_true = builder.ins().icmp_imm(IntCC::NotEqual, right_val, 0);
                                let right_bool = builder.ins().select(right_is_true, one_const, zero_const);
                                builder.ins().stack_store(right_bool, result_slot, 0);
                                builder.ins().jump(merge_block, &[]);
                                builder.seal_block(then_block);

                                builder.switch_to_block(else_block);
                                builder.ins().stack_store(zero_const, result_slot, 0); // Store 0 if left_val is false
                                builder.ins().jump(merge_block, &[]);
                                builder.seal_block(else_block);

                                builder.switch_to_block(merge_block);
                                let final_val = builder.ins().stack_load(result_type, result_slot, 0);
                                builder.seal_block(merge_block);
                                Ok((final_val, types::I64))
                            }
                            InfixOperator::Or => {
                                let result_type = types::I64;
                                let result_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, result_type.bytes(), 8));

                                let then_block = builder.create_block(); // For when left_val is true
                                let else_block = builder.create_block(); // For when left_val is false
                                let merge_block = builder.create_block();

                                builder.ins().brif(left_val, then_block, &[], else_block, &[]);

                                builder.switch_to_block(then_block);
                                builder.ins().stack_store(one_const, result_slot, 0); // Store 1 if left_val is true
                                builder.ins().jump(merge_block, &[]);
                                builder.seal_block(then_block);

                                builder.switch_to_block(else_block);
                                let (right_val, _) = self.compile_expression(builder, &infix_expr.right)?;
                                let right_is_true = builder.ins().icmp_imm(IntCC::NotEqual, right_val, 0);
                                let right_bool = builder.ins().select(right_is_true, one_const, zero_const);
                                builder.ins().stack_store(right_bool, result_slot, 0);
                                builder.ins().jump(merge_block, &[]);
                                builder.seal_block(else_block);

                                builder.switch_to_block(merge_block);
                                let final_val = builder.ins().stack_load(result_type, result_slot, 0);
                                builder.seal_block(merge_block);
                                Ok((final_val, types::I64))
                            }
                            InfixOperator::PlusEq
                            | InfixOperator::MinusEq
                            | InfixOperator::AsteriskEq
                            | InfixOperator::SlashEq => {
                                if let ExpressionEnum::Identifier(ident) = &*infix_expr.left {
                                    let (var, _) = self.environment.borrow().get(&ident.value).ok_or_else(
                                        || format!("Undefined variable: {}", ident.value),
                                    )?;
                                    let new_val = match infix_expr.operator {
                                        InfixOperator::PlusEq => builder.ins().iadd(left_val, right_val),
                                        InfixOperator::MinusEq => builder.ins().isub(left_val, right_val),
                                        InfixOperator::AsteriskEq => builder.ins().imul(left_val, right_val),
                                        InfixOperator::SlashEq => builder.ins().sdiv(left_val, right_val),
                                        _ => unreachable!(),
                                    };
                                    builder.def_var(var, new_val);
                                    Ok((new_val, types::I64))
                                } else {
                                    Err("The left-hand side of a compound assignment must be an identifier".to_string())
                                }
                            }
                            _ => Err(format!("Unsupported infix operator '{}' for type {}", infix_expr.operator, left_ty)),
                        },
                        types::F64 => match infix_expr.operator {
                            InfixOperator::Plus => Ok((builder.ins().fadd(left_val, right_val), types::F64)),
                            InfixOperator::Minus => Ok((builder.ins().fsub(left_val, right_val), types::F64)),
                            InfixOperator::Multiply => Ok((builder.ins().fmul(left_val, right_val), types::F64)),
                            InfixOperator::Divide => Ok((builder.ins().fdiv(left_val, right_val), types::F64)),
                            InfixOperator::Eq => {
                                let cmp = builder.ins().fcmp(FloatCC::Equal, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::NotEq => {
                                let cmp = builder.ins().fcmp(FloatCC::NotEqual, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::Lt => {
                                let cmp = builder
                                    .ins()
                                    .fcmp(FloatCC::LessThan, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            InfixOperator::Gt => {
                                let cmp = builder
                                    .ins()
                                    .fcmp(FloatCC::GreaterThan, left_val, right_val);
                                Ok((builder.ins().select(cmp, one_const, zero_const), types::I64))
                            }
                            _ => Err(format!("Unsupported infix operator '{}' for type {}", infix_expr.operator, left_ty)),
                        },
                        _ => Err(format!("Unsupported type for infix operation: {}", left_ty)),
                    }
                }
            }
            ExpressionEnum::Prefix(prefix_expr) => {
                let (right_val, right_ty) = self.compile_expression(builder, &prefix_expr.right)?;

                match prefix_expr.operator {
                    PrefixOperator::Bang => {
                        if right_ty != types::I64 {
                            return Err(format!("The '!' operator cannot be applied to type {}", right_ty));
                        }
                        let zero_const = builder.ins().iconst(types::I64, 0);
                        let one_const = builder.ins().iconst(types::I64, 1);
                        let is_zero = builder.ins().icmp(IntCC::Equal, right_val, zero_const);
                        Ok((builder.ins().select(is_zero, one_const, zero_const), types::I64))
                    }
                    PrefixOperator::Minus => match right_ty {
                        types::I64 => Ok((builder.ins().ineg(right_val), types::I64)),
                        types::F64 => Ok((builder.ins().fneg(right_val), types::F64)),
                        _ => Err(format!("The '-' operator cannot be applied to type {}", right_ty)),
                    },
                }
            }
            ExpressionEnum::If(if_expr) => {
                let result_type = types::I64;
                let result_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, result_type.bytes(), 8));

                let (condition_value, condition_ty) =
                    self.compile_expression(builder, &if_expr.condition)?;
                if condition_ty != types::I64 {
                    return Err(format!("If condition must be a boolean value (I64), but got {}", condition_ty));
                }

                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();

                // Add condition check - don't add instructions if current block is unreachable
                if !builder.is_unreachable() {
                    builder
                        .ins()
                        .brif(condition_value, then_block, &[], else_block, &[]);
                }

                let mut merge_block_has_predecessors = false;

                // --- THEN BLOCK ---
                builder.switch_to_block(then_block);
                if let Some((then_val, _)) = self.compile_block_statement(builder, &if_expr.consequence)? {
                    if !builder.is_unreachable() {
                        builder.ins().stack_store(then_val, result_slot, 0);
                        builder.ins().jump(merge_block, &[]);
                        merge_block_has_predecessors = true;
                    }
                }
                builder.seal_block(then_block);

                // --- ELSE BLOCK ---
                builder.switch_to_block(else_block);
                let else_result = if let Some(alternative) = &if_expr.alternative {
                    self.compile_block_statement(builder, alternative)?
                } else {
                    Some((builder.ins().iconst(result_type, 0), result_type)) // Default to 0 if no alternative
                };

                if let Some((else_val, _)) = else_result {
                    if !builder.is_unreachable() {
                        builder.ins().stack_store(else_val, result_slot, 0);
                        builder.ins().jump(merge_block, &[]);
                        merge_block_has_predecessors = true;
                    }
                }
                builder.seal_block(else_block);

                // --- MERGE BLOCK ---
                builder.switch_to_block(merge_block);
                if merge_block_has_predecessors {
                    let final_result = builder.ins().stack_load(result_type, result_slot, 0);
                    builder.seal_block(merge_block);
                    Ok((final_result, result_type))
                } else {
                    // Both branches returned or are unreachable - add dummy terminator
                    let dummy_result = builder.ins().iconst(result_type, 0);
                    builder.ins().return_(&[dummy_result]); // Add terminator to prevent "not filled" error
                    builder.seal_block(merge_block);
                    Ok((dummy_result, result_type))
                }
            }
            ExpressionEnum::Function(func_lit) => {
                // 1. Create a new Cranelift Signature for the function
                let mut sig = Signature::new(CallConv::SystemV);
                sig.returns.push(AbiParam::new(types::I64)); // Assume functions return I64 for now

                // Add parameters to signature
                for _param in &func_lit.parameters {
                    sig.params.push(AbiParam::new(types::I64)); // Assume all params are I64 for now
                }

                // 2. Declare the function in the module
                let func_name = func_lit.name.value.clone();
                let func_id = self
                    .module
                    .declare_function(&func_name, Linkage::Local, &sig)
                    .map_err(|e| e.to_string())?;
                self.function_ids.insert(func_name.clone(), func_id); // Store FuncId directly

                // 3. Create a new Cranelift Context and FunctionBuilder for the function's body
                let mut func_ctx = self.module.make_context();
                func_ctx.func.signature = sig; // Assign the new signature

                let mut func_builder_context = FunctionBuilderContext::new(); // NEW CONTEXT FOR NESTED FUNCTION
                let mut func_builder = FunctionBuilder::new(&mut func_ctx.func, &mut func_builder_context);
                let entry_block = func_builder.create_block();
                func_builder.switch_to_block(entry_block);

                // 4. Set up a new enclosed CodegenEnvironment for the function's scope
                let func_env = Rc::new(RefCell::new(CodegenEnvironment::new_enclosed_environment(Rc::clone(&self.environment))));

                // 5. Declare parameters as variables in the new environment
                func_builder.append_block_params_for_function_params(entry_block);
                for (i, param_ident) in func_lit.parameters.iter().enumerate() {
                    let param_var = func_builder.declare_var(types::I64);
                    let arg_value = func_builder.block_params(entry_block)[i];
                    func_env
                        .borrow_mut()
                        .set(param_ident.value.clone(), param_var, types::I64);
                    func_builder.def_var(param_var, arg_value);
                }

                // 6. Compile the function body (BlockStatement)
                let mut func_compiler_instance = FunctionCompiler {
                    module: self.module,
                    function_ids: self.function_ids,
                    builtin_registry: self.builtin_registry,
                    environment: func_env.clone(),
                    free_func_id: self.free_func_id,
                    heap_allocations: Vec::new(),
                };
                func_compiler_instance.compile_program_statements(&mut func_builder, &ast::Program { statements: func_lit.body.statements.clone() })?;

                func_;        // builder.seal_block(resize_block);
                func_builder.finalize();

                // 7. Define the function in the module
                self.module.define_function(func_id, &mut func_ctx).map_err(|e| e.to_string())?;

                // A function literal evaluates to 0 (its address or ID is not directly usable as a value)
                Ok((builder.ins().iconst(types::I64, 0), types::I64))
            }
            ExpressionEnum::Call(call_expr) => {
                let func_name = if let ExpressionEnum::Identifier(ident) = &*call_expr.function {
                    ident.value.clone()
                } else {
                    return Err("Only direct function calls by identifier are supported for now.".to_string());
                };

                // Compile arguments first
                let mut compiled_args = Vec::new();
                for arg_expr in &call_expr.arguments {
                    compiled_args.push(self.compile_expression(builder, arg_expr)?);
                }
                let mut arg_values: Vec<Value> = compiled_args.iter().map(|(v, _)| *v).collect();

                // Check if it's a built-in or user-defined function
                if let Some(builtin) = self.builtin_registry.lookup(&func_name) {
                    // --- BUILT-IN FUNCTION CALL ---
                    let func_id = self.function_ids.get(&func_name).unwrap(); // Should exist
                    let func_ref = self.module.declare_func_in_func(*func_id, builder.func);

                    // Type check arguments
                    if compiled_args.len() != builtin.signature.params.len() {
                        return Err(format!("Built-in function '{}' expects {} arguments, but {} were provided.", func_name, builtin.signature.params.len(), compiled_args.len()));
                    }

                    let ptr_type = self.module.target_config().pointer_type();
                    for (i, (_arg_val, arg_ty)) in compiled_args.iter().enumerate() {
                        let expected_fystan_ty = &builtin.signature.params[i];
                        let expected_cranelift_ty = expected_fystan_ty.to_cranelift_type(ptr_type);
                        if *arg_ty != expected_cranelift_ty {
                            return Err(format!("Type error in argument {} for function '{}': expected {:?}, got {}", i + 1, func_name, expected_fystan_ty, arg_ty));
                        }
                    }

                    // Special handling for string length argument for 'print'
                    if func_name == "print" {
                        let string_len = if let ExpressionEnum::StringLiteral(str_lit) = &call_expr.arguments[0] {
                            str_lit.value.len() as u64
                        } else {
                            // This could be improved to calculate length of non-literals
                            return Err("'print' function currently only supports string literals as arguments.".to_string());
                        };
                        let len_val = builder.ins().iconst(types::I64, string_len as i64);
                        arg_values.push(len_val);
                    }

                    let call_inst = builder.ins().call(func_ref, &arg_values);
                    let result_val = builder.inst_results(call_inst)[0];
                    let return_ty = builtin.signature.returns.to_cranelift_type(ptr_type);

                    Ok((result_val, return_ty))
                } else if let Some(func_id) = self.function_ids.get(&func_name) {
                    // --- USER-DEFINED FUNCTION CALL ---
                    let func_decl = self.module.declarations().get_function_decl(*func_id);
                    let func_sig = &func_decl.signature;
                    let expected_param_count = func_sig.params.len();
                    let expected_param_types: Vec<Type> = func_sig.params.iter().map(|p| p.value_type).collect();
                    let return_ty = func_sig.returns[0].value_type;
                    let func_ref = self.module.declare_func_in_func(*func_id, builder.func);

                    // Check argument count
                    if compiled_args.len() != expected_param_count {
                        return Err(format!("Function '{}' expects {} arguments, but {} were provided.", func_name, expected_param_count, compiled_args.len()));
                    }

                    // Type check arguments
                    for (i, (_arg_val, arg_ty)) in compiled_args.iter().enumerate() {
                        let expected_ty = expected_param_types[i];
                        if *arg_ty != expected_ty {
                            return Err(format!("Type error in argument {} for function '{}': expected {}, got {}", i + 1, func_name, expected_ty, arg_ty));
                        }
                    }

                    let call_inst = builder.ins().call(func_ref, &arg_values);
                    let result_val = builder.inst_results(call_inst)[0];

                    Ok((result_val, return_ty))
                } else {
                    Err(format!("Undefined function: {}", func_name))
                }
            }
            ExpressionEnum::StringLiteral(str_lit) => {
                let string_bytes = str_lit.value.as_bytes();
                let data_id = self.module.declare_anonymous_data(true, false).map_err(|e| e.to_string())?;
                let mut data_desc = DataDescription::new();
                data_desc.init = Init::Bytes { contents: string_bytes.into() };
                self.module.define_data(data_id, &data_desc).map_err(|e| e.to_string())?;
                let global_value = self.module.declare_data_in_func(data_id, builder.func);
                let ptr_type = self.module.target_config().pointer_type();
                let ptr_val = builder.ins().global_value(ptr_type, global_value);
                Ok((ptr_val, ptr_type))
            }
            ExpressionEnum::FloatLiteral(float_lit) => {
                Ok((builder.ins().f64const(float_lit.value), types::F64))
            }
            ExpressionEnum::ArrayLiteral(arr_lit) => {
                let ptr_type = self.module.target_config().pointer_type();
                let element_type = types::I64; // Assuming homogeneous arrays of i64 for now
                let element_size = element_type.bytes() as i64;

                let num_elements = arr_lit.elements.len() as i64;
                let alloc_size_val = 8 + num_elements * element_size; // 8 bytes for length (i64)
                let alloc_size = builder.ins().iconst(ptr_type, alloc_size_val);

                // Declare malloc
                let mut malloc_sig = Signature::new(CallConv::SystemV);
                malloc_sig.params.push(AbiParam::new(ptr_type));
                malloc_sig.returns.push(AbiParam::new(ptr_type));
                let malloc_func = self.module.declare_function("malloc", Linkage::Import, &malloc_sig).map_err(|e| e.to_string())?;
                let local_malloc_func = self.module.declare_func_in_func(malloc_func, builder.func);

                // Call malloc
                let call = builder.ins().call(local_malloc_func, &[alloc_size]);
                let base_ptr = builder.inst_results(call)[0];

                // CTMM: Track this allocation
                self.heap_allocations.push((base_ptr, AllocationType::Array));

                // Store length
                let num_elements_val = builder.ins().iconst(types::I64, num_elements);
                builder.ins().store(MemFlags::new(), num_elements_val, base_ptr, 0);

                // Store elements
                for (i, elem_expr) in arr_lit.elements.iter().enumerate() {
                    let (elem_val, elem_ty) = self.compile_expression(builder, elem_expr)?;
                    if builder.is_unreachable() {
                        // If compiling an element terminated the block, we can't continue.
                        break;
                    }
                    // TODO: Type check elements. For now, assume I64.
                    if elem_ty != element_type {
                        return Err(format!("Array elements must be of type {}. Found {}.", element_type, elem_ty));
                    }
                    let offset = 8 + (i as i64 * element_size);
                    builder.ins().store(MemFlags::new(), elem_val, base_ptr, offset as i32);
                }

                Ok((base_ptr, ptr_type))
            }
            ExpressionEnum::IndexExpression(idx_expr) => {
                let ptr_type = self.module.target_config().pointer_type();
                let i64_type = types::I64;
                let element_size: i64 = 8;

                // Compile the array (left expression) and the index
                let (array_ptr, array_ty) = self.compile_expression(builder, &idx_expr.left)?;
                if builder.is_unreachable() {
                    return Ok((builder.ins().iconst(types::I64, 0), types::I64));
                }
                let (index_val, index_ty) = self.compile_expression(builder, &idx_expr.index)?;
                if builder.is_unreachable() {
                    return Ok((builder.ins().iconst(types::I64, 0), types::I64));
                }

                // Type checking
                if array_ty != ptr_type {
                    return Err(format!("Expected array (pointer type) for index expression, but got {}", array_ty));
                }
                if index_ty != types::I64 {
                    return Err(format!("Index expression must be an integer, but got {}", index_ty));
                }

                // NO BOUNDS CHECKING FOR NOW
                let index_offset = builder.ins().imul_imm(index_val, element_size);
                let element_base_offset = builder.ins().iadd_imm(index_offset, 8);
                let element_addr = builder.ins().iadd(array_ptr, element_base_offset);
                let value = builder.ins().load(i64_type, MemFlags::new(), element_addr, 0);

                Ok((value, i64_type))
            }
            ExpressionEnum::HashLiteral(hash_lit) => {
                // A proper implementation would involve:
                // 1. A robust hash function for strings. (DONE)
                // 2. Collision handling (e.g., chaining with linked lists). (DONE)
                // 3. Dynamic resizing when the load factor is too high. (DONE)
                // 4. Freeing the hash table and all its keys/values. (DONE)

                let ptr_type = self.module.target_config().pointer_type();
                let i64_type = types::I64;
                let i64_size = i64_type.bytes() as i64;

                // For now, let's use a fixed capacity.
                let capacity = 16; // Must be a power of 2 for simple modulo
                let capacity_val = builder.ins().iconst(i64_type, capacity);

                // Size of the main HashTable struct: count (i64) + capacity (i64) + buckets_ptr (*mut)
                let struct_size = i64_size * 2 + ptr_type.bytes() as i64;

                // Size of the buckets array: capacity * sizeof(pointer)
                let buckets_array_size = capacity * ptr_type.bytes() as i64;

                let total_alloc_size = struct_size + buckets_array_size;
                let alloc_size_val = builder.ins().iconst(ptr_type, total_alloc_size);

                // Declare and call malloc
                let mut malloc_sig = Signature::new(CallConv::SystemV);
                malloc_sig.params.push(AbiParam::new(ptr_type));
                malloc_sig.returns.push(AbiParam::new(ptr_type));
                let malloc_func = self.module.declare_function("malloc", Linkage::Import, &malloc_sig).map_err(|e| e.to_string())?;
                let local_malloc_func = self.module.declare_func_in_func(malloc_func, builder.func);
                let call = builder.ins().call(local_malloc_func, &[alloc_size_val]);
                let table_ptr = builder.inst_results(call)[0];

                // CTMM: Track this allocation
                self.heap_allocations.push((table_ptr, AllocationType::HashTable));

                // Initialize the HashTable struct
                // 1. Store count = 0
                let count_val = builder.ins().iconst(i64_type, 0);
                builder.ins().store(MemFlags::new(), count_val, table_ptr, 0);

                // 2. Store capacity
                builder.ins().store(MemFlags::new(), capacity_val, table_ptr, i64_size as i32);

                // 3. Store pointer to the buckets array (which starts right after the struct)
                let buckets_ptr = builder.ins().iadd_imm(table_ptr, struct_size);
                builder.ins().store(MemFlags::new(), buckets_ptr, table_ptr, (i64_size * 2) as i32);

                // Initialize all bucket pointers to null
                let null_ptr = builder.ins().iconst(ptr_type, 0);
                for i in 0..capacity {
                    let offset = (i * ptr_type.bytes() as i64) as i32;
                    builder.ins().store(MemFlags::new(), null_ptr, buckets_ptr, offset);
                }

                // Now, iterate through the pairs and insert them.
                for (key_string, value_expr) in &hash_lit.pairs {
                    // The parser ensures keys are strings. We compile the string to get a pointer.
                    let (key_ptr, key_ty) = self.compile_expression(builder, &ExpressionEnum::StringLiteral(ast::StringLiteral { token: crate::lexer::token::Token::new(crate::lexer::token::TokenType::String, key_string.clone()), value: key_string.clone() }))?;
                    if key_ty != ptr_type {
                        return Err("Hash key must be a string.".to_string());
                    }

                    // Compile the value expression to get its value and type
                    let value_tuple = self.compile_expression(builder, value_expr)?;

                    // Call the new hash_insert with the (value, type) tuple
                    self.hash_insert(builder, table_ptr, key_ptr, value_tuple)?;
                    
                    if builder.is_unreachable() {
                        break;
                    }
                }

                Ok((table_ptr, ptr_type))
            }
        }
    }

    fn hash_string(&mut self, builder: &mut FunctionBuilder, string_ptr: Value) -> Result<Value, String> {
        let ptr_type = self.module.target_config().pointer_type();
        let i64_type = types::I64;

        let fnv_offset_basis = builder.ins().iconst(i64_type, 0xcbf29ce484222325u64 as i64);
        let fnv_prime = builder.ins().iconst(i64_type, 0x100000001b3 as i64);

        let hash_val = builder.declare_var(i64_type);
        builder.def_var(hash_val, fnv_offset_basis);

        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let exit_block = builder.create_block();

        let char_ptr = builder.declare_var(ptr_type);
        builder.def_var(char_ptr, string_ptr);

        builder.ins().jump(loop_header, &[]);
        builder.switch_to_block(loop_header);

        let current_char_ptr = builder.use_var(char_ptr);
        let current_char = builder.ins().load(types::I8, MemFlags::new(), current_char_ptr, 0);
        let is_null = builder.ins().icmp_imm(IntCC::Equal, current_char, 0);
        builder.ins().brif(is_null, exit_block, &[], loop_body, &[]);

        builder.switch_to_block(loop_body);
        let current_hash = builder.use_var(hash_val);
        let extended_char = builder.ins().uextend(i64_type, current_char);
        let updated_hash = builder.ins().bxor(current_hash, extended_char);
        let final_hash = builder.ins().imul(updated_hash, fnv_prime);
        builder.def_var(hash_val, final_hash);

        let next_char_ptr = builder.ins().iadd_imm(current_char_ptr, 1);
        builder.def_var(char_ptr, next_char_ptr);
        builder.ins().jump(loop_header, &[]);

        builder.seal_block(loop_header);
        builder.seal_block(loop_body);

        builder.switch_to_block(exit_block);
        builder.seal_block(exit_block);

        Ok(builder.use_var(hash_val))
    }

    fn hash_insert(
        &mut self,
        builder: &mut FunctionBuilder,
        table_ptr: Value,
        key_ptr: Value,
        value_tuple: (Value, Type),
    ) -> Result<(), String> {
        let ptr_type = self.module.target_config().pointer_type();
        let i64_type = types::I64;
        let i64_size = i64_type.bytes() as i64;

        // Type Tags
        const I64_TAG: i64 = 1;
        const F64_TAG: i64 = 2;
        const PTR_TAG: i64 = 3;

        // Check for resize
        let count = builder.ins().load(i64_type, MemFlags::new(), table_ptr, 0);
        let capacity = builder.ins().load(i64_type, MemFlags::new(), table_ptr, i64_size as i32);
        let count_plus_one = builder.ins().iadd_imm(count, 1);
        let lhs = builder.ins().imul_imm(count_plus_one, 4);
        let rhs = builder.ins().imul_imm(capacity, 3);
        let needs_resize = builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs);
        let resize_block = builder.create_block();
        let continue_block = builder.create_block();
        let current_block = builder.current_block().unwrap();
        builder.ins().brif(needs_resize, resize_block, &[], continue_block, &[]);
        builder.seal_block(current_block);

        builder.switch_to_block(resize_block);
        self.hash_resize(builder, table_ptr)?;
        if !builder.is_unreachable() {
            builder.ins().jump(continue_block, &[]);
        }
        // builder.seal_block(resize_block);

        builder.switch_to_block(continue_block);

        // Allocate memory for the new entry {key_ptr, tag, value, next_ptr}
        let entry_size = ptr_type.bytes() as i64 * 2 + i64_size * 2; // ptr, i64, i64, ptr
        let alloc_size = builder.ins().iconst(ptr_type, entry_size);
        let mut malloc_sig = Signature::new(CallConv::SystemV);
        malloc_sig.params.push(AbiParam::new(ptr_type));
        malloc_sig.returns.push(AbiParam::new(ptr_type));
        let malloc_func = self.module.declare_function("malloc", Linkage::Import, &malloc_sig).map_err(|e| e.to_string())?;
        let local_malloc_func = self.module.declare_func_in_func(malloc_func, builder.func);
        let call = builder.ins().call(local_malloc_func, &[alloc_size]);
        let entry_ptr = builder.inst_results(call)[0];

        // Get tag and value to store
        let (value, value_ty) = value_tuple;
        let (tag, val_to_store) = if value_ty == i64_type {
            (I64_TAG, value)
        } else if value_ty == types::F64 {
            (F64_TAG, builder.ins().bitcast(i64_type, MemFlags::new(), value))
        } else if value_ty == ptr_type {
            (PTR_TAG, value)
        } else {
            return Err(format!("Unsupported type in hash literal: {}", value_ty));
        };
        let tag_val = builder.ins().iconst(i64_type, tag);

        // Store key, tag, value, and next (initially null)
        let key_offset = 0;
        let tag_offset = key_offset + ptr_type.bytes() as i32;
        let value_offset = tag_offset + i64_size as i32;
        let next_offset = value_offset + i64_size as i32;

        builder.ins().store(MemFlags::new(), key_ptr, entry_ptr, key_offset);
        builder.ins().store(MemFlags::new(), tag_val, entry_ptr, tag_offset);
        builder.ins().store(MemFlags::new(), val_to_store, entry_ptr, value_offset);
        let null_ptr = builder.ins().iconst(ptr_type, 0);
        builder.ins().store(MemFlags::new(), null_ptr, entry_ptr, next_offset);

        // Get hash and index
        let updated_capacity = builder.ins().load(i64_type, MemFlags::new(), table_ptr, i64_size as i32);
        let hash_val = self.hash_string(builder, key_ptr)?;
        let index = builder.ins().urem(hash_val, updated_capacity);

        // Get buckets array pointer
        let buckets_ptr_ptr = builder.ins().iadd_imm(table_ptr, i64_size * 2);
        let buckets_ptr = builder.ins().load(ptr_type, MemFlags::new(), buckets_ptr_ptr, 0);

        // Get current entry at index
        let index_offset = builder.ins().imul_imm(index, ptr_type.bytes() as i64);
        let bucket_addr = builder.ins().iadd(buckets_ptr, index_offset);
        let current_entry = builder.ins().load(ptr_type, MemFlags::new(), bucket_addr, 0);

        // Link new entry
        builder.ins().store(MemFlags::new(), current_entry, entry_ptr, next_offset);
        builder.ins().store(MemFlags::new(), entry_ptr, bucket_addr, 0);

        // Increment count
        let current_count = builder.ins().load(i64_type, MemFlags::new(), table_ptr, 0);
        let new_count = builder.ins().iadd_imm(current_count, 1);
        builder.ins().store(MemFlags::new(), new_count, table_ptr, 0);

        Ok(())
    }

    fn hash_resize(
        &mut self,
        builder: &mut FunctionBuilder,
        table_ptr: Value,
    ) -> Result<(), String> {
        let ptr_type = self.module.target_config().pointer_type();
        let i64_type = types::I64;
        let i64_size = i64_type.bytes() as i64;

        // Get old capacity and buckets
        let old_capacity = builder.ins().load(i64_type, MemFlags::new(), table_ptr, i64_size as i32);
        let buckets_ptr_addr = builder.ins().iadd_imm(table_ptr, i64_size * 2);
        let old_buckets_ptr = builder.ins().load(ptr_type, MemFlags::new(), buckets_ptr_addr, 0);

        // Calculate new capacity
        let new_capacity = builder.ins().imul_imm(old_capacity, 2);

        // Allocate new buckets array
        let new_buckets_size = builder.ins().imul_imm(new_capacity, ptr_type.bytes() as i64);
        let mut malloc_sig = Signature::new(CallConv::SystemV);
        malloc_sig.params.push(AbiParam::new(ptr_type));
        malloc_sig.returns.push(AbiParam::new(ptr_type));
        let malloc_func = self.module.declare_function("malloc", Linkage::Import, &malloc_sig).map_err(|e| e.to_string())?;
        let local_malloc_func = self.module.declare_func_in_func(malloc_func, builder.func);
        let call = builder.ins().call(local_malloc_func, &[new_buckets_size]);
        let new_buckets_ptr = builder.inst_results(call)[0];

        // Initialize new buckets to null
        let loop_counter = builder.declare_var(i64_type);
        let initial_loop_counter = builder.ins().iconst(i64_type, 0);
        builder.def_var(loop_counter, initial_loop_counter);
        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let loop_exit = builder.create_block();
        builder.ins().jump(loop_header, &[]);

        builder.switch_to_block(loop_header);
        let current_i = builder.use_var(loop_counter);
        let condition = builder.ins().icmp(IntCC::SignedLessThan, current_i, new_capacity);
        builder.ins().brif(condition, loop_body, &[], loop_exit, &[]);
        builder.seal_block(loop_header);

        builder.switch_to_block(loop_body);
        let offset = builder.ins().imul_imm(current_i, ptr_type.bytes() as i64);
        let bucket_addr = builder.ins().iadd(new_buckets_ptr, offset);
        let null_ptr = builder.ins().iconst(ptr_type, 0);
        builder.ins().store(MemFlags::new(), null_ptr, bucket_addr, 0);
        let next_i = builder.ins().iadd_imm(current_i, 1);
        builder.def_var(loop_counter, next_i);
        builder.ins().jump(loop_header, &[]);
        builder.seal_block(loop_body);

        builder.switch_to_block(loop_exit);
        let after_init_loop = builder.create_block();
        builder.ins().jump(after_init_loop, &[]);
        builder.seal_block(loop_exit);
        builder.switch_to_block(after_init_loop);

        // Rehash and move entries
        let outer_loop_counter = builder.declare_var(i64_type);
        let initial_outer_loop_counter = builder.ins().iconst(i64_type, 0);
        builder.def_var(outer_loop_counter, initial_outer_loop_counter);
        let outer_header = builder.create_block();
        let outer_body = builder.create_block();
        let outer_exit = builder.create_block();
        builder.ins().jump(outer_header, &[]);

        builder.switch_to_block(outer_header);
        let i = builder.use_var(outer_loop_counter);
        let outer_cond = builder.ins().icmp(IntCC::SignedLessThan, i, old_capacity);
        builder.ins().brif(outer_cond, outer_body, &[], outer_exit, &[]);
        builder.seal_block(outer_header);

        builder.switch_to_block(outer_body);
        // Inner loop setup
        let entry_ptr_var = builder.declare_var(ptr_type);
        let old_offset = builder.ins().imul_imm(i, ptr_type.bytes() as i64);
        let old_bucket_addr = builder.ins().iadd(old_buckets_ptr, old_offset);
        let entry_ptr = builder.ins().load(ptr_type, MemFlags::new(), old_bucket_addr, 0);
        builder.def_var(entry_ptr_var, entry_ptr);

        let inner_header = builder.create_block();
        let inner_body = builder.create_block();
        let inner_exit = builder.create_block();
        builder.ins().jump(inner_header, &[]);

        builder.switch_to_block(inner_header);
        let current_entry_ptr = builder.use_var(entry_ptr_var);
        let is_null = builder.ins().icmp_imm(IntCC::Equal, current_entry_ptr, 0);
        builder.ins().brif(is_null, inner_exit, &[], inner_body, &[]);
        builder.seal_block(inner_header);

        builder.switch_to_block(inner_body);
        let next_entry_ptr_addr = builder.ins().iadd_imm(current_entry_ptr, (ptr_type.bytes() + i64_type.bytes()) as i64);
        let next_entry_ptr = builder.ins().load(ptr_type, MemFlags::new(), next_entry_ptr_addr, 0);
        let key_ptr = builder.ins().load(ptr_type, MemFlags::new(), current_entry_ptr, 0);
        let hash_val = self.hash_string(builder, key_ptr)?;
        let new_index = builder.ins().urem(hash_val, new_capacity);
        let new_offset = builder.ins().imul_imm(new_index, ptr_type.bytes() as i64);
        let new_bucket_addr = builder.ins().iadd(new_buckets_ptr, new_offset);
        let head = builder.ins().load(ptr_type, MemFlags::new(), new_bucket_addr, 0);
        builder.ins().store(MemFlags::new(), head, next_entry_ptr_addr, 0);
        builder.ins().store(MemFlags::new(), current_entry_ptr, new_bucket_addr, 0);
        builder.def_var(entry_ptr_var, next_entry_ptr);
        builder.ins().jump(inner_header, &[]);
        builder.seal_block(inner_body);

        builder.switch_to_block(inner_exit);
        let after_inner_loop = builder.create_block();
        builder.ins().jump(after_inner_loop, &[]);
        builder.seal_block(inner_exit);
        builder.switch_to_block(after_inner_loop);

        // Continue outer loop
        let next_i_outer = builder.ins().iadd_imm(i, 1);
        builder.def_var(outer_loop_counter, next_i_outer);
        builder.ins().jump(outer_header, &[]);
        builder.seal_block(outer_body);

        builder.switch_to_block(outer_exit);
        let after_outer_free_loop = builder.create_block();
        builder.ins().jump(after_outer_free_loop, &[]);
        builder.seal_block(outer_exit);
        builder.seal_block(outer_header);
        builder.switch_to_block(after_outer_free_loop);

        // Free old buckets array
        let free_func_ref = self.module.declare_func_in_func(self.free_func_id, builder.func);
        builder.ins().call(free_func_ref, &[old_buckets_ptr]);

        // Update table struct
        builder.ins().store(MemFlags::new(), new_capacity, table_ptr, i64_size as i32);
        builder.ins().store(MemFlags::new(), new_buckets_ptr, buckets_ptr_addr, 0);

        Ok(())
    }

    fn free_hash_table(
        &mut self,
        builder: &mut FunctionBuilder,
        table_ptr: Value,
    ) -> Result<(), String> {
        let ptr_type = self.module.target_config().pointer_type();
        let i64_type = types::I64;
        let i64_size = i64_type.bytes() as i64;
        let free_func_ref = self.module.declare_func_in_func(self.free_func_id, builder.func);

        // Get capacity and buckets pointer
        let capacity = builder.ins().load(i64_type, MemFlags::new(), table_ptr, i64_size as i32);
        let buckets_ptr_addr = builder.ins().iadd_imm(table_ptr, i64_size * 2);
        let buckets_ptr = builder.ins().load(ptr_type, MemFlags::new(), buckets_ptr_addr, 0);

        // Loop through buckets
        let outer_loop_counter = builder.declare_var(i64_type);
        let initial_outer_loop_counter = builder.ins().iconst(i64_type, 0);
        builder.def_var(outer_loop_counter, initial_outer_loop_counter);
        let outer_header = builder.create_block();
        let outer_body = builder.create_block();
        let outer_exit = builder.create_block();
        builder.ins().jump(outer_header, &[]);
        builder.switch_to_block(outer_header);
        let i = builder.use_var(outer_loop_counter);
        let outer_cond = builder.ins().icmp(IntCC::SignedLessThan, i, capacity);
        builder.ins().brif(outer_cond, outer_body, &[], outer_exit, &[]);
        builder.seal_block(outer_header);
        builder.switch_to_block(outer_body);

        // Inner loop to free entries in the chain
        let entry_ptr_var = builder.declare_var(ptr_type);
        let offset = builder.ins().imul_imm(i, ptr_type.bytes() as i64);
        let bucket_addr = builder.ins().iadd(buckets_ptr, offset);
        let entry_ptr = builder.ins().load(ptr_type, MemFlags::new(), bucket_addr, 0);
        builder.def_var(entry_ptr_var, entry_ptr);

        let inner_header = builder.create_block();
        let inner_body = builder.create_block();
        let inner_exit = builder.create_block();
        builder.ins().jump(inner_header, &[]);

        builder.switch_to_block(inner_header);
        let current_entry_ptr = builder.use_var(entry_ptr_var);
        let is_null = builder.ins().icmp_imm(IntCC::Equal, current_entry_ptr, 0);
        builder.ins().brif(is_null, inner_exit, &[], inner_body, &[]);
        builder.seal_block(inner_header);

        builder.switch_to_block(inner_body);
        let next_entry_ptr_addr = builder.ins().iadd_imm(current_entry_ptr, (ptr_type.bytes() + i64_type.bytes()) as i64);
        let next_entry_ptr = builder.ins().load(ptr_type, MemFlags::new(), next_entry_ptr_addr, 0);
        let key_ptr = builder.ins().load(ptr_type, MemFlags::new(), current_entry_ptr, 0);
        builder.ins().call(free_func_ref, &[key_ptr]);
        builder.ins().call(free_func_ref, &[current_entry_ptr]);
        builder.def_var(entry_ptr_var, next_entry_ptr);
        builder.ins().jump(inner_header, &[]);
        builder.seal_block(inner_body);

        builder.switch_to_block(inner_exit);
        let after_inner_free_loop = builder.create_block();
        builder.ins().jump(after_inner_free_loop, &[]);
        builder.seal_block(inner_exit);
        builder.switch_to_block(after_inner_free_loop);

        // Continue outer loop
        let next_i_outer = builder.ins().iadd_imm(i, 1);
        builder.def_var(outer_loop_counter, next_i_outer);
        builder.ins().jump(outer_header, &[]);
        builder.seal_block(outer_body);

        builder.switch_to_block(outer_exit);
        let after_outer_free_loop = builder.create_block();
        builder.ins().jump(after_outer_free_loop, &[]);
        builder.seal_block(outer_exit);
        builder.seal_block(outer_header);
        builder.switch_to_block(after_outer_free_loop);

        // Free the buckets array and the table itself
        builder.ins().call(free_func_ref, &[buckets_ptr]);
        builder.ins().call(free_func_ref, &[table_ptr]);

        Ok(())
    }
}