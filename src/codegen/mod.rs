use crate::ast::{BlockStatement, ExpressionEnum, ForExpression, InfixOperator, PrefixOperator, Program, Statement};
use crate::builtins;
use crate::parser::Parser;
use inkwell::llvm_sys::core::*; 
use inkwell::llvm_sys::prelude::*; 
use inkwell::llvm_sys::target::{LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets};
use inkwell::llvm_sys::target_machine::{LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMRelocMode, LLVMGetTargetFromTriple, LLVMTargetMachineEmitToFile};
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

// A struct to hold loop context for break/continue
pub struct LoopContext {
    cond_bb: LLVMBasicBlockRef,
    after_bb: LLVMBasicBlockRef,
}

use inkwell::llvm_sys::LLVMLinkage;

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    variables: HashMap<String, LLVMValueRef>,
    function: Option<LLVMValueRef>,
    loop_contexts: Vec<LoopContext>,
    printf_func: LLVMValueRef,
    fgets_func: LLVMValueRef,
    fgets_type: LLVMTypeRef,
    strlen_func: LLVMValueRef,
    strlen_type: LLVMTypeRef,
    stdin_ptr: LLVMValueRef,
    array_type: LLVMTypeRef,
}

impl Compiler {
    pub fn new(context: LLVMContextRef) -> Self {
        unsafe {
            let module = LLVMModuleCreateWithNameInContext(b"fystan_module\0".as_ptr() as *const _, context);
            let builder = LLVMCreateBuilderInContext(context);

            let i8_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
            let i32_type = LLVMInt32TypeInContext(context);
            let i64_type = LLVMInt64TypeInContext(context);

            let printf_type = LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 1);
            let printf_func = LLVMAddFunction(module, b"printf\0".as_ptr() as *const _, printf_type);

            let file_type = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
            let fgets_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i32_type, file_type].as_mut_ptr(), 3, 0);
            let fgets_func = LLVMAddFunction(module, b"fgets\0".as_ptr() as *const _, fgets_type);

            let strlen_type = LLVMFunctionType(i64_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let strlen_func = LLVMAddFunction(module, b"strlen\0".as_ptr() as *const _, strlen_type);

            let stdin_name = CString::new("stdin").unwrap();
            let stdin_ptr = LLVMAddGlobal(module, file_type, stdin_name.as_ptr());
            LLVMSetLinkage(stdin_ptr, LLVMLinkage::LLVMExternalLinkage);

            let array_struct_types = [LLVMPointerType(i64_type, 0), i64_type];
            let array_type = LLVMStructTypeInContext(context, array_struct_types.as_ptr() as *mut _, 2, 0);

            Compiler {
                context,
                module,
                builder,
                variables: HashMap::new(),
                function: None,
                loop_contexts: Vec::new(),
                printf_func,
                fgets_func,
                fgets_type,
                strlen_func,
                strlen_type,
                stdin_ptr,
                array_type,
            }
        }
    }

    pub fn run_from_source(source: &str, target_triple: &str, output_filename: &str) -> Result<(), String> {
        unsafe {
            let context = LLVMContextCreate();
            let mut compiler = Compiler::new(context);
            let l = crate::lexer::Lexer::new(source);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let errors = p.errors();
            if !errors.is_empty() {
                LLVMContextDispose(context);
                return Err(format!("Parser errors: {:?}", errors));
            }
            
            compiler.setup_test_main_function();

            compiler.compile(program)?;

            let last_block = LLVMGetLastBasicBlock(compiler.function.unwrap());
            if LLVMGetBasicBlockTerminator(last_block).is_null() {
                LLVMPositionBuilderAtEnd(compiler.builder, last_block);
                let zero = LLVMConstInt(LLVMInt64TypeInContext(context), 0, 0);
                LLVMBuildRet(compiler.builder, zero);
            }

            compiler.write_to_file(target_triple, output_filename)?;

            LLVMContextDispose(context);
            Ok(())
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<LLVMValueRef, String> {
        let mut result = ptr::null_mut();
        for statement in program.statements {
            result = self.compile_statement(statement)?;
        }
        Ok(result)
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<LLVMValueRef, String> {
        match statement {
            Statement::Expression(expr_stmt) => self.compile_expression(expr_stmt.expression),
            Statement::Return(ret_stmt) => {
                let value = self.compile_expression(ret_stmt.return_value)?;
                unsafe { LLVMBuildRet(self.builder, value) };
                Ok(value)
            }
            Statement::Break(_) => {
                if let Some(context) = self.loop_contexts.last() {
                    unsafe { LLVMBuildBr(self.builder, context.after_bb) };
                    unsafe { Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)) }
                } else {
                    Err("'break' outside of a loop".to_string())
                }
            }
            Statement::Continue(_) => {
                if let Some(context) = self.loop_contexts.last() {
                    unsafe { LLVMBuildBr(self.builder, context.cond_bb) };
                    unsafe { Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)) }
                } else {
                    Err("'continue' outside of a loop".to_string())
                }
            }
            Statement::Pass(_) => {
                // Pass statement does nothing, return a dummy value
                unsafe { Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)) }
            }
        }
    }

    fn compile_expression(&mut self, expression: ExpressionEnum) -> Result<LLVMValueRef, String> {
        match expression {
            ExpressionEnum::IntegerLiteral(lit) => unsafe {
                Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), lit.value as u64, 0))
            },
            ExpressionEnum::FloatLiteral(lit) => unsafe {
                Ok(LLVMConstReal(LLVMDoubleTypeInContext(self.context), lit.value))
            },
            ExpressionEnum::StringLiteral(lit) => unsafe {
                let c_string = CString::new(lit.value.as_str()).unwrap();
                let global_string = LLVMBuildGlobalStringPtr(self.builder, c_string.as_ptr(), b".str\0".as_ptr() as *const _);
                
                // Create a struct { i8*, i64 } for string (pointer, length)
                let string_len = lit.value.len() as u64;
                let string_len_val = LLVMConstInt(LLVMInt64TypeInContext(self.context), string_len, 0);

                let string_struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b".str_struct_ptr\0".as_ptr() as *const _);
                let ptr_field = LLVMBuildStructGEP2(self.builder, self.array_type, string_struct_ptr, 0, b".ptr_field\0".as_ptr() as *const _);
                let len_field = LLVMBuildStructGEP2(self.builder, self.array_type, string_struct_ptr, 1, b".len_field\0".as_ptr() as *const _);

                LLVMBuildStore(self.builder, global_string, ptr_field);
                LLVMBuildStore(self.builder, string_len_val, len_field);

                Ok(LLVMBuildLoad2(self.builder, self.array_type, string_struct_ptr, b".loaded_string_struct\0".as_ptr() as *const _))
            },
            ExpressionEnum::Boolean(b) => unsafe {
                Ok(LLVMConstInt(LLVMInt1TypeInContext(self.context), b.value as u64, 0))
            },
            ExpressionEnum::None(_) => unsafe {
                // None is represented as a null pointer
                Ok(LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)))
            },
            ExpressionEnum::Identifier(ident) => {
                if let Some(value) = self.variables.get(&ident.value) {
                    // Need to load based on the type of the alloca
                    let var_type = unsafe { LLVMGetAllocatedType(*value) };
                    unsafe { Ok(LLVMBuildLoad2(self.builder, var_type, *value, b"loadtmp\0".as_ptr() as *const _)) }
                } else {
                    Err(format!("Unknown variable: {}", ident.value))
                }
            }
            ExpressionEnum::Prefix(prefix_expr) => {
                let right = self.compile_expression(*prefix_expr.right)?;
                unsafe {
                    match prefix_expr.operator {
                        PrefixOperator::Not => {
                            let zero = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
                            Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntEQ, right, zero, b"booltmp\0".as_ptr() as *const _))
                        }
                        PrefixOperator::Minus => Ok(LLVMBuildNeg(self.builder, right, b"negtmp\0".as_ptr() as *const _)),
                    }
                }
            }
            ExpressionEnum::Infix(infix_expr) => {
                if infix_expr.operator == InfixOperator::Assign {
                    if let ExpressionEnum::Identifier(ident) = *infix_expr.left {
                        let value = self.compile_expression(*infix_expr.right)?;
                        let var = if let Some(var) = self.variables.get(&ident.value) {
                            *var
                        } else {
                            let value_type = unsafe { LLVMTypeOf(value) };
                            let alloca = self.create_entry_block_alloca(&ident.value, value_type);
                            self.variables.insert(ident.value.clone(), alloca);
                            alloca
                        };
                        unsafe { LLVMBuildStore(self.builder, value, var) };
                        return Ok(value);
                    } else {
                        return Err("Destination of assignment must be an identifier".to_string());
                    }
                }
                self.compile_infix_expression(*infix_expr.left, infix_expr.operator, *infix_expr.right)
            }
            ExpressionEnum::If(if_expr) => self.compile_if_expression(*if_expr.condition, if_expr.consequence, if_expr.alternative),
            ExpressionEnum::While(while_expr) => self.compile_while_expression(*while_expr.condition, while_expr.body),
            ExpressionEnum::For(for_expr) => self.compile_for_expression(for_expr),
            ExpressionEnum::Call(call_expr) => {
                if let ExpressionEnum::Identifier(ident) = *call_expr.function {
                    // Check for built-in functions first
                    if let Some(builtin) = builtins::get_builtin(&ident.value) {
                        return self.compile_builtin_call(builtin, call_expr.arguments);
                    }

                    // Handle user-defined functions
                    let function_name = CString::new(ident.value.as_str()).unwrap();
                    let function = self.get_or_create_function(&function_name, call_expr.arguments.len());
                    
                    let mut args = Vec::new();
                    for arg_expr in call_expr.arguments {
                        args.push(self.compile_expression(arg_expr)?);
                    }

                    unsafe {
                        let i64_type = LLVMInt64TypeInContext(self.context);
                        let mut arg_types: Vec<_> = (0..args.len()).map(|_| i64_type).collect();
                        let fn_type = LLVMFunctionType(i64_type, arg_types.as_mut_ptr(), args.len() as u32, 0);
                        Ok(LLVMBuildCall2(self.builder, fn_type, function, args.as_mut_ptr(), args.len() as u32, b"calltmp\0".as_ptr() as *const _))
                    }
                } else {
                    Err("Function name must be an identifier".to_string())
                }
            }
            ExpressionEnum::ArrayLiteral(arr_lit) => unsafe {
                let i64_type = LLVMInt64TypeInContext(self.context);
                let array_len = arr_lit.elements.len() as u64;
                let array_len_val = LLVMConstInt(i64_type, array_len, 0);

                // Allocate memory for elements on the heap (or stack for small arrays)
                let array_elements_ptr = LLVMBuildArrayMalloc(self.builder, i64_type, array_len_val, b"array_elements\0".as_ptr() as *const _);

                // Store elements
                for (i, element_expr) in arr_lit.elements.into_iter().enumerate() {
                    let element_val = self.compile_expression(element_expr)?;
                    let mut index_val = LLVMConstInt(i64_type, i as u64, 0);
                    let element_ptr = LLVMBuildGEP2(self.builder, i64_type, array_elements_ptr, &mut index_val, 1, b"element_ptr\0".as_ptr() as *const _);
                    LLVMBuildStore(self.builder, element_val, element_ptr);
                }

                // Create the array struct { i64*, i64 }
                let array_struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"array_struct_ptr\0".as_ptr() as *const _);
                let ptr_field = LLVMBuildStructGEP2(self.builder, self.array_type, array_struct_ptr, 0, b"ptr_field\0".as_ptr() as *const _);
                let len_field = LLVMBuildStructGEP2(self.builder, self.array_type, array_struct_ptr, 1, b"len_field\0".as_ptr() as *const _);

                LLVMBuildStore(self.builder, array_elements_ptr, ptr_field);
                LLVMBuildStore(self.builder, array_len_val, len_field);

                Ok(LLVMBuildLoad2(self.builder, self.array_type, array_struct_ptr, b"loaded_array_struct\0".as_ptr() as *const _))
            },
            ExpressionEnum::IndexExpression(idx_expr) => unsafe {
                let array_val = self.compile_expression(*idx_expr.left)?;
                let mut index_val = self.compile_expression(*idx_expr.index)?;

                // Assuming array_val is a { i64*, i64 } struct
                let array_struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"temp_array_struct_ptr\0".as_ptr() as *const _);
                LLVMBuildStore(self.builder, array_val, array_struct_ptr);

                let elements_ptr_ptr = LLVMBuildStructGEP2(self.builder, self.array_type, array_struct_ptr, 0, b"elements_ptr_ptr\0".as_ptr() as *const _);
                let elements_ptr = LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt64TypeInContext(self.context), 0), elements_ptr_ptr, b"elements_ptr\0".as_ptr() as *const _);

                let element_ptr = LLVMBuildGEP2(self.builder, LLVMInt64TypeInContext(self.context), elements_ptr, &mut index_val, 1, b"indexed_element_ptr\0".as_ptr() as *const _);
                Ok(LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), element_ptr, b"indexed_element\0".as_ptr() as *const _))
            },
            ExpressionEnum::Function(func_lit) => self.compile_function(func_lit),
            _ => Err(format!("Expression not yet implemented: {:?}", expression)),
        }
    }

    fn compile_function(&mut self, func_lit: crate::ast::FunctionLiteral) -> Result<LLVMValueRef, String> {
        unsafe {
            let function_name = CString::new(func_lit.name.value.as_str()).unwrap();
            let num_params = func_lit.parameters.len();

            // Create function signature
            let i64_type = LLVMInt64TypeInContext(self.context);
            let mut param_types = vec![i64_type; num_params];
            let function_type = LLVMFunctionType(i64_type, param_types.as_mut_ptr(), num_params as u32, 0);

            // Add function to module
            let function = LLVMAddFunction(self.module, function_name.as_ptr(), function_type);

            // Create entry block
            let entry = LLVMAppendBasicBlockInContext(self.context, function, b"entry\0".as_ptr() as *const _);

            // Save current function and builder position, and variables
            let old_function = self.function;
            let old_builder_block = LLVMGetInsertBlock(self.builder);
            let old_variables = self.variables.clone();

            // Set new function context
            self.function = Some(function);
            self.variables.clear();
            LLVMPositionBuilderAtEnd(self.builder, entry);

            // Allocate space for parameters and store them
            for (i, param) in func_lit.parameters.iter().enumerate() {
                let param_name = &param.value;
                let llvm_param = LLVMGetParam(function, i as u32);
                let alloca = self.create_entry_block_alloca(param_name, i64_type);
                LLVMBuildStore(self.builder, llvm_param, alloca);
                self.variables.insert(param_name.clone(), alloca);
            }

            // Compile function body
            self.compile_block_statement(func_lit.body)?;

            // If the function does not have a terminator, add a default return
            let last_block = LLVMGetLastBasicBlock(self.function.unwrap());
            if LLVMGetBasicBlockTerminator(last_block).is_null() {
                // Check if the last statement was a return, if not, add a default return 0
                let zero = LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0);
                LLVMBuildRet(self.builder, zero);
            }

            // Restore old function context
            self.function = old_function;
            self.variables = old_variables;
            if !old_builder_block.is_null() {
                LLVMPositionBuilderAtEnd(self.builder, old_builder_block);
            }

            Ok(function)
        }
    }

    fn compile_builtin_call(&mut self, builtin: &builtins::Builtin, args_expr: Vec<ExpressionEnum>) -> Result<LLVMValueRef, String> {
        match builtin.name {
            "print" => {
                if args_expr.len() != 1 {
                    return Err("print() takes exactly one argument".to_string());
                }
                let arg_val = self.compile_expression(args_expr[0].clone())?;
                
                let (format_str_ptr, val_to_print) = unsafe {
                    let arg_type = LLVMTypeOf(arg_val);
                    // Check if the argument is a pointer (likely a string or array struct)
                    if LLVMGetTypeKind(arg_type) == inkwell::llvm_sys::LLVMTypeKind::LLVMStructTypeKind && arg_type == self.array_type {
                        // For string/array struct, print the pointer part (i8*)
                        let struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"temp_struct_ptr\0".as_ptr() as *const _);
                        LLVMBuildStore(self.builder, arg_val, struct_ptr);
                        let ptr_field = LLVMBuildStructGEP2(self.builder, self.array_type, struct_ptr, 0, b"ptr_field\0".as_ptr() as *const _);
                        let loaded_ptr = LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt64TypeInContext(self.context), 0), ptr_field, b"loaded_ptr\0".as_ptr() as *const _);
                        
                        // Cast i64* to i8* for %s format
                        let i8_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                        let casted_ptr = LLVMBuildBitCast(self.builder, loaded_ptr, i8_ptr_type, b"casted_ptr\0".as_ptr() as *const _);
                        
                        (LLVMBuildGlobalStringPtr(self.builder, b"%s\n\0".as_ptr() as *const _, b".str_format\0".as_ptr() as *const _), casted_ptr)
                    } else { // Assume integer for now
                        (LLVMBuildGlobalStringPtr(self.builder, b"%lld\n\0".as_ptr() as *const _, b".int_format\0".as_ptr() as *const _), arg_val)
                    }
                };

                let mut printf_args = [format_str_ptr, val_to_print];
                unsafe {
                    let i8_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let printf_type = LLVMFunctionType(LLVMInt32TypeInContext(self.context), [i8_ptr_type].as_mut_ptr(), 1, 1);
                    LLVMBuildCall2(self.builder, printf_type, self.printf_func, printf_args.as_mut_ptr(), 2, b"printf_call\0".as_ptr() as *const _);
                    Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)) // print returns 0
                }
            }
            "len" => {
                if args_expr.len() != 1 {
                    return Err("len() takes exactly one argument".to_string());
                }
                let arg_val = self.compile_expression(args_expr[0].clone())?;
                
                unsafe {
                    let arg_type = LLVMTypeOf(arg_val);
                    if LLVMGetTypeKind(arg_type) == inkwell::llvm_sys::LLVMTypeKind::LLVMStructTypeKind && arg_type == self.array_type {
                        // Load the struct, then extract the length field
                        let struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"temp_len_struct_ptr\0".as_ptr() as *const _);
                        LLVMBuildStore(self.builder, arg_val, struct_ptr);
                        let len_field_ptr = LLVMBuildStructGEP2(self.builder, self.array_type, struct_ptr, 1, b"len_field_ptr\0".as_ptr() as *const _);
                        Ok(LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), len_field_ptr, b"loaded_len\0".as_ptr() as *const _))
                    } else {
                        Err("len() argument must be a string or an array".to_string())
                    }
                }
            }
            "read_line" => {
                if !args_expr.is_empty() {
                    return Err("read_line() takes no arguments".to_string());
                }
                unsafe {
                    let i8_type = LLVMInt8TypeInContext(self.context);
                    let i32_type = LLVMInt32TypeInContext(self.context);
                    let buffer_size = 2048; // Max input size
                    let buffer_size_val = LLVMConstInt(i32_type, buffer_size as u64, 0);

                    // Allocate buffer on the stack
                    let buffer = LLVMBuildArrayAlloca(self.builder, i8_type, buffer_size_val, b"read_line_buf\0".as_ptr() as *const _);

                    // Load stdin
                    let stdin = LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), self.stdin_ptr, b"stdin\0".as_ptr() as *const _);

                    // Call fgets
                    let mut fgets_args = [buffer, buffer_size_val, stdin];
                    LLVMBuildCall2(self.builder, self.fgets_type, self.fgets_func, fgets_args.as_mut_ptr(), 3, b"fgets_call\0".as_ptr() as *const _);

                    // Call strlen to get the actual length
                    let mut strlen_args = [buffer];
                    let string_len = LLVMBuildCall2(self.builder, self.strlen_type, self.strlen_func, strlen_args.as_mut_ptr(), 1, b"strlen_call\0".as_ptr() as *const _);

                    // Create the Fystan string struct
                    let string_struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"read_line_struct_ptr\0".as_ptr() as *const _);
                    let ptr_field = LLVMBuildStructGEP2(self.builder, self.array_type, string_struct_ptr, 0, b".ptr_field\0".as_ptr() as *const _);
                    let len_field = LLVMBuildStructGEP2(self.builder, self.array_type, string_struct_ptr, 1, b".len_field\0".as_ptr() as *const _);

                    LLVMBuildStore(self.builder, buffer, ptr_field);
                    LLVMBuildStore(self.builder, string_len, len_field);

                    Ok(LLVMBuildLoad2(self.builder, self.array_type, string_struct_ptr, b".loaded_read_line_struct\0".as_ptr() as *const _))
                }
            }
            _ => Err(format!("Unknown builtin function: {}", builtin.name))
        }
    }

    fn compile_infix_expression(&mut self, left_expr: ExpressionEnum, op: InfixOperator, right_expr: ExpressionEnum) -> Result<LLVMValueRef, String> {
        let left = self.compile_expression(left_expr)?;
        let right = self.compile_expression(right_expr)?;

        unsafe {
            match op {
                InfixOperator::Plus => Ok(LLVMBuildAdd(self.builder, left, right, b"addtmp\0".as_ptr() as *const _)),
                InfixOperator::Minus => Ok(LLVMBuildSub(self.builder, left, right, b"subtmp\0".as_ptr() as *const _)),
                InfixOperator::Multiply => Ok(LLVMBuildMul(self.builder, left, right, b"multmp\0".as_ptr() as *const _)),
                InfixOperator::Divide => Ok(LLVMBuildSDiv(self.builder, left, right, b"divtmp\0".as_ptr() as *const _)),
                InfixOperator::Mod => Ok(LLVMBuildSRem(self.builder, left, right, b"remtmp\0".as_ptr() as *const _)),
                InfixOperator::Eq => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntEQ, left, right, b"eqtmp\0".as_ptr() as *const _)),
                InfixOperator::NotEq => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, left, right, b"neqtmp\0".as_ptr() as *const _)),
                InfixOperator::Is => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntEQ, left, right, b"istmp\0".as_ptr() as *const _)),
                InfixOperator::IsNot => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, left, right, b"isnottmp\0".as_ptr() as *const _)),
                InfixOperator::Lt => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntSLT, left, right, b"lttmp\0".as_ptr() as *const _)),
                InfixOperator::Gt => Ok(LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntSGT, left, right, b"gttmp\0".as_ptr() as *const _)),
                InfixOperator::And => {
                    let current_block = LLVMGetInsertBlock(self.builder);
                    let function = LLVMGetBasicBlockParent(current_block);
                    let rhs_bb = LLVMAppendBasicBlockInContext(self.context, function, b"and_rhs\0".as_ptr() as *const _);
                    let merge_bb = LLVMAppendBasicBlockInContext(self.context, function, b"and_merge\0".as_ptr() as *const _);

                    let left_bool = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, left, LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0), b"left_bool\0".as_ptr() as *const _);
                    LLVMBuildCondBr(self.builder, left_bool, rhs_bb, merge_bb);

                    LLVMPositionBuilderAtEnd(self.builder, rhs_bb);
                    let right_bool = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, right, LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0), b"right_bool\0".as_ptr() as *const _);
                    LLVMBuildBr(self.builder, merge_bb);

                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let phi = LLVMBuildPhi(self.builder, LLVMInt1TypeInContext(self.context), b"and_res\0".as_ptr() as *const _);
                    let mut incoming_values = [LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0), right_bool];
                    let mut incoming_blocks = [current_block, rhs_bb];
                    LLVMAddIncoming(phi, incoming_values.as_mut_ptr(), incoming_blocks.as_mut_ptr(), 2);
                    Ok(LLVMBuildZExt(self.builder, phi, LLVMInt64TypeInContext(self.context), b"and_ext\0".as_ptr() as *const _))
                }
                InfixOperator::Or => {
                    let current_block = LLVMGetInsertBlock(self.builder);
                    let function = LLVMGetBasicBlockParent(current_block);
                    let rhs_bb = LLVMAppendBasicBlockInContext(self.context, function, b"or_rhs\0".as_ptr() as *const _);
                    let merge_bb = LLVMAppendBasicBlockInContext(self.context, function, b"or_merge\0".as_ptr() as *const _);

                    let left_bool = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, left, LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0), b"left_bool\0".as_ptr() as *const _);
                    LLVMBuildCondBr(self.builder, left_bool, merge_bb, rhs_bb);

                    LLVMPositionBuilderAtEnd(self.builder, rhs_bb);
                    let right_bool = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, right, LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0), b"right_bool\0".as_ptr() as *const _);
                    LLVMBuildBr(self.builder, merge_bb);

                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let phi = LLVMBuildPhi(self.builder, LLVMInt1TypeInContext(self.context), b"or_res\0".as_ptr() as *const _);
                    let mut incoming_values = [LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0), right_bool];
                    let mut incoming_blocks = [current_block, rhs_bb];
                    LLVMAddIncoming(phi, incoming_values.as_mut_ptr(), incoming_blocks.as_mut_ptr(), 2);
                    Ok(LLVMBuildZExt(self.builder, phi, LLVMInt64TypeInContext(self.context), b"or_ext\0".as_ptr() as *const _))
                }
                _ => Err(format!("Infix operator {:?} not implemented", op)),
            }
        }
    }

    fn compile_if_expression(&mut self, condition: ExpressionEnum, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Result<LLVMValueRef, String> {
        unsafe {
            let cond = self.compile_expression(condition)?;
            let zero = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
            let cond_val = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, cond, zero, b"ifcond\0".as_ptr() as *const _);

            let function = self.function.ok_or("If expression not in a function")?;

            let then_bb = LLVMAppendBasicBlockInContext(self.context, function, b"then\0".as_ptr() as *const _);
            let else_bb = LLVMAppendBasicBlockInContext(self.context, function, b"else\0".as_ptr() as *const _);
            let merge_bb = LLVMAppendBasicBlockInContext(self.context, function, b"ifcont\0".as_ptr() as *const _);

            LLVMBuildCondBr(self.builder, cond_val, then_bb, else_bb);

            // Build then block
            LLVMPositionBuilderAtEnd(self.builder, then_bb);
            let then_val = self.compile_block_statement(consequence)?;
            LLVMBuildBr(self.builder, merge_bb);
            let then_bb = LLVMGetInsertBlock(self.builder);

            // Build else block
            LLVMPositionBuilderAtEnd(self.builder, else_bb);
            let else_val = if let Some(alt) = alternative {
                self.compile_block_statement(alt)?
            } else {
                LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)
            };
            LLVMBuildBr(self.builder, merge_bb);
            let else_bb = LLVMGetInsertBlock(self.builder);

            // Build merge block
            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            let phi = LLVMBuildPhi(self.builder, LLVMInt64TypeInContext(self.context), b"iftmp\0".as_ptr() as *const _);
            let mut values = [then_val, else_val];
            let mut blocks = [then_bb, else_bb];
            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);

            Ok(phi)
        }
    }

    fn compile_while_expression(&mut self, condition: ExpressionEnum, body: BlockStatement) -> Result<LLVMValueRef, String> {
        unsafe {
            let function = self.function.ok_or("While loop not in a function")?;

            let cond_bb = LLVMAppendBasicBlockInContext(self.context, function, b"loop_cond\0".as_ptr() as *const _);
            let body_bb = LLVMAppendBasicBlockInContext(self.context, function, b"loop_body\0".as_ptr() as *const _);
            let after_bb = LLVMAppendBasicBlockInContext(self.context, function, b"after_loop\0".as_ptr() as *const _);

            self.loop_contexts.push(LoopContext { cond_bb, after_bb });

            LLVMBuildBr(self.builder, cond_bb);

            // Condition block
            LLVMPositionBuilderAtEnd(self.builder, cond_bb);
            let cond_val = self.compile_expression(condition)?;
            let zero = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
            let cond_bool = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntNE, cond_val, zero, b"loopcond\0".as_ptr() as *const _);
            LLVMBuildCondBr(self.builder, cond_bool, body_bb, after_bb);

            // Body block
            LLVMPositionBuilderAtEnd(self.builder, body_bb);
            self.compile_block_statement(body)?;
            LLVMBuildBr(self.builder, cond_bb); // Jump back to condition

            // After loop block
            LLVMPositionBuilderAtEnd(self.builder, after_bb);
            
            self.loop_contexts.pop();

            Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0))
        }
    }

    fn compile_for_expression(&mut self, for_expr: ForExpression) -> Result<LLVMValueRef, String> {
        unsafe {
            let function = self.function.ok_or("For loop not in a function")?;

            let iterable_expr = *for_expr.iterable;
            
            // Get the array struct (pointer to elements, length)
            let array_struct_val = self.compile_expression(iterable_expr)?;

            let array_struct_ptr = LLVMBuildAlloca(self.builder, self.array_type, b"temp_for_array_struct_ptr\0".as_ptr() as *const _);
            LLVMBuildStore(self.builder, array_struct_val, array_struct_ptr);

            let elements_ptr_ptr = LLVMBuildStructGEP2(self.builder, self.array_type, array_struct_ptr, 0, b"elements_ptr_ptr\0".as_ptr() as *const _);
            let elements_ptr = LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt64TypeInContext(self.context), 0), elements_ptr_ptr, b"elements_ptr\0".as_ptr() as *const _);

            let len_ptr = LLVMBuildStructGEP2(self.builder, self.array_type, array_struct_ptr, 1, b"len_ptr\0".as_ptr() as *const _);
            let array_len_val = LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), len_ptr, b"array_len_val\0".as_ptr() as *const _);

            // Create index variable
            let i64_type = LLVMInt64TypeInContext(self.context);
            let index_alloca = self.create_entry_block_alloca("__for_index", i64_type);
            let zero = LLVMConstInt(i64_type, 0, 0);
            LLVMBuildStore(self.builder, zero, index_alloca);

            // Create basic blocks
            let cond_bb = LLVMAppendBasicBlockInContext(self.context, function, b"for_cond\0".as_ptr() as *const _);
            let body_bb = LLVMAppendBasicBlockInContext(self.context, function, b"for_body\0".as_ptr() as *const _);
            let inc_bb = LLVMAppendBasicBlockInContext(self.context, function, b"for_inc\0".as_ptr() as *const _);
            let after_bb = LLVMAppendBasicBlockInContext(self.context, function, b"after_for\0".as_ptr() as *const _);

            self.loop_contexts.push(LoopContext { cond_bb: inc_bb, after_bb });

            LLVMBuildBr(self.builder, cond_bb);

            // Condition block
            LLVMPositionBuilderAtEnd(self.builder, cond_bb);
            let index = LLVMBuildLoad2(self.builder, i64_type, index_alloca, b"load_idx\0".as_ptr() as *const _);
            let cond = LLVMBuildICmp(self.builder, inkwell::llvm_sys::LLVMIntPredicate::LLVMIntULT, index, array_len_val, b"forcond\0".as_ptr() as *const _);
            LLVMBuildCondBr(self.builder, cond, body_bb, after_bb);

            // Body block
            LLVMPositionBuilderAtEnd(self.builder, body_bb);
            let element_alloca = self.create_entry_block_alloca(&for_expr.element.value, i64_type);
            let mut index_in_body = LLVMBuildLoad2(self.builder, i64_type, index_alloca, b"load_idx_body\0".as_ptr() as *const _);
            
            let element_ptr = LLVMBuildGEP2(self.builder, i64_type, elements_ptr, &mut index_in_body, 1, b"element_ptr\0".as_ptr() as *const _);
            let element_val = LLVMBuildLoad2(self.builder, i64_type, element_ptr, b"loaded_element\0".as_ptr() as *const _);
            LLVMBuildStore(self.builder, element_val, element_alloca);
            self.variables.insert(for_expr.element.value.clone(), element_alloca);

            self.compile_block_statement(for_expr.body)?;
            self.variables.remove(&for_expr.element.value);

            LLVMBuildBr(self.builder, inc_bb);

            // Increment block
            LLVMPositionBuilderAtEnd(self.builder, inc_bb);
            let index_to_inc = LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), index_alloca, b"load_idx_inc\0".as_ptr() as *const _);
            let one = LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, 0);
            let next_index = LLVMBuildAdd(self.builder, index_to_inc, one, b"next_idx\0".as_ptr() as *const _);
            LLVMBuildStore(self.builder, next_index, index_alloca);
            LLVMBuildBr(self.builder, cond_bb);

            LLVMPositionBuilderAtEnd(self.builder, after_bb);
            self.loop_contexts.pop();

            Ok(LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0))
        }
    }

    fn compile_block_statement(&mut self, block: BlockStatement) -> Result<LLVMValueRef, String> {
        let mut result = ptr::null_mut();
        for stmt in block.statements {
            result = self.compile_statement(stmt)?;
        }
        Ok(result)
    }

    fn create_entry_block_alloca(&mut self, name: &str, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            let function = self.function.unwrap();
            let builder = LLVMCreateBuilderInContext(self.context);
            let entry = LLVMGetEntryBasicBlock(function);
            let first_instruction = LLVMGetFirstInstruction(entry);

            if first_instruction.is_null() {
                LLVMPositionBuilderAtEnd(builder, entry);
            } else {
                LLVMPositionBuilderBefore(builder, first_instruction);
            }

            let c_name = CString::new(name).unwrap();
            let alloca = LLVMBuildAlloca(builder, ty, c_name.as_ptr());
            LLVMDisposeBuilder(builder);
            alloca
        }
    }
    
    fn get_or_create_function(&self, name: &CStr, num_params: usize) -> LLVMValueRef {
        unsafe {
            let mut function = LLVMGetNamedFunction(self.module, name.as_ptr());
            if function.is_null() {
                let mut param_types = vec![LLVMInt64TypeInContext(self.context); num_params];
                let function_type = LLVMFunctionType(LLVMInt64TypeInContext(self.context), param_types.as_mut_ptr(), num_params as u32, 0);
                function = LLVMAddFunction(self.module, name.as_ptr(), function_type);
            }
            function
        }
    }

    pub fn setup_test_main_function(&mut self) {
        unsafe {
            let main_type = LLVMFunctionType(LLVMInt64TypeInContext(self.context), [].as_mut_ptr(), 0, 0);
            let main_func = LLVMAddFunction(self.module, b"main\0".as_ptr() as *const _, main_type);
            let entry = LLVMAppendBasicBlockInContext(self.context, main_func, b"entry\0".as_ptr() as *const _);
            LLVMPositionBuilderAtEnd(self.builder, entry);
            self.function = Some(main_func);
        }
    }

    fn write_to_file(&self, target_triple: &str, filename: &str) -> Result<(), String> {
        unsafe {
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let target_triple_c = CString::new(target_triple).unwrap();
            let mut target = ptr::null_mut();
            let mut error = ptr::null_mut();

            if LLVMGetTargetFromTriple(target_triple_c.as_ptr(), &mut target, &mut error) != 0 {
                let err_msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                LLVMDisposeMessage(error);
                return Err(format!("Failed to get target: {}", err_msg));
            }

            let cpu = CString::new("generic").unwrap();
            let features = CString::new("").unwrap();
            let target_machine = LLVMCreateTargetMachine(
                target,
                target_triple_c.as_ptr(),
                cpu.as_ptr(),
                features.as_ptr(),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );
            if target_machine.is_null() {
                return Err("Failed to create target machine".to_string());
            }

            let filename_c = CString::new(filename).unwrap();
            let mut error = ptr::null_mut();
            if LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                filename_c.as_ptr() as *mut c_char,
                inkwell::llvm_sys::target_machine::LLVMCodeGenFileType::LLVMObjectFile,
                &mut error,
            ) != 0
            {
                let err_msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                LLVMDisposeMessage(error);
                return Err(format!("Failed to emit object file: {}", err_msg));
            }

            Ok(())
        }
    }
}
