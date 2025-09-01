// src/builtins/mod.rs
use cranelift::prelude::{types, AbiParam, Signature};
use cranelift::codegen::isa::CallConv;
use std::collections::HashMap;


/// Represents the type of a Fystan value.
/// We'll extend this as we add more types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FystanType {
    Integer,
    Boolean,
    String,
    Array(Box<FystanType>), // e.g., Array of Integers
    // Add other types like Float, Null, etc. later
}

impl FystanType {
    /// Maps a FystanType to a Cranelift IR type.
    pub fn to_cranelift_type(&self, ptr_type: types::Type) -> types::Type {
        match self {
            FystanType::Integer => types::I64,
            FystanType::Boolean => types::I64, // Booleans are represented as i64 (0 or 1)
            FystanType::String => ptr_type, // Strings are passed as pointers
            FystanType::Array(_) => ptr_type, // Arrays are passed as pointers
        }
    }
}

/// Defines the signature of a function, including parameter types and return type.
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<FystanType>,
    pub returns: FystanType,
}

/// Represents a built-in function available in the Fystan language.
pub struct BuiltinFunction {
    pub name: &'static str,
    pub signature: FunctionSignature,
    // The Cranelift signature, derived from the Fystan signature.
    pub cranelift_signature: Signature,
}

impl BuiltinFunction {
    /// Creates a new built-in function definition.
    fn new(name: &'static str, params: Vec<FystanType>, returns: FystanType, ptr_type: types::Type) -> Self {
        let mut cranelift_signature = Signature::new(CallConv::SystemV); // Or the appropriate call convention

        // Convert Fystan return type to Cranelift AbiParam
        cranelift_signature.returns.push(AbiParam::new(returns.to_cranelift_type(ptr_type)));

        // Convert Fystan param types to Cranelift AbiParams
        for param_type in &params {
            cranelift_signature.params.push(AbiParam::new(param_type.to_cranelift_type(ptr_type)));
        }
        
        // For string, we need a second parameter for the length
        if params.contains(&FystanType::String) {
            cranelift_signature.params.push(AbiParam::new(types::I64));
        }


        Self {
            name,
            signature: FunctionSignature { params, returns },
            cranelift_signature,
        }
    }
}

/// A registry for all built-in functions.
pub struct BuiltinRegistry {
    pub functions: HashMap<String, BuiltinFunction>,
}

impl BuiltinRegistry {
    /// Creates a new registry and populates it with all the built-in functions.
    pub fn new(ptr_type: types::Type) -> Self {
        let mut functions = HashMap::new();

        // 'print' function
        // Takes a string and returns an integer (let's say 0 for success).
        let print_func = BuiltinFunction::new(
            "print",
            vec![FystanType::String],
            FystanType::Integer, // Returns 0 on success
            ptr_type,
        );
        functions.insert("print".to_string(), print_func);

        // Add other built-in functions here...
        // For example, a 'len' function for strings or arrays.
        /*
        let len_func = BuiltinFunction::new(
            "len",
            vec![FystanType::Array(Box::new(FystanType::Integer))], // Example: len for array of int
            FystanType::Integer,
            ptr_type
        );
        functions.insert("len".to_string(), len_func);
        */

        Self { functions }
    }

    /// Looks up a built-in function by name.
    pub fn lookup(&self, name: &str) -> Option<&BuiltinFunction> {
        self.functions.get(name)
    }
}