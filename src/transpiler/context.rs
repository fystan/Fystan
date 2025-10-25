use crate::ast::Program;
use crate::target::Target;

#[derive(Debug, Clone)]
pub enum CompilationMode {
    AOT,
    JIT,
}

#[derive(Debug)]
pub enum CompilationResult {
    AOT(Vec<u8>),
    JIT(i64),
}

#[derive(Debug)]
pub struct Compiler {
    target: Target,
    mode: CompilationMode,
}

impl Compiler {
    pub fn new(target: &Target, mode: CompilationMode) -> Self {
        Self {
            target: target.clone(),
            mode,
        }
    }

    pub fn compile(&self, _program: Program) -> Result<CompilationResult, String> {
        Err("Compiler not implemented yet".to_string())
    }
}