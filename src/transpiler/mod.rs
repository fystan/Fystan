pub mod bytecode_generator;
pub mod vm;
pub mod scope;
pub mod string_allocator;

use crate::target::Target;
use self::bytecode_generator::BytecodeGenerator;
use self::string_allocator::StringAllocator;

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(source: &str, _target: &Target) -> Result<(Vec<u8>, StringAllocator), String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }

        let mut generator = BytecodeGenerator::new();
        let bytecode = generator.generate(program)?;
        Ok((bytecode, generator.string_allocator))
    }
}