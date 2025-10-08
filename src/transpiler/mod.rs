pub mod rust_generator;
pub mod scope;

use crate::target::Target;
use self::rust_generator::Generator;

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(source: &str, _target: &Target) -> Result<String, String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }

        let mut generator = Generator::new();
        generator.generate(program)
    }
}