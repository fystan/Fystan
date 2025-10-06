use crate::codegen::generator::Generator;
use crate::target::Target;

pub struct Compiler;

impl Compiler {
    pub fn compile(source: &str, target: &Target) -> Result<String, String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }

        let mut generator = Generator::new(target.clone());
        generator.generate(program)
    }
}