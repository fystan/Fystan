pub mod evaluator;

use crate::target::Target;
use self::evaluator::Evaluator;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(source: &str, _target: &Target) -> Result<(), String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }

        let mut evaluator = Evaluator::new();
        evaluator.evaluate(program)
    }
}