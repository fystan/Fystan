pub mod rust_generator;
pub mod scope;

use crate::target::Target;
use self::rust_generator::Generator;
use crate::ctmm::{CTMMAnalyzer, CTMMAnalysis};

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(source: &str, target: &Target) -> Result<String, String> {
        let l = crate::lexer::Lexer::new(source);
        let mut p = crate::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        if !errors.is_empty() {
            return Err(format!("Parser errors: {:?}", errors));
        }

        let mut ctmm_analyzer = CTMMAnalyzer::new();
        let ctmm_analysis = match ctmm_analyzer.analyze(&program) {
            Ok(analysis) => analysis,
            Err(_) => return Err("CTMM analysis failed.".to_string()),
        };

        let mut generator = Generator::new(target.clone(), ctmm_analysis);
        generator.generate(program)
    }
}