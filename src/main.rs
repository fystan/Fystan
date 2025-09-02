use fystan::codegen::Compiler;
use clap::Parser;
use std::fs;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The source file to compile
    source_path: String,

    /// The target triple to compile for (e.g., "x86_64-pc-windows-msvc")
    #[arg(short, long)]
    target: String,

    /// The output file name
    #[arg(short, long, default_value = "output")]
    output: String,
}

fn main() {
    let args = Args::parse();

    let src = match fs::read_to_string(&args.source_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read source file '{}': {}", args.source_path, e);
            std::process::exit(1);
        }
    };

    // Determine the output executable name based on the OS
    let mut output_filename = args.output.clone();
    if cfg!(windows) && !output_filename.ends_with(".exe") {
        output_filename.push_str(".exe");
    }

    match Compiler::run_from_source(&src, &args.target, &output_filename) {
        Ok(_) => {
            println!("Successfully compiled '{}' to '{}'", args.source_path, output_filename);
        }
        Err(e) => {
            eprintln!("Compilation failed: {}", e);
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use fystan::codegen::Compiler;
    use fystan::parser::Parser;
    use llvm_sys as llvm;

    fn assert_compiles_ok(fystan_code: &str) {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let mut compiler = Compiler::new(context);
            let l = fystan::lexer::Lexer::new(fystan_code);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(p.errors().is_empty(), "Parser errors found: {:?}", p.errors());

            compiler.setup_test_main_function();

            let result = compiler.compile(program);
            assert!(result.is_ok(), "Compilation failed: {:?}", result.err());
            
            llvm::core::LLVMContextDispose(context);
        }
    }

    #[test]
    fn test_return_statement() {
        assert_compiles_ok("return 5;");
    }

    #[test]
    fn test_simple_arithmetic() {
        assert_compiles_ok("return 2 * (3 + 4);");
    }

    #[test]
    fn test_let_statements() {
        assert_compiles_ok("let a = 5; let b = 10; return a + b;");
    }

    #[test]
    fn test_boolean_and_prefix_operators() {
        assert_compiles_ok("return !true;");
        assert_compiles_ok("return !false;");
        assert_compiles_ok("return -10;");
    }

    #[test]
    fn test_if_else_expressions() {
        assert_compiles_ok("if (1 < 2) { return 10; } else { return 20; }");
    }

    #[test]
    fn test_function_declaration_and_call() {
        let code = "
            fn add(a, b) {
                return a + b;
            }
            return add(5, 10);
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_string_literal_statement() {
        assert_compiles_ok("let a = \"hello world\";");
    }
}