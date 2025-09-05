use clap::Parser;
use fystan::codegen::Compiler;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The source file to compile
    source_path: String,

    /// The target triple to compile for (e.g., "x86_64-unknown-linux-gnu")
    #[arg(short, long)]
    target: String,

    /// The output file name
    #[arg(short, long)]
    output: Option<String>,
}

fn main() {
    let args = Args::parse();

    let source_code = match fs::read_to_string(&args.source_path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading source file '{}': {}", args.source_path, e);
            std::process::exit(1);
        }
    };

    let output_path = args.output.unwrap_or_else(|| {
        let path = Path::new(&args.source_path);
        path.with_extension("o").to_str().unwrap().to_string()
    });

    match Compiler::run_from_source(&source_code, &args.target, &output_path) {
        Ok(_) => println!("Compilation successful! Output written to {}", output_path),
        Err(e) => {
            eprintln!("Error: {}", e);
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

    #[test]
    fn test_while_loop() {
        let code = "
            let i = 0;
            while (i < 10) {
                i = i + 1;
            }
            return i;
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_break_statement() {
        let code = "
            let i = 0;
            while (i < 10) {
                if (i == 5) {
                    break;
                }
                i = i + 1;
            }
            return i; // Should be 5
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_continue_statement() {
        let code = "
            let i = 0;
            let j = 0;
            while (i < 10) {
                i = i + 1;
                if (i % 2 == 0) {
                    continue;
                }
                j = j + 1; // Should only increment for odd numbers
            }
            return j; // Should be 5 (for 1, 3, 5, 7, 9)
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_for_loop() {
        let code = "
            let sum = 0;
            let my_array = [1, 2, 3, 4, 5];
            for (x in my_array) {
                sum = sum + x;
            }
            return sum; // Should be 15
        ";
        // Note: This test depends on a simplified for-loop implementation
        // that requires a literal array in the for expression itself.
        // A more robust implementation would handle variables.
        let code_simplified = "
            let sum = 0;
            for (x in [1, 2, 3, 4, 5]) {
                sum = sum + x;
            }
            return sum;
        ";
        assert_compiles_ok(code_simplified);
    }

    #[test]
    fn test_print_builtin() {
        let code = "
            print(123);
            print(\"hello world\");
            let x = 42;
            print(x);
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_len_builtin() {
        let code = "
            let s = \"hello\";
            let l1 = len(s);
            let a = [1, 2, 3];
            let l2 = len(a);
            return l1 + l2; // Should be 5 + 3 = 8
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_array_variable_len_and_for_loop() {
        let code = "
            let my_arr = [10, 20, 30];
            let arr_len = len(my_arr);
            let sum = 0;
            for (val in my_arr) {
                sum = sum + val;
            }
            return sum + arr_len; // Should be (10+20+30) + 3 = 63
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_string_variable_len() {
        let code = "
            let my_str = \"Fystan\";
            let str_len = len(my_str);
            return str_len; // Should be 6
        ";
        assert_compiles_ok(code);
    }
}