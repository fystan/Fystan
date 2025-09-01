use fystan::codegen;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The subcommand to run
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Build the source file to an object file
    Build {
        /// The source file to compile
        source_path: String,

        /// The target architecture to compile for (e.g., "x86_64-pc-windows-msvc")
        #[arg(short, long)]
        target: String, // Target is now required for 'build'
    },
}

fn main() {
    let args = Args::parse();

    let (source_path, target_str) = match args.command {
        Commands::Build { source_path, target } => (source_path, target),
    };

    let src = match fs::read_to_string(&source_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read source file '{}': {}", source_path, e);
            std::process::exit(1);
        }
    };

    let compiler = codegen::Compiler::new();

    match compiler.compile_to_object_file(&src, &target_str) {
        Ok(result_bytes) => {
            let output_filename = format!("{}.obj", Path::new(&source_path).file_stem().unwrap().to_str().unwrap());
            match fs::write(&output_filename, result_bytes) {
                Ok(_) => println!("Successfully compiled to {}", output_filename),
                Err(e) => eprintln!("Failed to write output file: {}", e),
            }
        }
        Err(e) => {
            eprintln!("Compilation failed: {}", e);
        }
    }
}


#[no_mangle]
pub extern "C" fn print(ptr: *const u8, len: usize) {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    let s = std::str::from_utf8(slice).expect("Invalid UTF-8 string");
    println!("{}", s);
}

#[cfg(test)]
mod tests {
    use super::*;
    use fystan::lexer::token::{Token, TokenType};

    fn assert_compiles_ok(fystan_code: &str) {
        let compiler = codegen::Compiler::new();
        // The target doesn't matter as much since we are not linking/running,
        // but it's still needed for codegen.
        let target = "windows:amd64"; 
        match compiler.compile_to_object_file(fystan_code, target) {
            Ok(_) => {
                // Compilation successful, do nothing.
            },
            Err(e) => {
                panic!("Compilation failed when it should have succeeded. Error: {}", e);
            }
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
        assert_compiles_ok("if (1 > 2) { 10; } else { 20; }");
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
    fn test_assignment() {
        let code = "
            let a = 5;
            a = 10;
            return a;
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_float_comparison() {
        let code = "
            if (2.5 > 2.0) {
                return 1;
            } else {
                return 0;
            }
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_string_literal_declaration() {
        let code = "
            let s = \"hello\";
            return 1;
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_array_literal_and_index() {
        let code = "
            let a = [10, 20, 30];
            return a[1];
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_array_index_with_expression() {
        let code = "
            let a = [10, 20, 30];
            return a[1 + 1];
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_array_assignment_and_return() {
        let code = "
            let x = [1, 2, 3];
            let y = x[0] + x[2];
            return y;
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_print_builtin() {
        let code = "print(\"Hello\");";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_logical_operators() {
        assert_compiles_ok("return true && false;");
        assert_compiles_ok("return true || false;");
        assert_compiles_ok("let a = true; let b = false; return a && b;");
    }

    #[test]
    fn test_compound_assignment() {
        assert_compiles_ok("let a = 5; a += 5; return a;");
        assert_compiles_ok("let a = 5; a -= 5; return a;");
        assert_compiles_ok("let a = 5; a *= 5; return a;");
        assert_compiles_ok("let a = 5; a /= 5; return a;");
    }

    #[test]
    fn test_ctmm_array_compilation() {
        assert_compiles_ok("let my_array = [1, 2, 3, 4, 5];");
    }

    #[test]
    fn test_hash_literal_compilation() {
        let code = "let my_hash = { \"a\": 1, \"b\": 2 };";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_simple_hash() {
        let input = "{ \"a\": 1 }";
        let l = fystan::lexer::Lexer::new(input);
        let mut p = fystan::parser::Parser::new(l);
        let program = p.parse_program();
        let errors = p.errors();
        assert_eq!(errors.len(), 0, "Parser errors: {:?}", errors);
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_lexer_tokens() {
        let input = r###"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
! - / * 5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
"###;
        let mut l = fystan::lexer::Lexer::new(input);
        let expected_tokens = vec![
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Function, "fn".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::LBrace, "{".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::RBrace, "}".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "result".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Bang, "!".to_string()),
            Token::new(TokenType::Minus, "-".to_string()),
            Token::new(TokenType::Slash, "/".to_string()),
            Token::new(TokenType::Asterisk, "*".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Lt, "<".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Gt, ">".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::If, "if".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Lt, "<".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::LBrace, "{".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::True, "true".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::RBrace, "}".to_string()),
            Token::new(TokenType::Else, "else".to_string()),
            Token::new(TokenType::LBrace, "{".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::False, "false".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::RBrace, "}".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Eq, "==".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::NotEq, "!=".to_string()),
            Token::new(TokenType::Int, "9".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::String, "foobar".to_string()),
            Token::new(TokenType::String, "foo bar".to_string()),
            Token::new(TokenType::LBrack, "[".to_string()),
            Token::new(TokenType::Int, "1".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Int, "2".to_string()),
            Token::new(TokenType::RBrack, "]".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::LBrace, "{".to_string()),
            Token::new(TokenType::String, "foo".to_string()),
            Token::new(TokenType::Colon, ":".to_string()),
            Token::new(TokenType::String, "bar".to_string()),
            Token::new(TokenType::RBrace, "}".to_string()),
            Token::new(TokenType::Eof, "".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = l.next_token();
            assert_eq!(token, expected_token);
        }
    }
}