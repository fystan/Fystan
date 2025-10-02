use clap::{Parser, Subcommand};
use fystan::codegen::Compiler;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Builds a fystan file
    Build(BuildArgs),
}

#[derive(Parser, Debug)]
#[command(disable_help_flag = true)]
struct BuildArgs {
    /// The source file to compile
    source_path: String,

    /// The output executable name
    #[arg(short, long)]
    output: String,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build(args) => {
            let source_code = match fs::read_to_string(&args.source_path) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Error reading source file '{}': {}", args.source_path, e);
                    std::process::exit(1);
                }
            };

            // Ensure the output has executable extension if on Windows
            let output_path = if cfg!(windows) && !args.output.ends_with(".exe") {
                format!("{}.exe", args.output)
            } else {
                args.output
            };

            if let Err(e) = Compiler::run_from_source(&source_code, &output_path) {
                eprintln!("Compilation Error: {}", e);
                std::process::exit(1);
            }

            println!(
                "Build successful! Executable written to {}",
                output_path
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use fystan::codegen::Compiler;

    fn assert_compiles_ok(fystan_code: &str) {
        let mut compiler = Compiler::new();
        let l = fystan::lexer::Lexer::new(fystan_code);
        let mut p = fystan::parser::Parser::new(l);
        let program = p.parse_program();
        assert!(p.errors().is_empty(), "Parser errors found: {:?}", p.errors());

        let result = compiler.compile(program);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());
    }

    #[test]
    fn test_return_statement() {
        assert_compiles_ok("return 5");
    }

    #[test]
    fn test_simple_arithmetic() {
        assert_compiles_ok("return 2 * (3 + 4)");
    }

    #[test]
    fn test_assignment_statements() {
        assert_compiles_ok("a = 5\nb = 10\nreturn a + b");
    }

    #[test]
    fn test_boolean_and_prefix_operators() {
        assert_compiles_ok("return not True");
        assert_compiles_ok("return not False");
        assert_compiles_ok("return -10");
    }

    #[test]
    fn test_if_else_expressions() {
        assert_compiles_ok("if 1 < 2:\n    return 10\nelse:\n    return 20");
    }

    #[test]
    fn test_function_declaration_and_call() {
        let code = "
def add(a, b):
    return a + b
return add(5, 10)
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_string_literal_statement() {
        assert_compiles_ok("a = \"hello world\"");
    }

    #[test]
    fn test_while_loop() {
        let code = "
i = 0
while i < 10:
    i = i + 1
return i
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_break_statement() {
        let code = "
i = 0
while i < 10:
    if i == 5:
        break
    i = i + 1
return i
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_continue_statement() {
        let code = "
i = 0
j = 0
while i < 10:
    i = i + 1
    if i % 2 == 0:
        continue
    j = j + 1
return j
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_for_loop() {
        let code = "
sum = 0
my_array = [1, 2, 3, 4, 5]
for x in my_array:
    sum = sum + x
return sum
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_python_none_literal() {
        assert_compiles_ok("return None");
    }

    #[test]
    fn test_python_pass_statement() {
        let code = "
def empty_function():
    pass
return 42
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_python_none_comparison() {
        let code = "
x = None
if x is None:
    return True
else:
    return False
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_print_builtin() {
        let code = "
print(123)
print(\"hello world\")
x = 42
print(x)
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_len_builtin() {
        let code = "
s = \"hello\"
l1 = len(s)
a = [1, 2, 3]
l2 = len(a)
return l1 + l2
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_array_variable_len_and_for_loop() {
        let code = "
my_arr = [10, 20, 30]
arr_len = len(my_arr)
sum = 0
for val in my_arr:
    sum = sum + val
return sum + arr_len
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_string_variable_len() {
        let code = "
my_str = \"Fystan\"
str_len = len(my_str)
return str_len
        ";
        assert_compiles_ok(code);
    }

    #[test]
    fn test_read_line_builtin() {
        let code = "
line = read_line()
print(line)
        ";
        assert_compiles_ok(code);
    }
}