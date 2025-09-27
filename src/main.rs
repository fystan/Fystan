use clap::{Parser, Subcommand};
use fystan::codegen::Compiler;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::Builder;

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
    /// The target triple to compile for (e.g., "windows:amd64")
    #[arg(short, long)]
    target: String,

    /// The source file to compile
    source_path: String,

    /// The output file name
    #[arg(short, long)]
    output: Option<String>,
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

            // 1. Determine output path for the final executable
            let output_path = args.output.map(PathBuf::from).unwrap_or_else(|| {
                let path = Path::new(&args.source_path);
                path.with_extension("")
            });

            // 2. Create a temporary directory for the .o file
            let tmp_dir = Builder::new().prefix("fystan").tempdir().unwrap();
            let obj_path = tmp_dir.path().join("output.o");

            // 3. Get the target triple
            let target_triple = match map_target_to_triple(&args.target) {
                Some(triple) => triple,
                None => {
                    eprintln!("Error: Unsupported target '{}'", args.target);
                    std::process::exit(1);
                }
            };

            // 4. Compile to object file in the temporary directory
            if let Err(e) = Compiler::run_from_source(&source_code, target_triple, obj_path.to_str().unwrap()) {
                eprintln!("Compilation Error: {}", e);
                std::process::exit(1);
            }

            // 5. Link the object file into an executable
            let linker_output = Command::new("clang")
                .arg(obj_path)
                .arg("-o")
                .arg(&output_path)
                .output();

            match linker_output {
                Ok(output) => {
                    if !output.status.success() {
                        eprintln!(
                            "Linker error:\n{}",
                            String::from_utf8_lossy(&output.stderr)
                        );
                        std::process::exit(1);
                    }
                    println!("Build successful! Executable written to {}", output_path.to_str().unwrap());
                }
                Err(e) => {
                    eprintln!("Failed to run linker: {}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

fn map_target_to_triple(target: &str) -> Option<&'static str> {
    match target {
        "windows:amd64" => Some("x86_64-pc-windows-msvc"),
        "linux:amd64" => Some("x86_64-unknown-linux-gnu"),
        "darwin:amd64" => Some("x86_64-apple-darwin"),
        "android:arm64" => Some("aarch64-linux-android"),
        "windows:x86" => Some("i686-pc-windows-msvc"),
        "linux:x86" => Some("i686-unknown-linux-gnu"),
        "linux:arm" => Some("armv7-unknown-linux-gnueabihf"),
        "linux:arm64" => Some("aarch64-unknown-linux-gnu"),
        "darwin:arm64" => Some("aarch64-apple-darwin"),
        "wasm:wasm32" => Some("wasm32-unknown-unknown"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use fystan::codegen::Compiler;
    use fystan::parser::Parser;
    use inkwell::llvm_sys as llvm;

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