use clap::{Parser, Subcommand};
use fystan::transpiler::{Transpiler, vm::VM};
use fystan::interpreter::Interpreter;
use fystan::target::{Target, TargetArch, TargetOS};
use std::fs;
use std::path::Path;
use rand::Rng;

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

    /// The target to compile for (e.g., linux:x86_64)
    #[arg(long)]
    target: Option<String>,

    /// The path to the output file
    #[arg(long, short = 'o')]
    output: Option<String>,

    /// The compilation mode (aot, jit, or interpret)
    #[arg(long, short = 'm', default_value = "aot")]
    mode: String,
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

            let target = match &args.target {
                Some(target_str) => match Target::from_string(target_str) {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                },
                None => {
                    let os = if cfg!(target_os = "windows") {
                        TargetOS::Windows
                    } else if cfg!(target_os = "linux") {
                        TargetOS::Linux
                    } else {
                        eprintln!("Error: Unsupported host OS. Please specify a --target.");
                        std::process::exit(1);
                    };
                    let arch = if cfg!(target_arch = "x86_64") {
                        TargetArch::X86_64
                    } else if cfg!(target_arch = "aarch64") {
                        TargetArch::AArch64
                    } else {
                        eprintln!("Error: Unsupported host architecture. Please specify a --target.");
                        std::process::exit(1);
                    };
                    Target::new(os, arch).unwrap()
                }
            };

            if args.mode == "interpret" {
                match Interpreter::interpret(&source_code, &target) {
                    Ok(_) => {
                        println!("Interpretation successful!");
                        return;
                    }
                    Err(e) => {
                        eprintln!("Interpretation Error: {}", e);
                        std::process::exit(1);
                    }
                }
            }

            let (bytecode, string_allocator) = match Transpiler::transpile(&source_code, &target) {
                Ok((code, sa)) => (code, sa),
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            let _output_path = if args.mode == "jit" {
                let rand_string: String = rand::thread_rng()
                    .sample_iter(rand::distributions::Alphanumeric)
                    .take(10)
                    .map(char::from)
                    .collect();
                let temp_filename = format!("fystan_jit_{}", rand_string);
                let is_windows_target = target.os == TargetOS::Windows;
                if is_windows_target {
                    format!("{}.exe", temp_filename)
                } else {
                    temp_filename
                }
            } else {
                match args.output {
                    Some(path) => path,
                    None => {
                        let source_path = Path::new(&args.source_path);
                        let output_filename = source_path
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("output");

                        let is_windows_target = target.os == TargetOS::Windows;

                        if is_windows_target {
                            format!("{}.exe", output_filename)
                        } else {
                            output_filename.to_string()
                        }
                    }
                }
            };

            // For AOT and JIT, use VM to execute bytecode
            let strings = string_allocator.get_strings();
            let mut vm = VM::new(bytecode, strings);
            if let Err(e) = vm.run() {
                eprintln!("Execution Error: {}", e);
                std::process::exit(1);
            }

            if args.mode == "aot" {
                // For AOT, we could save bytecode to file, but for now just execute
                println!("AOT mode: Bytecode executed successfully!");
            } else if args.mode == "jit" {
                println!("JIT mode: Bytecode executed successfully!");
            }
        }
    }
}



