use clap::{Parser, Subcommand};
use fystan::transpiler::{Transpiler, aot::AOTCompiler, jit::JITCompiler};
use fystan::target::{Target, TargetArch, TargetOS};
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

    /// The target to compile for (e.g., linux:x86_64)
    #[arg(long)]
    target: Option<String>,

    /// The path to the output file
    #[arg(long, short = 'o')]
    output: Option<String>,

    /// The compilation mode (aot or jit)
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

            let (bytecode, _string_allocator) = match Transpiler::transpile(&source_code, &target) {
                Ok((code, sa)) => (code, sa),
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            if args.mode == "aot" {
                let output_path = match &args.output {
                    Some(path) => path.clone(),
                    None => {
                        let source_path = Path::new(&args.source_path);
                        let output_filename = source_path
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("output");

                        let is_windows_target = target.os == TargetOS::Windows;

                        if is_windows_target {
                            format!("{}.obj", output_filename)
                        } else {
                            format!("{}.o", output_filename)
                        }
                    }
                };

                let mut aot = AOTCompiler::new(&target);
                aot.compile_and_save_executable(&bytecode, &output_path, &target).expect("AOT compilation failed");
            } else if args.mode == "jit" {
                let mut jit = JITCompiler::new();
                match jit.compile_and_run(&bytecode) {
                    Ok(result) => println!("JIT execution result: {}", result),
                    Err(e) => eprintln!("JIT compilation failed: {}", e),
                }
            }
        }
    }
}