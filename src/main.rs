use clap::{Parser, Subcommand};
use fystan::codegen::Compiler;
use fystan::target::{Target, TargetArch, TargetOS};
use std::fs;

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

    /// The target to compile for (e.g., linux:x86_64)
    #[arg(long)]
    target: Option<String>,
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

            let rust_code = match Compiler::compile(&source_code, &target) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            let is_windows_target = target.os == TargetOS::Windows;

            let output_path = if is_windows_target && !args.output.ends_with(".exe") {
                format!("{}.exe", args.output)
            } else {
                args.output.clone()
            };

            let temp_rs_name = args.output.trim_end_matches(".exe");
            let temp_rs_file = format!("{}.rs", temp_rs_name);
            if let Err(e) = fs::write(&temp_rs_file, rust_code) {
                eprintln!("Failed to write temporary file: {}", e);
                std::process::exit(1);
            }

            let mut command = std::process::Command::new("rustc");
            command.arg(&temp_rs_file).arg("-o").arg(&output_path);

            if args.target.is_some() {
                command.arg("--target").arg(target.to_rust_triple());
            }

            let output = match command.output() {
                Ok(out) => out,
                Err(e) => {
                    eprintln!("Failed to run rustc: {}", e);
                    fs::remove_file(&temp_rs_file).ok();
                    std::process::exit(1);
                }
            };

            if !output.status.success() {
                eprintln!(
                    "Rust compilation failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                fs::remove_file(&temp_rs_file).ok();
                std::process::exit(1);
            }

            if let Err(e) = fs::remove_file(&temp_rs_file) {
                eprintln!("Failed to clean up temporary file: {}", e);
            }

            println!("Build successful! Executable written to {}", output_path);
        }
    }
}