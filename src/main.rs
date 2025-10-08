use clap::{Parser, Subcommand};
use fystan::transpiler::Transpiler;
use fystan::target::{Target, TargetArch, TargetOS};
use std::fs;
use std::io::Write;
use std::process::Stdio;
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
            if args.mode == "jit" {
                println!("JIT mode is not yet implemented.");
                std::process::exit(0);
            }

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

            let rust_code = match Transpiler::transpile(&source_code, &target) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            let output_path = match args.output {
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
            };

            let mut command = std::process::Command::new("rustc");
            command
                .arg("-")
                .arg("-o")
                .arg(&output_path)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());

            if args.target.is_some() {
                command.arg("--target").arg(target.to_rust_triple());
            }

            let mut child = match command.spawn() {
                Ok(child) => child,
                Err(e) => {
                    eprintln!("Failed to spawn rustc: {}", e);
                    std::process::exit(1);
                }
            };

            if let Some(mut stdin) = child.stdin.take() {
                if let Err(e) = stdin.write_all(rust_code.as_bytes()) {
                    eprintln!("Failed to write to rustc stdin: {}", e);
                    std::process::exit(1);
                }
            } else {
                eprintln!("Failed to open stdin for rustc");
                std::process::exit(1);
            }

            let output = match child.wait_with_output() {
                Ok(out) => out,
                Err(e) => {
                    eprintln!("Failed to wait for rustc: {}", e);
                    std::process::exit(1);
                }
            };

            if !output.status.success() {
                eprintln!(
                    "Rust compilation failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                std::process::exit(1);
            }

            println!("Build successful! Executable written to {}", output_path);
        }
    }
}