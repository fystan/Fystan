use clap::{Parser, Subcommand};
use fystan::codegen::Compiler;
use std::collections::HashMap;
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

    /// The target to compile for (e.g., windows:amd64)
    #[arg(long)]
    target: Option<String>,
}

fn get_supported_targets() -> HashMap<String, String> {
    let mut targets = HashMap::new();
    targets.insert("windows:amd64".to_string(), "x86_64-pc-windows-msvc".to_string());
    targets.insert("windows:arm64".to_string(), "aarch64-pc-windows-msvc".to_string());
    targets.insert("linux:amd64".to_string(), "x86_64-unknown-linux-gnu".to_string());
    targets.insert("linux:arm64".to_string(), "aarch64-unknown-linux-gnu".to_string());
    targets.insert("android:arm64".to_string(), "aarch64-linux-android".to_string());
    targets.insert("wasm:wasm32".to_string(), "wasm32-unknown-unknown".to_string());
    targets.insert("wasm:wasm64".to_string(), "wasm64-unknown-unknown".to_string());
    targets
}

fn main() {
    let cli = Cli::parse();
    let supported_targets = get_supported_targets();

    match cli.command {
        Commands::Build(args) => {
            let source_code = match fs::read_to_string(&args.source_path) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Error reading source file '{}': {}", args.source_path, e);
                    std::process::exit(1);
                }
            };

            let rust_code = match Compiler::compile(&source_code) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            let target_triple = match &args.target {
                Some(target_str) => {
                    if !supported_targets.contains_key(target_str) {
                        eprintln!("Error: Unsupported target '{}'.", target_str);
                        eprintln!("Supported targets are: {}", supported_targets.keys().map(|s| s.as_str()).collect::<Vec<_>>().join(", "));
                        std::process::exit(1);
                    }
                    supported_targets.get(target_str).cloned()
                }
                None => None,
            };

            let is_windows_target = target_triple.as_deref().map_or(cfg!(windows), |t| t.contains("windows"));

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

            if let Some(triple) = &target_triple {
                command.arg("--target").arg(triple);
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