use clap::{Parser, Subcommand};
use fystan::codegen::Compiler;
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

            let rust_code = match Compiler::compile(&source_code) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Compilation Error: {}", e);
                    std::process::exit(1);
                }
            };

            let output_path = if cfg!(windows) && !args.output.ends_with(".exe") {
                format!("{}.exe", args.output)
            } else {
                args.output.clone()
            };

            // Write the generated Rust code to a temporary file
            let temp_rs_name = args.output.trim_end_matches(".exe");
            let temp_rs_file = format!("{}.rs", temp_rs_name);
            if let Err(e) = fs::write(&temp_rs_file, rust_code) {
                eprintln!("Failed to write temporary file: {}", e);
                std::process::exit(1);
            }

            // Compile the Rust code to an executable using rustc
            let output = match std::process::Command::new("rustc")
                .arg(&temp_rs_file)
                .arg("-o")
                .arg(&output_path)
                .output()
            {
                Ok(out) => out,
                Err(e) => {
                    eprintln!("Failed to run rustc: {}", e);
                    fs::remove_file(&temp_rs_file).ok(); // Attempt to clean up
                    std::process::exit(1);
                }
            };

            if !output.status.success() {
                eprintln!(
                    "Rust compilation failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                fs::remove_file(&temp_rs_file).ok(); // Attempt to clean up
                std::process::exit(1);
            }

            // Clean up the temporary Rust file
            if let Err(e) = fs::remove_file(&temp_rs_file) {
                eprintln!("Failed to clean up temporary file: {}", e);
            }

            println!("Build successful! Executable written to {}", output_path);
        }
    }
}