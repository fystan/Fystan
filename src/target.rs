use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOS {
    Linux,
    Windows,
    WebAssembly,
    Android,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArch {
    X86_64,
    AArch64,
    Wasm32,
    Wasm64,
}

#[derive(Debug, Clone)]
pub struct Target {
    pub os: TargetOS,
    pub arch: TargetArch,
}

impl Target {
    pub fn new(os: TargetOS, arch: TargetArch) -> Result<Self, String> {
        let target = Self { os, arch };
        if !target.is_valid_combination() {
            return Err(format!("Invalid target combination: OS={:?}, Arch={:?}", os, arch));
        }
        Ok(target)
    }

    pub fn from_string(target_str: &str) -> Result<Self, String> {
        let parts: Vec<&str> = target_str.split(':').collect();
        if parts.len() != 2 {
            return Err("Invalid target format. Use OS:Arch (e.g., linux:x86_64)".to_string());
        }

        let os = match parts[0].to_lowercase().as_str() {
            "linux" => TargetOS::Linux,
            "windows" => TargetOS::Windows,
            "wasm" => TargetOS::WebAssembly,
            "android" => TargetOS::Android,
            _ => return Err(format!("Unsupported OS: {}", parts[0])),
        };

        let arch = match parts[1].to_lowercase().as_str() {
            "x86_64" | "amd64" => TargetArch::X86_64,
            "aarch64" | "arm64" => TargetArch::AArch64,
            "wasm32" => TargetArch::Wasm32,
            "wasm64" => TargetArch::Wasm64,
            _ => return Err(format!("Unsupported architecture: {}", parts[1])),
        };

        Self::new(os, arch)
    }

    pub fn is_valid_combination(&self) -> bool {
        matches!((self.os, self.arch),
            (TargetOS::Windows, TargetArch::X86_64) |
            (TargetOS::Windows, TargetArch::AArch64) |
            (TargetOS::Linux, TargetArch::X86_64) |
            (TargetOS::Linux, TargetArch::AArch64) |
            (TargetOS::Android, TargetArch::AArch64) |
            (TargetOS::WebAssembly, TargetArch::Wasm32) |
            (TargetOS::WebAssembly, TargetArch::Wasm64)
        )
    }

    pub fn to_rust_triple(&self) -> &str {
        match (self.os, self.arch) {
            (TargetOS::Linux, TargetArch::X86_64) => "x86_64-unknown-linux-gnu",
            (TargetOS::Linux, TargetArch::AArch64) => "aarch64-unknown-linux-gnu",
            (TargetOS::Windows, TargetArch::X86_64) => "x86_64-pc-windows-msvc",
            (TargetOS::Windows, TargetArch::AArch64) => "aarch64-pc-windows-msvc",
            (TargetOS::WebAssembly, TargetArch::Wasm32) => "wasm32-unknown-unknown",
            (TargetOS::WebAssembly, TargetArch::Wasm64) => "wasm64-unknown-unknown",
            (TargetOS::Android, TargetArch::AArch64) => "aarch64-linux-android",
            _ => "unknown-unknown-unknown", // Should not happen due to validation
        }
    }

    pub fn to_c_triple(&self) -> &str {
        match (self.os, self.arch) {
            (TargetOS::Linux, TargetArch::X86_64) => "x86_64-linux-gnu",
            (TargetOS::Linux, TargetArch::AArch64) => "aarch64-linux-gnu",
            (TargetOS::Windows, TargetArch::X86_64) => "x86_64-w64-mingw32",
            (TargetOS::Windows, TargetArch::AArch64) => "aarch64-w64-mingw32",
            (TargetOS::WebAssembly, TargetArch::Wasm32) => "wasm32-unknown-emscripten",
            (TargetOS::WebAssembly, TargetArch::Wasm64) => "wasm64-unknown-emscripten",
            (TargetOS::Android, TargetArch::AArch64) => "aarch64-linux-android",
            _ => "unknown-unknown-unknown",
        }
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_rust_triple())
    }
}