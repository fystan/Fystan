pub struct StringAllocator {
    arena: Vec<String>,
}

impl StringAllocator {
    pub fn new() -> Self {
        Self { arena: Vec::new() }
    }

    pub fn allocate(&mut self, s: &str) -> usize {
        if let Some(index) = self.arena.iter().position(|st| st == s) {
            return index;
        }
        let index = self.arena.len();
        self.arena.push(s.to_string());
        index
    }

    pub fn get_strings(&self) -> Vec<String> {
        self.arena.clone()
    }

    pub fn get_initialization_code(&self) -> String {
        if self.arena.is_empty() {
            return "".to_string();
        }
        let mut code = "lazy_static! {\n".to_string();
        code.push_str("    static ref STRING_ARENA: Vec<&'static str> = {\n");
        code.push_str("        vec![\n");
        for s in &self.arena {
            code.push_str(&format!("            \"{}\",\n", s.escape_default()));
        }
        code.push_str("        ]\n");
        code.push_str("    };\n");
        code.push_str("}\n");
        code
    }

    pub fn get_c_initialization_code(&self) -> String {
        if self.arena.is_empty() {
            return "".to_string();
        }
        let mut code = "const char* string_arena[] = {\n".to_string();
        for s in &self.arena {
            code.push_str(&format!("    \"{}\",\n", s.escape_default()));
        }
        code.push_str("};\n");
        code
    }
}

impl Default for StringAllocator {
    fn default() -> Self {
        Self::new()
    }
}