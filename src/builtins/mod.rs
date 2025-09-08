use std::collections::HashMap;
use lazy_static::lazy_static;

#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    pub name: &'static str,
    pub min_args: usize,
    pub max_args: usize,
}

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Builtin> = {
        let mut m = HashMap::new();
        m.insert("print", Builtin {
            name: "print",
            min_args: 1,
            max_args: 1, // Start with one argument for simplicity
        });
        m.insert("len", Builtin {
            name: "len",
            min_args: 1,
            max_args: 1,
        });
        m.insert("read_line", Builtin {
            name: "read_line",
            min_args: 0,
            max_args: 0,
        });
        m
    };
}

pub fn get_builtin(name: &str) -> Option<&'static Builtin> {
    BUILTINS.get(name)
}
