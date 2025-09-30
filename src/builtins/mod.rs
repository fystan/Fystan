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
        m.insert("range", Builtin {
            name: "range",
            min_args: 1,
            max_args: 3, // range(stop) or range(start, stop) or range(start, stop, step)
        });
        m.insert("str", Builtin {
            name: "str",
            min_args: 1,
            max_args: 1,
        });
        m.insert("int", Builtin {
            name: "int",
            min_args: 1,
            max_args: 1,
        });
        m.insert("bool", Builtin {
            name: "bool",
            min_args: 1,
            max_args: 1,
        });
        m.insert("float", Builtin {
            name: "float",
            min_args: 1,
            max_args: 1,
        });
        m.insert("list", Builtin {
            name: "list",
            min_args: 0,
            max_args: 1,
        });
        m.insert("tuple", Builtin {
            name: "tuple",
            min_args: 0,
            max_args: 1,
        });
        m
    };
}

pub fn get_builtin(name: &str) -> Option<&'static Builtin> {
    BUILTINS.get(name)
}
