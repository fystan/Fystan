// ...existing code...
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,
    Float,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    PlusEq,
    MinusEq,
    AsteriskEq,
    SlashEq,
    Mod,

    // Delimiters
    Comma,
    Lparen,
    Rparen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Colon,
    Dot,

    // Indentation
    Indent,
    Dedent,
    Newline,

    // Keywords
    Let,
    Def,
    True,
    False,
    If,
    Else,
    Return,
    While,
    Break,
    Continue,
    For,
    In,
    And,
    Or,
    Not,
    Class,
    Import,
    From,
    As,
    Pass,
    Is,
    Del,
    Global,
    Nonlocal,
    Assert,
    Async,
    Await,
    Yield,
    Lambda,
    With,
    Try,
    Except,
    Finally,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }

    pub fn new_empty() -> Self {
        Self {
            token_type: TokenType::Eof,
            literal: String::new(),
        }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "let" => TokenType::Let,
            "def" => TokenType::Def,
            "True" => TokenType::True,
            "False" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "for" => TokenType::For,
            "in" => TokenType::In,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "not" => TokenType::Not,
            "class" => TokenType::Class,
            "import" => TokenType::Import,
            "from" => TokenType::From,
            "as" => TokenType::As,
            "pass" => TokenType::Pass,
            "is" => TokenType::Is,
            "del" => TokenType::Del,
            "global" => TokenType::Global,
            "nonlocal" => TokenType::Nonlocal,
            "assert" => TokenType::Assert,
            "async" => TokenType::Async,
            "await" => TokenType::Await,
            "yield" => TokenType::Yield,
            "lambda" => TokenType::Lambda,
            "with" => TokenType::With,
            "try" => TokenType::Try,
            "except" => TokenType::Except,
            "finally" => TokenType::Finally,
            _ => TokenType::Ident,
        }
    }
}