// ...existing code...
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,
    Float, // 부동 소수점 숫자 추가
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    And, // 논리 AND (&&)
    Or,  // 논리 OR (||)
    PlusEq,    // +=
    MinusEq,   // -=
    AsteriskEq, // *=
    SlashEq,   // /=

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    LBrace, // `{` 토큰
    RBrace, // `}` 토큰
    LBrack, // `[` 토큰
    RBrack, // `]` 토큰
    Colon,
    Dot,

    // Keywords
    Function,
    Let,
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
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "for" => TokenType::For,
            "in" => TokenType::In,
            _ => TokenType::Ident,
        }
    }
}