pub mod token;

use self::token::{Token, TokenType};

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            chars: input.chars().peekable(),
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.ch = self.chars.next();
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(ch) = self.ch {
            if is_letter(ch) || is_digit(ch) { // Identifiers can contain digits after the first char
                identifier.push(ch);
                self.read_char();
            } else {
                break;
            }
        }
        identifier
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();
        let mut has_decimal = false;
        while let Some(ch) = self.ch {
            if is_digit(ch) {
                number.push(ch);
                self.read_char();
            } else if ch == '.' && !has_decimal && self.peek_char().map_or(false, |c| is_digit(c)) {
                number.push(ch);
                self.read_char();
                has_decimal = true;
            } else {
                break;
            }
        }
        number
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let token = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".to_string())
                } else {
                    Token::new(TokenType::Assign, "=".to_string())
                }
            }
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    Token::new(TokenType::Bang, "!".to_string())
                }
            }
            Some('&') => {
                if self.peek_char() == Some('&') {
                    self.read_char();
                    Token::new(TokenType::And, "&&".to_string())
                } else {
                    Token::new(TokenType::Illegal, "&".to_string())
                }
            }
            Some('|') => {
                if self.peek_char() == Some('|') {
                    self.read_char();
                    Token::new(TokenType::Or, "||".to_string())
                } else {
                    Token::new(TokenType::Illegal, "|".to_string())
                }
            }
            Some('+') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::PlusEq, "+=".to_string())
                } else {
                    Token::new(TokenType::Plus, "+".to_string())
                }
            }
            Some('-') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::MinusEq, "-=".to_string())
                } else {
                    Token::new(TokenType::Minus, "-".to_string())
                }
            }
            Some('*') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::AsteriskEq, "*=".to_string())
                } else {
                    Token::new(TokenType::Asterisk, "*".to_string())
                }
            }
            Some('/') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::new(TokenType::SlashEq, "/=".to_string())
                } else {
                    Token::new(TokenType::Slash, "/".to_string())
                }
            }
            Some(';') => Token::new(TokenType::Semicolon, ";".to_string()),
            Some('(') => Token::new(TokenType::Lparen, "(".to_string()),
            Some(')') => Token::new(TokenType::Rparen, ")".to_string()),
            Some(',') => Token::new(TokenType::Comma, ",".to_string()),
            Some('<') => Token::new(TokenType::Lt, "<".to_string()),
            Some('>') => Token::new(TokenType::Gt, ">".to_string()),
            Some('{') => Token::new(TokenType::LBrace, "{".to_string()),
            Some('}') => Token::new(TokenType::RBrace, "}".to_string()),
            Some('[') => Token::new(TokenType::LBrack, "[".to_string()),
            Some(']') => Token::new(TokenType::RBrack, "]".to_string()),
            Some(':') => Token::new(TokenType::Colon, ":".to_string()),
            Some('.') => Token::new(TokenType::Dot, ".".to_string()),
            Some('%') => Token::new(TokenType::Mod, "%".to_string()),
            Some('"') => {
                Token::new(TokenType::String, self.read_string())
            }
            Some('\u{0000}') | None => Token::new(TokenType::Eof, "".to_string()),
            Some(ch) => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    return Token::new(Token::lookup_ident(&literal), literal);
                } else if is_digit(ch) {
                    let literal = self.read_number();
                    if literal.contains('.') {
                        return Token::new(TokenType::Float, literal);
                    } else {
                        return Token::new(TokenType::Int, literal);
                    }
                } else {
                    Token::new(TokenType::Illegal, ch.to_string())
                }
            }
        };

        self.read_char();
        token
    }

    fn read_string(&mut self) -> String {
        let mut s = String::new();
        self.read_char(); // Consume the opening quote
        loop {
            match self.ch {
                Some('"') | Some('\u{0000}') | None => break, // End of string or EOF
                Some('\\') => { // Handle escape sequences
                    self.read_char(); // Consume '\'
                    match self.ch {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('"') => s.push('"'),
                        Some('\\') => s.push('\\'),
                        Some(other) => {
                            s.push('\\');
                            s.push(other);
                        }
                        None => s.push('\\'), // Handle case where string ends with a single '\'
                    }
                }
                Some(ch) => s.push(ch),
            }
            self.read_char();
        }
        s
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            if let Some(ch) = self.ch {
                if ch.is_whitespace() {
                    self.read_char();
                } else if ch == '/' && self.peek_char() == Some('/') { // Single-line comment
                    self.read_char(); // Consume first '/'
                    self.read_char(); // Consume second '/'
                    while let Some(c) = self.ch {
                        if c == '\n' || c == '\r' || c == '\u{0000}' {
                            break;
                        }
                        self.read_char();
                    }
                } else if ch == '/' && self.peek_char() == Some('*') { // Multi-line comment
                    self.read_char(); // Consume '/'
                    self.read_char(); // Consume '*'
                    loop {
                        if self.ch == Some('*') && self.peek_char() == Some('/') {
                            self.read_char(); // Consume '*'
                            self.read_char(); // Consume '/'
                            break;
                        }
                        if self.ch == Some('\0') || self.ch.is_none() { // EOF inside multi-line comment
                            break;
                        }
                        self.read_char();
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Token, TokenType};

    #[test]
    fn test_string_with_unrecognized_escape() {
        let input = r#""\w""#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::String);
        assert_eq!(token.literal, r"\w");
    }

    #[test]
    fn test_string_with_known_escapes() {
        let input = r#""\n\t\r\"\\""#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        assert_eq!(token.token_type, TokenType::String);
        assert_eq!(token.literal, "\n\t\r\"\\");
    }
}
