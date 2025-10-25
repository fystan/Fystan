pub mod token;

use self::token::{Token, TokenType};

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    ch: Option<char>,
    indent_stack: Vec<usize>,
    tokens: Vec<Token>,
    at_line_start: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            chars: input.chars().peekable(),
            ch: None,
            indent_stack: vec![0],
            tokens: Vec::new(),
            at_line_start: true,
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

    fn handle_indentation(&mut self) {
        if !self.at_line_start {
            return;
        }

        loop {
            let mut current_indent = 0;
            while let Some(' ') = self.ch {
                current_indent += 1;
                self.read_char();
            }

            if self.ch == Some('#') {
                while self.ch.is_some() && self.ch != Some('\n') {
                    self.read_char();
                }
            }

            if self.ch == Some('\n') {
                self.read_char();
                continue;
            }

            if self.ch.is_none() {
                break;
            }

            self.at_line_start = false;
            let last_indent = *self.indent_stack.last().unwrap();

            if current_indent > last_indent {
                self.indent_stack.push(current_indent);
                self.tokens.push(Token::new(TokenType::Indent, "INDENT".to_string()));
            } else if current_indent < last_indent {
                while let Some(&last) = self.indent_stack.last() {
                    if current_indent < last {
                        self.indent_stack.pop();
                        self.tokens.push(Token::new(TokenType::Dedent, "DEDENT".to_string()));
                    } else {
                        break;
                    }
                }
                if *self.indent_stack.last().unwrap() != current_indent {
                    self.tokens.push(Token::new(TokenType::Illegal, "IndentationError".to_string()));
                }
            }

            break;
        }
    }


    pub fn next_token(&mut self) -> Token {
        if !self.tokens.is_empty() {
            return self.tokens.remove(0);
        }
        
        if self.at_line_start {
            self.handle_indentation();
            if !self.tokens.is_empty() {
                return self.tokens.remove(0);
            }
        }


        self.skip_whitespace();

        let token = match self.ch {
            Some('@') => {
                self.read_char();
                Token::new(TokenType::At, "@".to_string())
            }
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
                    Token::new(TokenType::Illegal, "!".to_string())
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
                if self.peek_char() == Some('*') {
                    self.read_char();
                    Token::new(TokenType::Power, "**".to_string())
                } else if self.peek_char() == Some('=') {
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
            Some('\n') => {
                self.at_line_start = true;
                Token::new(TokenType::Newline, "\n".to_string())
            }
            Some('(') => Token::new(TokenType::Lparen, "(".to_string()),
            Some(')') => Token::new(TokenType::Rparen, ")".to_string()),
            Some('{') => Token::new(TokenType::LBrace, "{".to_string()),
            Some('}') => Token::new(TokenType::RBrace, "}".to_string()),
            Some(',') => Token::new(TokenType::Comma, ",".to_string()),
            Some('<') => Token::new(TokenType::Lt, "<".to_string()),
            Some('>') => Token::new(TokenType::Gt, ">".to_string()),
            Some('[') => Token::new(TokenType::LBrack, "[".to_string()),
            Some(']') => Token::new(TokenType::RBrack, "]".to_string()),
            Some(':') => Token::new(TokenType::Colon, ":".to_string()),
            Some('.') => Token::new(TokenType::Dot, ".".to_string()),
            Some('%') => Token::new(TokenType::Mod, "%".to_string()),
            Some('"') => {
                if self.peek_char() == Some('"') && self.chars.clone().nth(1) == Some('"') {
                    self.read_char();
                    self.read_char();
                    return Token::new(TokenType::String, self.read_multiline_string('"'));
                }
                Token::new(TokenType::String, self.read_string('"'))
            },
            Some('\'') => {
                if self.peek_char() == Some('\'') && self.chars.clone().nth(1) == Some('\'') {
                    self.read_char();
                    self.read_char();
                    return Token::new(TokenType::String, self.read_multiline_string('\''));
                }
                Token::new(TokenType::String, self.read_string('\''))
            },
            Some('#') => {
                self.skip_comment();
                return self.next_token();
            }
            None => {
                // End of file, emit remaining Dedent tokens
                while self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    self.tokens.push(Token::new(TokenType::Dedent, "DEDENT".to_string()));
                }
                if !self.tokens.is_empty() {
                    return self.tokens.remove(0);
                }
                Token::new(TokenType::Eof, "".to_string())
            }
            Some(ch) => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    let token_type = Token::lookup_ident(&literal);
                    return Token::new(token_type, literal);
                } else if is_digit(ch) {
                    let (literal, is_float) = self.read_number();
                    if is_float {
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
    
    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(ch) = self.ch {
            if is_letter(ch) || is_digit(ch) {
                identifier.push(ch);
                self.read_char();
            } else {
                break;
            }
        }
        identifier
    }

    fn read_number(&mut self) -> (String, bool) {
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
        (number, has_decimal)
    }

    fn read_string(&mut self, quote: char) -> String {
        let mut s = String::new();
        self.read_char(); // Consume the opening quote
        loop {
            match self.ch {
                Some(ch) if ch == quote => {
                    break;
                }
                None => break, // Unclosed string
                Some('\\') => {
                    self.read_char();
                    match self.ch {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('\\') => s.push('\\'),
                        Some('\'') => s.push('\''),
                        Some('"') => s.push('"'),
                        Some(other) => {
                            s.push('\\');
                            s.push(other);
                        }
                        None => s.push('\\'), // Trailing backslash
                    }
                }
                Some(ch) => s.push(ch),
            }
            self.read_char();
        }
        s
    }

    fn read_multiline_string(&mut self, quote: char) -> String {
        let mut s = String::new();
        self.read_char(); // Consume the first opening quote
        loop {
            match self.ch {
                Some(ch) if ch == quote => {
                    if self.peek_char() == Some(quote) && self.chars.clone().nth(1) == Some(quote) {
                        self.read_char();
                        self.read_char();
                        self.read_char(); // Consume the third closing quote
                        break; // End of multiline string
                    }
                    s.push(ch);
                }
                None => break, // Unclosed string
                Some(ch) => s.push(ch),
            }
            self.read_char();
        }
        s
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.read_char();
            } else {
                break;
            }
        }
    }
    
    fn skip_comment(&mut self) {
        while self.ch.is_some() && self.ch != Some('\n') {
            self.read_char();
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
    fn test_simple_indentation() {
        let input = "def main():\n    pass";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::new(TokenType::Def, "def".to_string()),
            Token::new(TokenType::Ident, "main".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Colon, ":".to_string()),
            Token::new(TokenType::Newline, "\n".to_string()),
            Token::new(TokenType::Indent, "INDENT".to_string()),
            Token::new(TokenType::Pass, "pass".to_string()),
            Token::new(TokenType::Dedent, "DEDENT".to_string()),
            Token::new(TokenType::Eof, "".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected_token.token_type, "token type mismatch");
            assert_eq!(token.literal, expected_token.literal, "token literal mismatch");
        }
    }

    #[test]
    fn test_multiline_string() {
        let input = r#"print("""hello
world""")"#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::new(TokenType::Ident, "print".to_string()),
            Token::new(TokenType::Lparen, "(".to_string()),
            Token::new(TokenType::String, "hello\nworld".to_string()),
            Token::new(TokenType::Rparen, ")".to_string()),
            Token::new(TokenType::Eof, "".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected_token.token_type, "token type mismatch for {:?}", expected_token);
            assert_eq!(token.literal, expected_token.literal, "token literal mismatch for {:?}", expected_token);
        }
    }
}