use crate::ast::Program;
use crate::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, ExpressionEnum, ExpressionStatement,
    FloatLiteral, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
    InfixOperator, IntegerLiteral, PrefixExpression, PrefixOperator,
    ReturnStatement, StringLiteral, Statement, WhileExpression, BreakStatement, ContinueStatement, ForExpression,
    NoneLiteral, PassStatement, TryStatement, ExceptClause,
};
use crate::lexer::{Lexer, token::{Token, TokenType}};
use std::collections::HashMap;
use lazy_static::lazy_static; 

#[derive(PartialOrd, PartialEq, Eq, Debug, Clone)]
enum Precedence {
    Lowest,
    Assign,      // =
    Or,          // ||
    And,         // &&
    Equals,      // == or !=
    LessGreater, // < or >
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut map = HashMap::new();
        map.insert(TokenType::Eq, Precedence::Equals);
        map.insert(TokenType::NotEq, Precedence::Equals);
        map.insert(TokenType::Is, Precedence::Equals);
        map.insert(TokenType::IsNot, Precedence::Equals);
        map.insert(TokenType::Lt, Precedence::LessGreater);
        map.insert(TokenType::Gt, Precedence::LessGreater);
        map.insert(TokenType::Plus, Precedence::Sum);
        map.insert(TokenType::Minus, Precedence::Sum);
        map.insert(TokenType::Asterisk, Precedence::Product);
        map.insert(TokenType::Slash, Precedence::Product);
        map.insert(TokenType::Mod, Precedence::Product);
        map.insert(TokenType::Lparen, Precedence::Call);
        map.insert(TokenType::LBrack, Precedence::Index);
        map.insert(TokenType::Dot, Precedence::Call);
        map.insert(TokenType::Assign, Precedence::Assign);
        map.insert(TokenType::And, Precedence::And);
        map.insert(TokenType::Or, Precedence::Or);
        map.insert(TokenType::PlusEq, Precedence::Assign);
        map.insert(TokenType::MinusEq, Precedence::Assign);
        map.insert(TokenType::AsteriskEq, Precedence::Assign);
        map.insert(TokenType::SlashEq, Precedence::Assign);
        map
    };
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Parser<'a> {
        let mut p = Parser {
            l,
            cur_token: Token::new_empty(),
            peek_token: Token::new_empty(),
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.cur_token_is(TokenType::Eof) {
            if self.cur_token_is(TokenType::Newline) {
                self.next_token();
                continue;
            }
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Return => self.parse_return_statement().map(Statement::Return),
            TokenType::Break => self.parse_break_statement().map(Statement::Break),
            TokenType::Continue => self.parse_continue_statement().map(Statement::Continue),
            TokenType::Pass => self.parse_pass_statement().map(Statement::Pass),
            TokenType::Try => self.parse_try_statement().map(Statement::Try),
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        Some(ReturnStatement {
            token,
            return_value,
        })
    }

    fn parse_break_statement(&mut self) -> Option<BreakStatement> {
        let token = self.cur_token.clone();
        Some(BreakStatement { token })
    }

    fn parse_continue_statement(&mut self) -> Option<ContinueStatement> {
        let token = self.cur_token.clone();
        Some(ContinueStatement { token })
    }

    fn parse_pass_statement(&mut self) -> Option<PassStatement> {
        let token = self.cur_token.clone();
        Some(PassStatement { token })
    }

    fn parse_try_statement(&mut self) -> Option<TryStatement> {
        let token = self.cur_token.clone(); // 'try' token

        // Expect colon after 'try'
        if !self.expect_peek(TokenType::Colon) {
            return None;
        }

        // Expect newline and indent for the try block
        if !self.expect_peek(TokenType::Newline) { return None; }
        if !self.expect_peek(TokenType::Indent) { return None; }

        let body = self.parse_indented_block_statement()?;

        // After the try block, consume the Dedent
        if self.peek_token_is(&TokenType::Dedent) {
            self.next_token(); // consume the Dedent that ends the try block
        }

        let mut except_clauses = Vec::new();
        // Parse except clauses if present
        while self.peek_token_is(&TokenType::Except) {
            self.next_token(); // consume Except
            let except_clause = self.parse_except_clause()?;
            except_clauses.push(except_clause);

            // After each except block, consume the Dedent
            if self.peek_token_is(&TokenType::Dedent) {
                self.next_token(); // consume the Dedent that ends the except block
            }
        }

        // Optionally parse else clause
        let mut else_block = None;
        if self.peek_token_is(&TokenType::Else) {
            self.next_token(); // consume Else
            if !self.cur_token_is(TokenType::Colon) {
                self.errors.push(format!("expected ':', got {:?}", self.cur_token.token_type));
                return None;
            }

            if !self.expect_peek(TokenType::Newline) { return None; }
            if !self.expect_peek(TokenType::Indent) { return None; }

            else_block = Some(self.parse_indented_block_statement()?);
            
            // Consume the Dedent after else block if present
            if self.peek_token_is(&TokenType::Dedent) {
                self.next_token(); // consume the Dedent that ends the else block
            }
        }

        // Optionally parse finally clause  
        let mut finally_block = None;
        if self.peek_token_is(&TokenType::Finally) {
            self.next_token(); // consume Finally
            if !self.cur_token_is(TokenType::Colon) {
                self.errors.push(format!("expected ':', got {:?}", self.cur_token.token_type));
                return None;
            }

            if !self.expect_peek(TokenType::Newline) { return None; }
            if !self.expect_peek(TokenType::Indent) { return None; }

            finally_block = Some(self.parse_indented_block_statement()?);
        }

        Some(TryStatement {
            token,
            body,
            except_clauses,
            else_block,
            finally_block,
        })
    }

    fn parse_except_clause(&mut self) -> Option<ExceptClause> {
        let token = self.cur_token.clone(); // 'except' token

        // Check if there's an exception type specified
        let mut exception_type = None;
        let mut exception_name = None;

        // If next token is not colon, it might be an exception type
        if !self.peek_token_is(&TokenType::Colon) && 
           !self.peek_token_is(&TokenType::Newline) {
            self.next_token(); // consume the exception type token
            exception_type = self.parse_expression(Precedence::Lowest);

            // Check for 'as' clause to catch the exception with a name
            if self.peek_token_is(&TokenType::As) {
                self.next_token(); // consume 'as'
                self.next_token(); // consume the identifier after 'as'
                
                if self.cur_token.token_type == TokenType::Ident {
                    exception_name = Some(self.cur_token.literal.clone());
                } else {
                    self.errors.push(format!("expected identifier after 'as', got {:?}", self.cur_token.token_type));
                    return None;
                }
            }
        }

        if !self.expect_peek(TokenType::Colon) {
            return None;
        }

        if !self.expect_peek(TokenType::Newline) { return None; }
        if !self.expect_peek(TokenType::Indent) { return None; }

        let body = self.parse_indented_block_statement()?;

        Some(ExceptClause {
            token,
            exception_type,
            exception_name,
            body,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        Some(ExpressionStatement { token, expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ExpressionEnum> {
        let mut left_exp = self.parse_prefix()?;

        while precedence < self.peek_precedence() {
            match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::Mod
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Is
                | TokenType::IsNot
                | TokenType::Lt
                | TokenType::Gt
                | TokenType::And
                | TokenType::Or
                | TokenType::Assign
                | TokenType::PlusEq
                | TokenType::MinusEq
                | TokenType::AsteriskEq
                | TokenType::SlashEq => {
                    self.next_token();
                    left_exp = self.parse_infix(left_exp)?;
                }
                TokenType::Lparen => {
                    self.next_token();
                    left_exp = self.parse_call_expression(left_exp)?;
                }
                TokenType::LBrack => {
                    self.next_token();
                    left_exp = self.parse_index_expression(left_exp)?;
                }
                _ => return Some(left_exp),
            }
        }
        Some(left_exp)
    }

    fn parse_prefix(&mut self) -> Option<ExpressionEnum> {
        match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Float => self.parse_float_literal(),
            TokenType::String => self.parse_string_literal(),
            TokenType::Not => self.parse_prefix_expression(PrefixOperator::Not),
            TokenType::Minus => self.parse_prefix_expression(PrefixOperator::Minus),
            TokenType::True => self.parse_boolean(true),
            TokenType::False => self.parse_boolean(false),
            TokenType::None => self.parse_none_literal(),
            TokenType::Lparen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::While => self.parse_while_expression(),
            TokenType::For => self.parse_for_expression(),
            TokenType::Def => self.parse_function_definition(),
            TokenType::LBrack => self.parse_array_literal(),
            TokenType::LBrace => self.parse_hash_literal(),
            _ => {
                self.errors.push(format!("no prefix parse function for {:?}", self.cur_token.token_type));
                None
            }
        }
    }

    fn parse_infix(&mut self, left: ExpressionEnum) -> Option<ExpressionEnum> {
        let operator = self.cur_token.token_type.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        
        match operator {
            TokenType::Plus => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Plus, "+".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Plus,
                right: Box::new(right),
            })),
            TokenType::Minus => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Minus, "-".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Minus,
                right: Box::new(right),
            })),
            TokenType::Asterisk => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Asterisk, "*".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Multiply,
                right: Box::new(right),
            })),
            TokenType::Slash => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Slash, "/".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Divide,
                right: Box::new(right),
            })),
            TokenType::Mod => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Mod, "%".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Mod,
                right: Box::new(right),
            })),
            TokenType::Eq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Eq, "==".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Eq,
                right: Box::new(right),
            })),
            TokenType::NotEq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::NotEq, "!=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::NotEq,
                right: Box::new(right),
            })),
            TokenType::Is => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Is, "is".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Is,
                right: Box::new(right),
            })),
            TokenType::IsNot => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::IsNot, "is not".to_string()),
                left: Box::new(left),
                operator: InfixOperator::IsNot,
                right: Box::new(right),
            })),
            TokenType::Lt => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Lt, "<".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Lt,
                right: Box::new(right),
            })),
            TokenType::Gt => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Gt, ">".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Gt,
                right: Box::new(right),
            })),
            TokenType::And => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::And, "and".to_string()),
                left: Box::new(left),
                operator: InfixOperator::And,
                right: Box::new(right),
            })),
            TokenType::Or => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Or, "or".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Or,
                right: Box::new(right),
            })),
            TokenType::Assign => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::Assign, "=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::Assign,
                right: Box::new(right),
            })),
            TokenType::PlusEq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::PlusEq, "+=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::PlusEq,
                right: Box::new(right),
            })),
            TokenType::MinusEq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::MinusEq, "-=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::MinusEq,
                right: Box::new(right),
            })),
            TokenType::AsteriskEq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::AsteriskEq, "*=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::AsteriskEq,
                right: Box::new(right),
            })),
            TokenType::SlashEq => Some(ExpressionEnum::Infix(InfixExpression {
                token: Token::new(TokenType::SlashEq, "/=".to_string()),
                left: Box::new(left),
                operator: InfixOperator::SlashEq,
                right: Box::new(right),
            })),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Option<ExpressionEnum> {
        Some(ExpressionEnum::Identifier(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        if token.token_type != TokenType::Int {
            self.errors.push(format!("expected integer literal, got {:?}", token.token_type));
            return None;
        }
        match self.cur_token.literal.parse::<i64>() {
            Ok(value) => Some(ExpressionEnum::IntegerLiteral(IntegerLiteral { token, value })),
            Err(_) => {
                self.errors.push(format!("could not parse {} as integer", self.cur_token.literal));
                None
            }
        }
    }

    fn parse_float_literal(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        if token.token_type != TokenType::Float {
            self.errors.push(format!("expected float literal, got {:?}", token.token_type));
            return None;
        }
        match self.cur_token.literal.parse::<f64>() {
            Ok(value) => Some(ExpressionEnum::FloatLiteral(FloatLiteral { token, value })),
            Err(_) => {
                self.errors.push(format!("could not parse {} as float", self.cur_token.literal));
                None
            }
        }
    }

    fn parse_string_literal(&mut self) -> Option<ExpressionEnum> {
        Some(ExpressionEnum::StringLiteral(StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_boolean(&mut self, value: bool) -> Option<ExpressionEnum> {
        Some(ExpressionEnum::Boolean(Boolean {
            token: self.cur_token.clone(),
            value,
        }))
    }

    fn parse_none_literal(&mut self) -> Option<ExpressionEnum> {
        Some(ExpressionEnum::None(NoneLiteral {
            token: self.cur_token.clone(),
        }))
    }

    fn parse_prefix_expression(&mut self, operator: PrefixOperator) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        self.next_token();
        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Some(ExpressionEnum::Prefix(PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<ExpressionEnum> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        exp
    }

    fn parse_if_expression(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone(); // 'if' token

        self.next_token(); // consume 'if'
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Colon) {
            return None;
        }

        if !self.expect_peek(TokenType::Newline) { return None; }
        if !self.expect_peek(TokenType::Indent) { return None; }

        let consequence = self.parse_indented_block_statement()?;

        // Check for else
        // NOTE: We don't consume the dedent that ends the consequence block here
        // because parse_indented_block_statement() may handle it internally
        // or the calling context may handle it
        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token(); // consume Else

            if !self.cur_token_is(TokenType::Colon) {
                self.errors.push(format!("expected ':', got {:?}", self.cur_token.token_type));
                return None;
            }

            if !self.expect_peek(TokenType::Newline) { return None; }
            if !self.expect_peek(TokenType::Indent) { return None; }

            Some(self.parse_indented_block_statement()?)
        } else {
            None
        };

        Some(ExpressionEnum::If(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_for_expression(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone(); // 'for' token

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }
        let element = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::In) {
            return None;
        }

        self.next_token(); // consume 'in'
        let iterable = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Colon) {
            return None;
        }

        if !self.expect_peek(TokenType::Newline) {
            return None;
        }
        if !self.expect_peek(TokenType::Indent) {
            return None;
        }

        let body = self.parse_indented_block_statement()?;

        Some(ExpressionEnum::For(ForExpression {
            token,
            element,
            iterable: Box::new(iterable),
            body,
        }))
    }

    fn parse_while_expression(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone(); // 'while' token

        self.next_token(); // consume 'while'
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::Colon) {
            return None;
        }

        if !self.expect_peek(TokenType::Newline) { return None; }
        if !self.expect_peek(TokenType::Indent) { return None; }

        let body = self.parse_indented_block_statement()?;

        Some(ExpressionEnum::While(WhileExpression {
            token,
            condition: Box::new(condition),
            body,
        }))
    }

    fn parse_function_definition(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone(); // The 'def' token

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }
        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::Colon) {
            return None;
        }
        
        // After the colon, we expect a newline and then an indented block.
        // The lexer produces a Newline token. We must consume it.
        if !self.peek_token_is(&TokenType::Newline) {
             self.errors.push(format!(
                "expected newline after function definition, got {:?}",
                self.peek_token.token_type
            ));
            return None;
        }
        self.next_token(); // Consume newline

        // Now, the next token should be an Indent token.
        if !self.peek_token_is(&TokenType::Indent) {
            self.errors.push(format!(
                "expected indented block after function definition, got {:?}",
                self.peek_token.token_type
            ));
            return None;
        }
        self.next_token(); // Consume Indent, cur_token is now Indent

        let body = self.parse_indented_block_statement()?;

        Some(ExpressionEnum::Function(FunctionLiteral {
            token,
            name,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&TokenType::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        identifiers.push(ident);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(ident);
        }
        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }
        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: ExpressionEnum) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        let arguments = self.parse_expression_list(TokenType::Rparen)?;

        Some(ExpressionEnum::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<ExpressionEnum>> {
        let mut list = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_indented_block_statement(&mut self) -> Option<BlockStatement> {
        let token = self.cur_token.clone(); // Indent token
        let mut statements = Vec::new();

        self.next_token(); // Consume Indent.

        while !self.cur_token_is(TokenType::Dedent) && !self.cur_token_is(TokenType::Eof) {
            if self.cur_token_is(TokenType::Newline) {
                self.next_token();
                continue;
            }
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Some(BlockStatement { token, statements })
    }


    fn parse_array_literal(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenType::RBrack)?;
        Some(ExpressionEnum::ArrayLiteral(ArrayLiteral {
            token,
            elements,
        }))
    }

    fn parse_index_expression(&mut self, left: ExpressionEnum) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(TokenType::RBrack) {
            return None;
        }
        Some(ExpressionEnum::IndexExpression(IndexExpression {
            token,
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    fn parse_hash_literal(&mut self) -> Option<ExpressionEnum> {
        let token = self.cur_token.clone();
        let mut pairs = HashMap::new();

        while !self.peek_token_is(&TokenType::RBrace) {
            self.next_token();

            let key = if self.cur_token_is(TokenType::String) {
                self.cur_token.literal.clone()
            } else {
                self.errors.push(format!("expected string key, got {:?}", self.cur_token.token_type));
                return None;
            };

            if !self.expect_peek(TokenType::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.insert(key, value);

            if !self.peek_token_is(&TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenType::RBrace) {
            return None;
        }

        Some(ExpressionEnum::HashLiteral(HashLiteral { token, pairs }))
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        &self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(&t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        PRECEDENCES
            .get(&self.peek_token.token_type)
            .cloned()
            .unwrap_or(Precedence::Lowest)
    }

    fn cur_precedence(&self) -> Precedence {
        PRECEDENCES
            .get(&self.cur_token.token_type)
            .cloned()
            .unwrap_or(Precedence::Lowest)
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExpressionEnum, Statement};
    use crate::lexer::Lexer;


    fn check_parser_errors(p: &Parser) {
        if p.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors", p.errors.len());
        for msg in &p.errors {
            eprintln!("Parser error: {}", msg);
        }
        panic!("Parser errors occurred");
    }


    #[test]
    fn test_return_statements() {
        let input = "return 5\nreturn 10\nreturn 993322";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            assert!(matches!(stmt, Statement::Return(_)));
        }
    }

    #[test]
    fn test_assignment_statements() {
        let input = "x = 5\ny = 10\nfoobar = 838383";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3);

        let expected_identifiers = vec!["x", "y", "foobar"];

        for (i, stmt) in program.statements.iter().enumerate() {
            if let Statement::Expression(expr_stmt) = stmt {
                if let ExpressionEnum::Infix(infix_expr) = &expr_stmt.expression {
                    assert_eq!(infix_expr.operator, InfixOperator::Assign);
                    if let ExpressionEnum::Identifier(ident) = &*infix_expr.left {
                        assert_eq!(ident.value, expected_identifiers[i]);
                    } else {
                        panic!("Left side of assignment is not an Identifier");
                    }
                } else {
                    panic!("Expression is not an InfixExpression");
                }
            } else {
                panic!("Statement is not an ExpressionStatement");
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::Identifier(ident) = &expr_stmt.expression {
                assert_eq!(ident.value, "foobar");
            } else {
                panic!("Expression is not an Identifier ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_for_expression() {
        let input = "
for i in my_array:
    i
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::For(for_expr) = &expr_stmt.expression {
                assert_eq!(for_expr.element.value, "i");
                // More detailed check for iterable can be added
                assert_eq!(for_expr.body.statements.len(), 1);
            } else {
                panic!("Expression is not a ForExpression ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_while_expression() {
        let input = "
while x < y:
    x
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::While(while_expr) = &expr_stmt.expression {
                // We can add more detailed assertions here later
                assert_eq!(while_expr.body.statements.len(), 1);
            } else {
                panic!("Expression is not a WhileExpression ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::IntegerLiteral(lit) = &expr_stmt.expression {
                assert_eq!(lit.value, 5);
            } else {
                panic!("Expression is not an IntegerLiteral ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("not 5", PrefixOperator::Not),
            ("-15", PrefixOperator::Minus),
        ];

        for (input, operator) in prefix_tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::Expression(expr_stmt) = stmt {
                if let ExpressionEnum::Prefix(prefix_expr) = &expr_stmt.expression {
                    assert_eq!(prefix_expr.operator, operator);
                    // You'd need a more robust way to test the right expression
                } else {
                    panic!("Expression is not a PrefixExpression ");
                }
            } else {
                panic!("Statement is not an ExpressionStatement ");
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5", 5, InfixOperator::Plus, 5),
            ("5 - 5", 5, InfixOperator::Minus, 5),
            ("5 * 5", 5, InfixOperator::Multiply, 5),
            ("5 / 5", 5, InfixOperator::Divide, 5),
            ("5 > 5", 5, InfixOperator::Gt, 5),
            ("5 < 5", 5, InfixOperator::Lt, 5),
            ("5 == 5", 5, InfixOperator::Eq, 5),
            ("5 != 5", 5, InfixOperator::NotEq, 5),
        ];

        for (input, _left_val, op, _right_val) in infix_tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::Expression(expr_stmt) = stmt {
                if let ExpressionEnum::Infix(infix_expr) = &expr_stmt.expression {
                    assert_eq!(infix_expr.operator, op);
                    // Again, need better testing for expressions
                } else {
                    panic!("Expression is not an InfixExpression ");
                }
            } else {
                panic!("Statement is not an ExpressionStatement ");
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("not -a ", "(not(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f ", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4", "(3 + 4)"),
            ("-5 * 5", "((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("not (true == true)", "(not(true == true))"),
            ("a + add(b * c) + d ", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
            ("a * [1, 2, 3, 4][b * c] * d ", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
        ];

        for (input, expected) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "
if x < y:
    x
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::If(if_expr) = &expr_stmt.expression {
                // More detailed checks needed here
                assert!(if_expr.alternative.is_none());
            } else {
                panic!("Expression is not an IfExpression ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "
if x < y:
    x
else:
    y
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::If(if_expr) = &expr_stmt.expression {
                assert!(if_expr.alternative.is_some());
            } else {
                panic!("Expression is not an IfExpression ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_function_definition_parsing() {
        let input = "
def my_func(x, y):
    x + y
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::Function(func_lit) = &expr_stmt.expression {
                assert_eq!(func_lit.name.value, "my_func");
                assert_eq!(func_lit.parameters.len(), 2);
                assert_eq!(func_lit.parameters[0].value, "x");
                assert_eq!(func_lit.parameters[1].value, "y");
                assert_eq!(func_lit.body.statements.len(), 1);
            } else {
                panic!("Expression is not a FunctionLiteral ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::Expression(expr_stmt) = stmt {
            if let ExpressionEnum::Call(call_expr) = &expr_stmt.expression {
                assert_eq!(call_expr.arguments.len(), 3);
            } else {
                panic!("Expression is not a CallExpression ");
            }
        } else {
            panic!("Statement is not an ExpressionStatement ");
        }
    }

}
