use crate::lexer::token::Token;
use std::fmt::{Display, Formatter, Result};
use std::hash::Hash;
use std::collections::HashMap;

// Common traits
pub trait Node: Display {
    fn token_literal(&self) -> String;
}
pub trait Expression: Node {}

// Operator enums
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrefixOperator {
    Minus, // -
    Not,   // not
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InfixOperator {
    Plus,       // +
    Minus,      // -
    Multiply,   // *
    Divide,     // /
    Mod,        // %
    Eq,         // ==
    NotEq,      // !=
    Is,         // is
    IsNot,      // is not
    Lt,         // <
    Gt,         // >
    And,        // and
    Or,         // or
    Assign,     // =
    PlusEq,     // +=
    MinusEq,    // -=
    AsteriskEq, // *=
    SlashEq,    // /=
    Power,      // **
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            PrefixOperator::Not => "not",
            PrefixOperator::Minus => "-",
        };
        write!(f, "{}", s)
    }
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            InfixOperator::Plus => "+",
            InfixOperator::Minus => "-",
            InfixOperator::Multiply => "*",
            InfixOperator::Divide => "/",
            InfixOperator::Mod => "%",
            InfixOperator::Eq => "==",
            InfixOperator::NotEq => "!=",
            InfixOperator::Is => "is",
            InfixOperator::IsNot => "is not",
            InfixOperator::Lt => "<",
            InfixOperator::Gt => ">",
            InfixOperator::And => "and",
            InfixOperator::Or => "or",
            InfixOperator::Assign => "=",
            InfixOperator::PlusEq => "+=",
            InfixOperator::MinusEq => "-=",
            InfixOperator::AsteriskEq => "*=",
            InfixOperator::SlashEq => "/=",
            InfixOperator::Power => "**",
        };
        write!(f, "{}", s)
    }
}

// Statement enum
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Pass(PassStatement),
    Try(TryStatement),
    If(IfStatement),
    Class(ClassDefinition),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Return(s) => s.token_literal(),
            Statement::Expression(s) => s.token_literal(),
            Statement::Break(s) => s.token_literal(),
            Statement::Continue(s) => s.token_literal(),
            Statement::Pass(s) => s.token_literal(),
            Statement::Try(s) => s.token_literal(),
            Statement::If(s) => s.token_literal(),
            Statement::Class(s) => s.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
            Statement::Break(s) => write!(f, "{}", s),
            Statement::Continue(s) => write!(f, "{}", s),
            Statement::Pass(s) => write!(f, "{}", s),
            Statement::Try(s) => write!(f, "{}", s),
            Statement::If(s) => write!(f, "{}", s),
            Statement::Class(s) => write!(f, "{}", s),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionEnum {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    StringLiteral(StringLiteral),
    Boolean(Boolean),
    None(NoneLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
    HashLiteral(HashLiteral),
    While(WhileExpression),
    For(ForExpression),
}

impl Display for ExpressionEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ExpressionEnum::Identifier(e) => write!(f, "{}", e),
            ExpressionEnum::IntegerLiteral(e) => write!(f, "{}", e),
            ExpressionEnum::FloatLiteral(e) => write!(f, "{}", e),
            ExpressionEnum::StringLiteral(e) => write!(f, "{}", e),
            ExpressionEnum::Boolean(e) => write!(f, "{}", e),
            ExpressionEnum::None(e) => write!(f, "{}", e),
            ExpressionEnum::Prefix(e) => write!(f, "{}", e),
            ExpressionEnum::Infix(e) => write!(f, "{}", e),
            ExpressionEnum::If(e) => write!(f, "{}", e),
            ExpressionEnum::Function(e) => write!(f, "{}", e),
            ExpressionEnum::Call(e) => write!(f, "{}", e),
            ExpressionEnum::ArrayLiteral(e) => write!(f, "{}", e),
            ExpressionEnum::IndexExpression(e) => write!(f, "{}", e),
            ExpressionEnum::HashLiteral(e) => write!(f, "{}", e),
            ExpressionEnum::While(e) => write!(f, "{}", e),
            ExpressionEnum::For(e) => write!(f, "{}", e),
        }
    }
}

impl Node for ExpressionEnum {
    fn token_literal(&self) -> String {
        match self {
            ExpressionEnum::Identifier(e) => e.token_literal(),
            ExpressionEnum::IntegerLiteral(e) => e.token_literal(),
            ExpressionEnum::FloatLiteral(e) => e.token_literal(),
            ExpressionEnum::StringLiteral(e) => e.token_literal(),
            ExpressionEnum::Boolean(e) => e.token_literal(),
            ExpressionEnum::None(e) => e.token_literal(),
            ExpressionEnum::Prefix(e) => e.token_literal(),
            ExpressionEnum::Infix(e) => e.token_literal(),
            ExpressionEnum::If(e) => e.token_literal(),
            ExpressionEnum::Function(e) => e.token_literal(),
            ExpressionEnum::Call(e) => e.token_literal(),
            ExpressionEnum::ArrayLiteral(e) => e.token_literal(),
            ExpressionEnum::IndexExpression(e) => e.token_literal(),
            ExpressionEnum::HashLiteral(e) => e.token_literal(),
            ExpressionEnum::While(e) => e.token_literal(),
            ExpressionEnum::For(e) => e.token_literal(),
        }
    }
}

impl ExpressionEnum {
    pub fn token(&self) -> Token {
        match self {
            ExpressionEnum::Identifier(e) => e.token.clone(),
            ExpressionEnum::IntegerLiteral(e) => e.token.clone(),
            ExpressionEnum::FloatLiteral(e) => e.token.clone(),
            ExpressionEnum::StringLiteral(e) => e.token.clone(),
            ExpressionEnum::Boolean(e) => e.token.clone(),
            ExpressionEnum::None(e) => e.token.clone(),
            ExpressionEnum::Prefix(e) => e.token.clone(),
            ExpressionEnum::Infix(e) => e.token.clone(),
            ExpressionEnum::If(e) => e.token.clone(),
            ExpressionEnum::Function(e) => e.token.clone(),
            ExpressionEnum::Call(e) => e.token.clone(),
            ExpressionEnum::ArrayLiteral(e) => e.token.clone(),
            ExpressionEnum::IndexExpression(e) => e.token.clone(),
            ExpressionEnum::HashLiteral(e) => e.token.clone(),
            ExpressionEnum::While(e) => e.token.clone(),
            ExpressionEnum::For(e) => e.token.clone(),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}


// Class definition
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub token: Token,
    pub name: Identifier,
    pub body: BlockStatement,
}

impl Node for ClassDefinition {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ClassDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "class {} {{ {} }}", self.name, self.body)
    }
}

// If statement
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub token: Token,
    pub condition: ExpressionEnum,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{ {} }}", alt)?;
        }
        Ok(())
    }
}

// Return statement
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: ExpressionEnum,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.token_literal(), self.return_value)
    }
}

// Break statement
#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub token: Token,
}

impl Node for BreakStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for BreakStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token_literal())
    }
}

// Continue statement
#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub token: Token,
}

impl Node for ContinueStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ContinueStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token_literal())
    }
}

// Expression statement
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: ExpressionEnum,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.expression)
    }
}

// Block statement
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

// Identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for Identifier {}
impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

// Integer literal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for IntegerLiteral {}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

// Float literal
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub token: Token,
    pub value: f64,
}

impl Node for FloatLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for FloatLiteral {}
impl Display for FloatLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self.value)
    }
}

// String literal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for StringLiteral {}
impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "\"{}\"", self.value)
    }
}

// Boolean
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for Boolean {}
impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

// Prefix expression
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: PrefixOperator,
    pub right: Box<ExpressionEnum>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for PrefixExpression {}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}{})", self.operator.to_string(), self.right)
    }
}

// Infix expression
#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<ExpressionEnum>,
    pub operator: InfixOperator,
    pub right: Box<ExpressionEnum>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for InfixExpression {}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({} {} {})", self.left, self.operator.to_string(), self.right)
    }
}

// If expression
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<ExpressionEnum>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for IfExpression {}
impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{ {} }}", alt)?;
        }
        Ok(())
    }
}

// While expression
#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpression {
    pub token: Token,
    pub condition: Box<ExpressionEnum>,
    pub body: BlockStatement,
}

impl Node for WhileExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for WhileExpression {}

impl Display for WhileExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "while ({}) {}", self.condition, self.body)
    }
}

// For expression
#[derive(Debug, Clone, PartialEq)]
pub struct ForExpression {
    pub token: Token, // The 'for' token
    pub element: Identifier, // The variable for each element
    pub iterable: Box<ExpressionEnum>, // The expression being iterated over
    pub body: BlockStatement, // The loop body
}

impl Node for ForExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for ForExpression {}

impl Display for ForExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "for ({} in {}) {}",
            self.element, self.iterable, self.body
        )
    }
}


// Function literal
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for FunctionLiteral {}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({}) {} {}", self.token_literal(), self.name, params.join(", "), self.body)
    }
}

// Call expression
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<ExpressionEnum>,
    pub arguments: Vec<ExpressionEnum>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for CallExpression {}
impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let args: Vec<String> = self.arguments.iter().map(|a| a.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

// Array literal
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<ExpressionEnum>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for ArrayLiteral {}
impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let elements: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

// Index expression
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<ExpressionEnum>,
    pub index: Box<ExpressionEnum>,
}

impl Node for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for IndexExpression {}
impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

// Hash literal
#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<String, ExpressionEnum>,
}

impl Node for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for HashLiteral {}
impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pairs: Vec<String> = self.pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

// None literal
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NoneLiteral {
    pub token: Token,
}

impl Node for NoneLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for NoneLiteral {}
impl Display for NoneLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "None")
    }
}

// Pass statement
#[derive(Debug, Clone, PartialEq)]
pub struct PassStatement {
    pub token: Token,
}

impl Node for PassStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for PassStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "pass")
    }
}

// Try statement
#[derive(Debug, Clone, PartialEq)]
pub struct TryStatement {
    pub token: Token,
    pub body: BlockStatement,
    pub except_clauses: Vec<ExceptClause>,
    pub else_block: Option<BlockStatement>,
    pub finally_block: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExceptClause {
    pub token: Token,
    pub exception_type: Option<ExpressionEnum>,
    pub exception_name: Option<String>,
    pub body: BlockStatement,
}

impl Node for TryStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for TryStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "try {{ {} }}", self.body)?;
        for except in &self.except_clauses {
            write!(f, " {}", except)?;
        }
        if let Some(ref else_block) = self.else_block {
            write!(f, " else {{ {} }}", else_block)?;
        }
        if let Some(ref finally_block) = self.finally_block {
            write!(f, " finally {{ {} }}", finally_block)?;
        }
        Ok(())
    }
}

impl Display for ExceptClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "except")?;
        if let Some(ref exc_type) = self.exception_type {
            write!(f, " {}", exc_type)?;
            if let Some(ref exc_name) = self.exception_name {
                write!(f, " as {}", exc_name)?;
            }
        }
        write!(f, " {{ {} }}", self.body)
    }
}
