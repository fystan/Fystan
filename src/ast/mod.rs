
use std::fmt;
impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PrefixOperator::Not => "not",
            PrefixOperator::Minus => "-",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        };
        write!(f, "{}", s)
    }
}
use crate::lexer::token::Token;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::collections::HashMap;

// 공통 트레이트
pub trait Node: Display {
    fn token_literal(&self) -> String;
}
pub trait Expression: Node {}

// 연산자 열거형
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
    And,        // &&
    Or,         // ||
    Assign,     // =
    PlusEq,     // +=
    MinusEq,    // -=
    AsteriskEq, // *=
    SlashEq,    // /=
}

// Statement Enum
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Pass(PassStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Return(s) => s.token_literal(),
            Statement::Expression(s) => s.token_literal(),
            Statement::Break(s) => s.token_literal(),
            Statement::Continue(s) => s.token_literal(),
            Statement::Pass(s) => s.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
            Statement::Break(s) => write!(f, "{}", s),
            Statement::Continue(s) => write!(f, "{}", s),
            Statement::Pass(s) => write!(f, "{}", s),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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


#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}


// Return Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.token_literal(), self.return_value)
    }
}

// Break Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

// Continue Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

// Expression Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

// Block Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Integer Literal
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Float Literal
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

// String Literal
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Prefix Expression
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator.to_string(), self.right)
    }
}

// Infix Expression
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator.to_string(), self.right)
    }
}

// If Expression
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{ {} }}", alt)?;
        }
        Ok(())
    }
}

// While Expression
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "while ({}) {}", self.condition, self.body)
    }
}

// For Expression
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for ({} in {}) {}",
            self.element, self.iterable, self.body
        )
    }
}


// Function Literal
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({}) {} {}", self.token_literal(), self.name, params.join(", "), self.body)
    }
}

// Call Expression
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|a| a.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

// 배열 리터럴
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let elements: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

// 인덱스 표현식
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

// 해시(HashMap) 리터럴
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let pairs: Vec<String> = self.pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

// None 리터럴
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "None")
    }
}

// Pass Statement
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "pass")
    }
}
