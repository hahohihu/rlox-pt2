use crate::common::ui::{Span, Spanned};

pub type Identifier = String;
pub type Node<T> = Box<Spanned<T>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryKind {
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryKind {
    Not,
    Neg,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub kind: Spanned<BinaryKind>,
    pub lhs: Node<Expression>,
    pub rhs: Node<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Assignment {
        id: Spanned<Identifier>,
        rhs: Node<Expression>,
    },
    Binary(BinaryExpr),
    Unary {
        kind: Spanned<UnaryKind>,
        val: Node<Expression>,
    },
    Literal(Spanned<Literal>),
    Identifier(Spanned<Identifier>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statements(pub Vec<Spanned<Statement>>);
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expr(Spanned<Expression>),
    Print(Spanned<Expression>),
    VarDeclaration {
        id: Spanned<Identifier>,
        rhs: Option<Spanned<Expression>>,
    },
    Block(Spanned<Statements>),
    IfElse {
        cond: Spanned<Expression>,
        then_branch: Spanned<Statements>,
        else_branch: Option<Spanned<Statements>>,
    },
    While {
        cond: Spanned<Expression>,
        body: Spanned<Statements>,
    },
    Return {
        span: Span,
        value: Option<Spanned<Expression>>,
    },
}
