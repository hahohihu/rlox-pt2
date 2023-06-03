use crate::common::ui::{Span, Spanned};

mod sexpr;

pub type Identifier = String;

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
pub enum Expression {
    Assignment {
        id: Spanned<Identifier>,
        rhs: Box<Expression>,
    },
    Binary {
        kind: BinaryKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        kind: UnaryKind,
        val: Box<Expression>,
    },
    Literal(Spanned<Literal>),
    Identifier(Spanned<Identifier>),
}

pub type Statements = Vec<Statement>;
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expr(Expression),
    Print(Expression),
    VarDeclaration {
        id: Spanned<Identifier>,
        rhs: Option<Expression>,
    },
    Block(Statements),
    IfElse {
        cond: Expression,
        then_branch: Statements,
        else_branch: Option<Statements>,
    },
    While {
        cond: Expression,
        body: Statements,
    },
    Return {
        span: Span,
        value: Option<Expression>,
    },
}
