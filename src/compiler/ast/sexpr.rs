use std::fmt::Display;

pub use super::tree::*;

impl Display for BinaryKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryKind::Equals => "==",
            BinaryKind::NotEquals => "!=",
            BinaryKind::LessThan => "<",
            BinaryKind::LessThanEqual => "<=",
            BinaryKind::GreaterThan => ">",
            BinaryKind::GreaterThanEqual => ">=",
            BinaryKind::Plus => "+",
            BinaryKind::Minus => "-",
            BinaryKind::Multiply => "*",
            BinaryKind::Divide => "/",
            BinaryKind::And => "and",
            BinaryKind::Or => "or",
        }
        .fmt(f)
    }
}

impl Display for UnaryKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryKind::Not => "!",
            UnaryKind::Neg => "-",
        }
        .fmt(f)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => n.fmt(f),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Boolean(b) => b.fmt(f),
            Literal::Nil => "nil".fmt(f),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Assignment { id, rhs } => write!(f, "(set {} {})", id.data, rhs),
            Expression::Binary { kind, lhs, rhs } => write!(f, "({} {} {})", kind, lhs, rhs),
            Expression::Unary { kind, val } => write!(f, "({} {})", kind, val),
            Expression::Literal(lit) => lit.data.fmt(f),
            Expression::Identifier(id) => id.data.fmt(f),
        }
    }
}

fn print_many<T: Display>(
    mut it: impl Iterator<Item = T>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    "(".fmt(f)?;
    if let Some(stmt) = it.next() {
        stmt.fmt(f)?;
        for stmt in it {
            write!(f, " {stmt}")?;
        }
    }
    ")".fmt(f)
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => expr.fmt(f),
            Statement::Print(expr) => expr.fmt(f),
            Statement::VarDeclaration { id, rhs } => {
                write!(f, "(var {}", id.data)?;
                if let Some(rhs) = rhs {
                    write!(f, " {}", rhs)?;
                }
                ")".fmt(f)
            },
            Statement::Block(body) => print_many(body.iter(), f),
            Statement::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "(if {cond} ",)?;
                print_many(then_branch.iter(), f)?;
                if let Some(branch) = else_branch {
                    print_many(branch.iter(), f)?;
                }
                ")".fmt(f)
            }
            Statement::While { cond, body } => {
                write!(f, "(while {cond} ")?;
                print_many(body.iter(), f)?;
                ")".fmt(f)
            },
            Statement::Return { span, value } => {
                write!(f, "(return")?;
                if let Some(value) = value {
                    write!(f, " {}", value)?;
                }
                ")".fmt(f)
            },
        }
    }
}
