use std::fmt::Display;

pub use super::ast::*;

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
            Expression::Assignment { id, rhs } => write!(f, "(set {} {})", id.data, rhs.data),
            Expression::Binary(BinaryExpr { kind, lhs, rhs }) => {
                write!(f, "({} {} {})", kind.data, lhs.data, rhs.data)
            }
            Expression::Call(Call { callee, args }) => {
                write!(f, "({} ", callee)?;
                fmt_list(args.iter(), f)?;
                ")".fmt(f)
            }
            Expression::Unary { kind, val } => write!(f, "({} {})", kind.data, val.data),
            Expression::Literal(lit) => lit.data.fmt(f),
            Expression::Identifier(id) => id.data.fmt(f),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => expr.data.fmt(f),
            Statement::Print(expr) => expr.data.fmt(f),
            Statement::VarDeclaration { id, rhs } => {
                write!(f, "(var {}", id.data)?;
                if let Some(rhs) = rhs {
                    write!(f, " {}", rhs.data)?;
                }
                ")".fmt(f)
            }
            Statement::FunctionDeclaration(FunctionDeclaration { name, args, body }) => {
                write!(f, "(defun {}", name.data)?;
                fmt_list(args.iter(), f)?;
                body.data.fmt(f)
            }
            Statement::Block(body) => body.data.fmt(f),
            Statement::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "(if {} {}", cond.data, then_branch)?;
                if let Some(branch) = else_branch {
                    branch.fmt(f)?;
                }
                ")".fmt(f)
            }
            Statement::While { cond, body } => {
                write!(f, "(while {} {})", cond.data, body)
            }
            Statement::Return { span: _, value } => {
                write!(f, "(return")?;
                if let Some(value) = value {
                    write!(f, " {}", value.data)?;
                }
                ")".fmt(f)
            }
        }
    }
}

fn fmt_list<T: Display>(
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

impl Display for Statements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_list(self.0.iter(), f)
    }
}
