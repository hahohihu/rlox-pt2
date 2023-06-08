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
            Expression::Assignment { id, rhs } => write!(f, "{} = {}", id.data, rhs.data),
            Expression::Binary(BinaryExpr { kind, lhs, rhs }) => {
                write!(f, "({} {} {})", lhs.data, kind.data, rhs.data)
            }
            Expression::Call(Call { callee, args }) => {
                write!(f, "{}", callee)?;
                fmt_list(args.iter(), f)
            }
            Expression::Unary { kind, val } => write!(f, "({}{})", kind.data, val.data),
            Expression::Literal(lit) => lit.data.fmt(f),
            Expression::Identifier(id) => id.data.fmt(f),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{expr};")?,
            Statement::Print(expr) => write!(f, "print {expr};")?,
            Statement::VarDeclaration { id, rhs } => {
                write!(f, "var {}", id.data)?;
                if let Some(rhs) = rhs {
                    write!(f, " = {}", rhs.data)?;
                }
                ";".fmt(f)?;
            }
            Statement::FunctionDeclaration(FunctionDeclaration { name, args, body }) => {
                write!(f, "fun {}(", name.data)?;
                fmt_list(args.iter(), f)?;
                ") ".fmt(f)?;
                write!(f, "{{\n{body}}}")?;
            }
            Statement::Block(body) => write!(f, "{{\n{body}}}")?,
            Statement::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} {{\n{}}}", cond.data, then_branch)?;
                if let Some(branch) = else_branch {
                    write!(f, " else {{\n{branch}}}")?;
                }
            }
            Statement::While { cond, body } => {
                write!(f, "while {} {{\n{}}}", cond.data, body)?;
            }
            Statement::Return { span: _, value } => {
                write!(f, "return")?;
                if let Some(value) = value {
                    write!(f, " {}", value.data)?;
                }
                ";".fmt(f)?;
            }
        }
        "\n".fmt(f)
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
            write!(f, ", {stmt}")?;
        }
    }
    ")".fmt(f)
}

impl Display for Statements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.0.iter() {
            write!(f, "{stmt}")?;
        }
        Ok(())
    }
}
