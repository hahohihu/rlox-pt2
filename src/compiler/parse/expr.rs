use crate::common::ui::{Span, Spanned};

use super::ast::*;

impl Expression {
    pub fn spanned(self) -> Spanned<Self> {
        let span = match &self {
            Expression::Assignment { id, rhs } => id.span.unite(rhs.span),
            Expression::Binary(BinaryExpr { kind, lhs, rhs }) => {
                Span::unite_many(&[kind.span, lhs.span, rhs.span])
            }
            Expression::Call(Call { callee, args }) => {
                if let Some(arg) = args.last() {
                    callee.span.unite(arg.span)
                } else {
                    callee.span
                }
            }
            Expression::Unary { kind, val } => kind.span.unite(val.span),
            Expression::Literal(lit) => lit.span,
            Expression::Identifier(id) => id.span,
        };
        Spanned { data: self, span }
    }

    pub fn literal<T: Into<Literal>>(span: Span, data: T) -> Self {
        Expression::Literal(Spanned {
            data: data.into(),
            span,
        })
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::String(StringLiteral(value))
    }
}
