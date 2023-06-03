use crate::common::ui::{Span, Spanned};

use super::{FunctionDeclaration, Statement, Statements};

impl Statement {
    pub fn spanned(self) -> Spanned<Self> {
        let span = match &self {
            Statement::Expr(expr) => expr.span,
            Statement::Print(expr) => expr.span,
            Statement::VarDeclaration { id, rhs } => {
                let mut span = id.span;
                if let Some(rhs) = rhs {
                    span = span.unite(rhs.span);
                }
                span
            }
            Statement::FunctionDeclaration(FunctionDeclaration {
                name,
                args: _,
                body,
            }) => name.span.unite(body.span),
            Statement::Block(block) => block.span,
            Statement::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                let mut span = cond.span.unite(then_branch.span);
                if let Some(else_branch) = else_branch {
                    span = span.unite(else_branch.span);
                }
                span
            }
            Statement::While { cond, body } => cond.span.unite(body.span),
            Statement::Return { span, value } => {
                let mut span = *span;
                if let Some(value) = value {
                    span = span.unite(value.span);
                }
                span
            }
        };
        Spanned { data: self, span }
    }
}

impl Statements {
    pub fn spanned(self) -> Spanned<Self> {
        let span = if let Some(stmt) = self.0.first() {
            let mut span = stmt.span;
            for s in self.0.iter().skip(1) {
                span = span.unite(s.span);
            }
            span
        } else {
            Span::from(0..0)
        };
        Spanned { data: self, span }
    }
}
