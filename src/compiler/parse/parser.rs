use std::io::Write;
use std::iter::Peekable;

use crate::compiler::parse::FunctionDeclaration;
#[cfg(not(feature = "verbose_parsing"))]
use crate::noop as trace;
use crate::repr::interner::InternedU8;

#[cfg(feature = "verbose_parsing")]
use tracing::trace;

use super::lex::Lexer;
use super::lex::Token;
use super::BinaryExpr;
use super::BinaryKind;
use super::Call;
use super::Expression;
use super::Literal;
use super::Precedence;
use super::Statement;
use super::Statements;
use super::UnaryKind;

use crate::common::ui;
use crate::common::ui::*;

struct Parser<'src, StdErr: Write> {
    lexer: Peekable<Lexer<'src>>,
    source: &'src str,
    stderr: StdErr,
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
    Handled,
}

impl ParseError {
    pub fn print(&self, stderr: impl Write, source: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        let report = match self {
            Self::InvalidToken(span) => Report::build(ReportKind::Error, (), ui::OFFSET)
                .with_message("Lexing error")
                .with_label(
                    Label::new(*span)
                        .with_color(Color::Red)
                        .with_message("Invalid token"),
                ),
            Self::ExpectError { expected, got } => Report::build(ReportKind::Error, (), ui::OFFSET)
                .with_message("Parse error")
                .with_label(
                    Label::new(*got)
                        .with_color(Color::Green)
                        .with_message(format!("Expected {expected}")),
                ),
            Self::Handled => return,
        };
        report.finish().write(Source::from(source), stderr).unwrap();
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

impl<'src, StdErr: Write> Parser<'src, StdErr> {
    fn new(source: &'src str, stderr: StdErr) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            lexer,
            source,
            stderr,
        }
    }

    fn mismatched_pair(
        &mut self,
        left: ui::Span,
        left_msg: &str,
        right: ui::Span,
        right_msg: &str,
    ) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_label(
                Label::new(left)
                    .with_color(Color::Red)
                    .with_message(left_msg),
            )
            .with_label(
                Label::new(right)
                    .with_color(Color::Red)
                    .with_message(right_msg),
            )
            .finish()
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
    }

    fn check_semicolon(&mut self, lhs: Span) -> ParseResult<()> {
        let next = self.pop()?;
        if next.data != Token::Semicolon {
            self.mismatched_pair(
                lhs,
                "This statement should be terminated with ;",
                next.span,
                "Expected ;",
            );
            return Err(ParseError::Handled);
        }
        Ok(())
    }

    fn simple_error(&mut self, span: ui::Span, msg: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_label(Label::new(span).with_color(Color::Red).with_message(msg))
            .finish()
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
    }

    fn eof(&self) -> Spanned<Token> {
        let len = self.source.len();
        let span = ui::Span::from(len - 1..len);
        Spanned {
            data: Token::Eof,
            span,
        }
    }

    fn pop(&mut self) -> ParseResult<Spanned<Token>> {
        match self.lexer.next() {
            Some(Ok(t)) => {
                trace!("popping {:?} - '{}'", t.data, &self.source[t.span]);
                Ok(t)
            }
            Some(Err(t)) => Err(ParseError::InvalidToken(t)),
            None => Ok(self.eof()),
        }
    }

    fn peek(&mut self) -> ParseResult<Spanned<Token>> {
        match self.lexer.peek() {
            Some(Ok(t)) => Ok(*t),
            Some(Err(t)) => Err(ParseError::InvalidToken(*t)),
            None => Ok(self.eof()),
        }
    }

    fn matches(&mut self, maybe: Token) -> Option<Spanned<Token>> {
        let Ok(token) = self.peek() else {
            return None;
        };
        if token.data == maybe {
            Some(self.pop().unwrap())
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token, expected_msg: &'static str) -> ParseResult<Span> {
        let token = self.pop()?;
        if token.data == expected {
            Ok(token.span)
        } else {
            Err(ParseError::ExpectError {
                expected: expected_msg,
                got: token.span,
            })
        }
    }

    fn variable_access_or_assignment(
        &mut self,
        ident_span: Span,
        can_assign: bool,
    ) -> ParseResult<Spanned<Expression>> {
        let name = &self.source[ident_span];
        let id = Spanned {
            data: name.to_owned(),
            span: ident_span,
        };
        if let Some(eq) = self.matches(Token::Eq) {
            if !can_assign {
                self.simple_error(eq.span, "Invalid assignment at this expression depth");
                return Err(ParseError::Handled);
            }
            let rhs = self.expression(can_assign)?;
            Ok(Expression::Assignment {
                id,
                rhs: rhs.boxed(),
            }
            .spanned())
        } else {
            Ok(Expression::Identifier(id).spanned())
        }
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, can_assign: bool) -> ParseResult<Spanned<Expression>> {
        let token = self.pop()?;
        match token.data {
            Token::Minus => Ok(Expression::Unary {
                kind: UnaryKind::Neg.with_span(token.span),
                val: self.primary(false)?.boxed(),
            }
            .spanned()),
            Token::Bang => Ok(Expression::Unary {
                kind: UnaryKind::Not.with_span(token.span),
                val: self.primary(false)?.boxed(),
            }
            .spanned()),
            Token::LParen => {
                let val = self.expression(true)?;
                let next = self.pop()?;
                if next.data != Token::RParen {
                    self.mismatched_pair(
                        token.span,
                        "This ( is unmatched",
                        next.span,
                        "There should be a ) here",
                    );
                    return Err(ParseError::Handled);
                }
                Ok(val)
            }
            Token::Num => {
                let n = self.source[token.span].parse::<f64>().unwrap();
                Ok(Expression::literal(token.span, n).spanned())
            }
            Token::True => Ok(Expression::literal(token.span, true).spanned()),
            Token::False => Ok(Expression::literal(token.span, false).spanned()),
            Token::Nil => Ok(Expression::literal(token.span, Literal::Nil).spanned()),
            Token::String => {
                let str = &self.source[token.span];
                // remove parens
                let str = &str[1..str.len() - 1];
                Ok(Expression::literal(token.span, str.to_owned()).spanned())
            }
            Token::Ident => self.variable_access_or_assignment(token.span, can_assign),
            _ => Err(ParseError::ExpectError {
                expected: "primary",
                got: token.span,
            }),
        }
    }

    fn while_loop(&mut self) -> ParseResult<Spanned<Statement>> {
        let while_token = self.pop().unwrap();
        debug_assert_eq!(while_token.data, Token::While);

        let cond = self.expression(false)?;
        let body = self.block()?;

        Ok(Statement::While { cond, body }.spanned())
    }

    fn for_loop(&mut self) -> ParseResult<Spanned<Statement>> {
        let for_token = self.pop().unwrap();
        debug_assert_eq!(for_token.data, Token::For);

        // initializer
        let peeked = self.peek()?;
        let init = match peeked.data {
            Token::Semicolon => {
                self.pop()?;
                None
            }
            Token::Var => Some(self.var_declaration()?),
            _ => Some(self.expression_statement()?),
        };

        let peeked = self.peek()?;
        let cond = if self.matches(Token::Semicolon).is_none() {
            let cond = self.expression(true)?;
            self.check_semicolon(peeked.span)?;
            cond
        } else {
            Expression::literal(peeked.span, true).spanned()
        };

        let inc = if self.peek()?.data != Token::LBrace {
            Some(self.expression(true)?)
        } else {
            None
        };

        let mut body = self.block()?;
        if let Some(inc) = inc {
            body.data.0.push(Statement::Expr(inc).spanned());
        }
        let while_loop = Statement::While { cond, body }.spanned();
        Ok(if let Some(init) = init {
            let data = Statements(vec![init, while_loop]).spanned();
            Statement::Block(data).spanned()
        } else {
            while_loop
        })
    }

    fn argument_list(&mut self) -> ParseResult<Vec<Spanned<Expression>>> {
        let lparen_span = self.expect(Token::LParen, "(")?;

        let mut args = vec![];
        let ident = self.peek()?;
        if ident.data != Token::RParen {
            args.push(self.expression(false)?);
            while self.peek()?.data == Token::Comma {
                self.pop().unwrap();
                args.push(self.expression(false)?);
            }
        }

        let rparen = self.pop()?;
        if rparen.data != Token::RParen {
            self.mismatched_pair(
                lparen_span,
                "This ( must be terminated",
                rparen.span,
                "Expected )",
            );
            return Err(ParseError::Handled);
        }

        if args.len() > u8::MAX as usize {
            self.simple_error(
                lparen_span.unite(rparen.span),
                "Cannot have more than 255 arguments",
            );
            return Err(ParseError::Handled);
        }
        Ok(args)
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn expression_bp(
        &mut self,
        min: Precedence,
        mut can_assign: bool,
    ) -> ParseResult<Spanned<Expression>> {
        let mut lhs = self.primary(can_assign)?;
        loop {
            let operation = self.peek()?;

            match operation.data {
                Token::LParen => {
                    let args = self.argument_list()?;
                    lhs = Expression::Call(Call {
                        callee: lhs.boxed(),
                        args,
                    })
                    .spanned();
                    continue;
                }
                _ => {}
            }

            let Ok(kind) = BinaryKind::try_from(operation.data) else {
                break;
            };
            let prec = Precedence::from(kind);
            if prec <= min {
                break;
            }
            self.pop().unwrap();

            can_assign = prec <= Precedence::Assignment;

            let rhs = self.expression_bp(prec, can_assign)?;
            lhs = Expression::Binary(BinaryExpr {
                kind: kind.with_span(operation.span),
                lhs: lhs.boxed(),
                rhs: rhs.boxed(),
            })
            .spanned()
        }
        Ok(lhs)
    }

    fn expression(&mut self, can_assign: bool) -> ParseResult<Spanned<Expression>> {
        self.expression_bp(Precedence::Start, can_assign)
    }

    fn block(&mut self) -> ParseResult<Spanned<Statements>> {
        let lbrace = self.pop().unwrap();
        if lbrace.data != Token::LBrace {
            return Err(ParseError::ExpectError {
                expected: "{",
                got: lbrace.span,
            });
        }
        let mut body = vec![];
        loop {
            let token = self.peek()?;
            if let Token::RBrace | Token::Eof = token.data {
                break;
            }
            body.push(self.declaration()?);
        }

        let rbrace = self.pop()?;
        if rbrace.data == Token::RBrace {
            Ok(Statements(body).spanned())
        } else {
            self.mismatched_pair(
                lbrace.span,
                "This { must be terminated",
                rbrace.span,
                "Expected }",
            );
            Err(ParseError::Handled)
        }
    }

    fn print_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let print_token = self.pop()?;
        debug_assert_eq!(print_token.data, Token::Print);
        let expr = self.expression(false)?;
        self.check_semicolon(print_token.span)?;
        Ok(Statement::Print(expr).spanned())
    }

    fn expression_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let expr_span = self.peek();
        let expr = self.expression(true)?;
        let expr_span = expr_span.unwrap().span;
        self.check_semicolon(expr_span)?;
        Ok(Statement::Expr(expr).spanned())
    }

    fn var_declaration(&mut self) -> ParseResult<Spanned<Statement>> {
        let var = self.pop().unwrap();
        debug_assert_eq!(var.data, Token::Var);

        let namespan = self.expect(Token::Ident, "identifier")?;

        let rhs = if self.matches(Token::Eq).is_some() {
            Some(self.expression(true)?)
        } else {
            None
        };

        self.check_semicolon(var.span)?;

        let id = Spanned {
            data: self.source[namespan].to_owned(),
            span: namespan,
        };
        Ok(Statement::VarDeclaration { id, rhs }.spanned())
    }

    fn if_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        let if_token = self.pop().unwrap();
        debug_assert_eq!(if_token.data, Token::If);
        // intentionally omit mandatory parens for a Rust-like syntax
        let cond = self.expression(false)?;

        let then_branch = self.block()?;

        let else_branch = if let Some(_else_token) = self.matches(Token::Else) {
            Some(self.block()?)
        } else {
            None
        };

        Ok(Statement::IfElse {
            cond,
            then_branch,
            else_branch,
        }
        .spanned())
    }

    fn parameter_list(&mut self) -> ParseResult<Vec<Spanned<String>>> {
        let lparen_span = self.expect(Token::LParen, "(")?;

        let mut args = vec![];
        let ident = self.peek()?;
        if ident.data == Token::Ident {
            self.pop().unwrap();
            args.push(String::from(&self.source[ident.span]).with_span(ident.span));
            while self.peek()?.data == Token::Comma {
                self.pop().unwrap();
                let ident = self.expect(Token::Ident, "identifier")?;
                args.push(String::from(&self.source[ident]).with_span(ident));
            }
        }

        let rparen = self.pop()?;
        if rparen.data != Token::RParen {
            self.mismatched_pair(
                lparen_span,
                "This ( must be terminated",
                rparen.span,
                "Expected )",
            );
            return Err(ParseError::Handled);
        }

        if args.len() > u8::MAX as usize {
            self.simple_error(
                lparen_span.unite(rparen.span),
                "Cannot have more than 255 parameters",
            );
            return Err(ParseError::Handled);
        }
        Ok(args)
    }

    fn function_declaration(&mut self) -> ParseResult<Spanned<Statement>> {
        let fun_token = self.pop().unwrap();
        debug_assert_eq!(fun_token.data, Token::Fun);

        let name = self.expect(Token::Ident, "identifier")?;
        let args = self.parameter_list()?;
        let body = self.block()?;

        Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            name: String::from(&self.source[name]).with_span(name),
            args,
            body,
        })
        .spanned())
    }

    fn declaration(&mut self) -> ParseResult<Spanned<Statement>> {
        let token = self.peek()?;
        match token.data {
            Token::If => self.if_statement(),
            Token::Fun => self.function_declaration(),
            Token::Var => self.var_declaration(),
            Token::While => self.while_loop(),
            Token::For => self.for_loop(),
            Token::Return => {
                self.pop().unwrap();
                let value = if self.matches(Token::Semicolon).is_some() {
                    None
                } else {
                    let value = Some(self.expression(false)?);
                    self.check_semicolon(token.span)?;
                    value
                };
                Ok(Statement::Return {
                    span: token.span,
                    value,
                }
                .spanned())
            }
            Token::LBrace => {
                let block = self.block()?;
                Ok(Statement::Block(block).spanned())
            }
            Token::Print => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn top(mut self) -> ParseResult<Statements> {
        let mut res = vec![];
        while self.peek()?.data != Token::Eof {
            res.push(self.declaration()?);
        }
        Ok(Statements(res))
    }
}

pub fn parse(source: &str, output: impl Write) -> ParseResult<Statements> {
    let parser = Parser::new(source, output);
    parser.top()
}
