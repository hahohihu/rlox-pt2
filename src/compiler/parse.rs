use std::io::Write;
use std::iter::Peekable;

#[cfg(not(feature = "verbose_parsing"))]
use crate::noop as trace;

#[cfg(feature = "verbose_parsing")]
use tracing::trace;

use super::lex::Lexer;
use super::lex::Token;
use crate::repr::chunk::Chunk;
use crate::repr::chunk::OpCode;
use crate::repr::value::Value;

use crate::common::ui;
use crate::common::ui::*;

struct Parser<'src, StdErr: Write> {
    lexer: Peekable<Lexer<'src>>,
    source: &'src str,
    stderr: StdErr,
}

/// The same rules around emit_byte applies
macro_rules! emit_bytes {
    ($chunk:expr, $span:expr; $($byte:expr),+ $(,)?) => {{
        let span: Span = $span.into();
        $($chunk.emit_byte($byte, span);)+
    }}
}

impl Chunk {
    /// PRECONDITION: If an OpCode is emitted, it must have the specified number of follow bytes + follow other constraints
    unsafe fn emit_byte(&mut self, byte: impl Into<u8>, span: impl Into<Span>) {
        self.write_byte(byte, span.into());
    }

    fn emit_constant(&mut self, value: Value, span: impl Into<Span>) -> u8 {
        let constant = self.add_constant(value);
        // SAFETY: constant
        unsafe {
            emit_bytes!(self, span; OpCode::Constant, constant);
        }
        constant
    }

    fn emit_return(&mut self) {
        unsafe {
            // SAFETY: constant
            self.emit_byte(OpCode::Return, 0..0);
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
    Handled,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Start,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl From<Token> for Precedence {
    fn from(value: Token) -> Self {
        match value {
            Token::Minus => Self::Term,
            Token::Plus => Self::Term,
            Token::Slash => Self::Factor,
            Token::Star => Self::Factor,
            Token::EqEq => Self::Equality,
            Token::Greater => Self::Comparison,
            Token::GreaterEq => Self::Comparison,
            Token::Less => Self::Comparison,
            Token::LessEq => Self::Comparison,
            _ => Self::None,
        }
    }
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
                trace!("popping '{}'", &self.source[t.span]);
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

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let token = self.pop()?;
        match token.data {
            Token::Minus => {
                self.primary(chunk)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                unsafe { chunk.emit_byte(OpCode::Negate, token.span) }
            }
            Token::Bang => {
                self.primary(chunk)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                unsafe { chunk.emit_byte(OpCode::Not, token.span) }
            }
            Token::LParen => {
                self.expression(chunk)?;
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
            }
            Token::Num => {
                let n = self.source[token.span].parse::<f64>().unwrap();
                chunk.emit_constant(Value::Num(n), token.span);
            }
            Token::True => unsafe {
                // SAFETY: constant
                chunk.emit_byte(OpCode::True, token.span);
            },
            Token::False => unsafe {
                // SAFETY: constant
                chunk.emit_byte(OpCode::False, token.span);
            },
            Token::Nil => unsafe {
                // SAFETY: constant
                chunk.emit_byte(OpCode::Nil, token.span);
            },
            Token::String => {
                // remove parens
                let str = &self.source[token.span][1..];
                let str = &str[..str.len() - 1];
                chunk.emit_constant(Value::from(str), token.span);
            }
            Token::Ident => {
                let name = &self.source[token.span];
                let nameid = chunk.add_literal(name);
                unsafe {
                    emit_bytes!(chunk, token.span; OpCode::GetGlobal, nameid);
                }
            }
            _ => {
                return Err(ParseError::ExpectError {
                    expected: "primary",
                    got: token.span,
                })
            }
        }
        Ok(())
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn expression_bp(&mut self, chunk: &mut Chunk, min: Precedence) -> ParseResult<()> {
        self.primary(chunk)?;
        loop {
            let operation = self.peek()?;

            let prec = Precedence::from(operation.data); // todo: everything's currently left associative
            if prec <= min {
                break;
            }

            self.pop().unwrap();
            self.expression_bp(chunk, prec)?;

            // SAFETY: There must be 2 prior values on the stack. This is guaranteed by primary + expression
            unsafe {
                macro_rules! emit {
                    ($($opcode:expr),+) => {
                        emit_bytes!(chunk, operation.span; $($opcode,)+)
                    };
                }
                match operation.data {
                    Token::Minus => emit!(OpCode::Sub),
                    Token::Plus => emit!(OpCode::Add),
                    Token::Slash => emit!(OpCode::Div),
                    Token::Star => emit!(OpCode::Mul),
                    Token::EqEq => emit!(OpCode::Equal),
                    Token::BangEq => emit!(OpCode::Equal, OpCode::Not),
                    Token::Greater => emit!(OpCode::Greater),
                    Token::GreaterEq => emit!(OpCode::Less, OpCode::Not),
                    Token::Less => emit!(OpCode::Less),
                    Token::LessEq => emit!(OpCode::Greater, OpCode::Not),
                    _ => unreachable!("Precedence::from must handle this"),
                }
            }
        }
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        self.expression_bp(chunk, Precedence::Start)
    }

    fn check_semicolon(&mut self, lhs: Span) -> ParseResult<()> {
        let next = self.pop()?;
        if next.data != Token::Semicolon {
            self.mismatched_pair(
                lhs,
                "This should be terminated with ;",
                next.span,
                "There should be a ; here",
            );
            return Err(ParseError::Handled);
        }
        Ok(())
    }

    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn statement(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let token = self.peek()?;
        let opcode = match token.data {
            Token::Print => {
                self.pop().unwrap();
                OpCode::Print
            }
            _ => OpCode::Pop,
        };
        self.expression(chunk)?;
        self.check_semicolon(token.span)?;
        unsafe {
            chunk.emit_byte(opcode, token.span);
        }
        Ok(())
    }

    fn expect_identifier(&mut self) -> ParseResult<Spanned<&str>> {
        let token = self.pop()?;
        if token.data == Token::Ident {
            Ok(Spanned {
                data: &self.source[token.span],
                span: token.span,
            })
        } else {
            Err(ParseError::ExpectError {
                expected: "identifier",
                got: token.span,
            })
        }
    }

    fn pops(&mut self, maybe: Token) -> bool {
        let Ok(token) = self.peek() else {
            return false;
        };
        if token.data == maybe {
            self.pop().unwrap();
            true
        } else {
            false
        }
    }

    fn var_declaration(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let var = self.pop().unwrap();
        debug_assert_eq!(var.data, Token::Var);

        let name = self.expect_identifier()?;
        let namespan = name.span;
        let nameid = chunk.add_literal(name.data);

        if self.pops(Token::Eq) {
            self.expression(chunk)?;
        } else {
            chunk.emit_constant(Value::Nil, namespan);
        }

        self.check_semicolon(var.span)?;

        unsafe {
            emit_bytes!(chunk, namespan; OpCode::DefineGlobal, nameid);
        }
        Ok(())
    }

    fn declaration(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let token = self.peek()?;
        match token.data {
            Token::Var => self.var_declaration(chunk),
            _ => self.statement(chunk),
        }
    }

    fn top(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        while self.peek()?.data != Token::Eof {
            self.declaration(chunk)?;
        }
        Ok(())
    }
}

pub fn compile(source: &str, output: impl Write) -> ParseResult<Chunk> {
    let mut chunk = Chunk::new();
    let mut parser = Parser::new(source, output);
    parser.top(&mut chunk)?;
    let next = parser.peek()?;
    if next.data != Token::Eof {
        return Err(ParseError::ExpectError {
            expected: "EOF",
            got: next.span,
        });
    }
    chunk.emit_return();
    Ok(chunk)
}

#[cfg(test)]
mod tests {
    use std::io::stderr;
    fn snap_bytecode(source: &str) {
        crate::common::util::setup_test();
        let chunk = super::compile(source, stderr()).unwrap();
        let mut out = vec![];
        chunk.disassemble("test", source, &mut out);
        let res = String::from_utf8(out).unwrap();
        insta::assert_snapshot!(res);
    }

    macro_rules! bytecode {
        ($name:ident, $input:literal) => {
            #[test]
            fn $name() {
                snap_bytecode($input);
            }
        };
    }

    bytecode!(order, "print 1 / 2 * 2;");
}
