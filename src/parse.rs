use std::iter::Peekable;

use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::lex::Lexer;
use crate::lex::Token;
use crate::ui::*;
use crate::value::Value;

struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

macro_rules! expect {
    ($self:expr, $msg:expr, $pat:pat => $ret:expr) => {
        (|| {
            let tok = $self.pop(stringify!($pat))?;
            match tok.data {
                $pat => Ok($ret),
                _ => return Err(ParseError::ExpectError {
                    expected: $msg,
                    got: tok.span
                }),
            }
        })()
    };
    ($self:expr, $msg:expr, $pat:pat) => {
        expect!($self, $msg, $pat => ())
    }
}

/// The same rules around emit_byte applies
macro_rules! emit_bytes {
    ($chunk:expr; $(($byte:expr, $span:expr)),+ $(,)?) => {{
        let span: Span = $span.into();
        $($chunk.emit_byte($byte, span);)+
    }};
    ($chunk:expr, $span:expr; $($byte:expr),+ $(,)?) => {{
        let span: Span = $span.into();
        $($chunk.emit_byte($byte, span);)+
    }}
}

impl Chunk {
    /// SAFETY: If an OpCode is emitted, it must have the specified number of follow bytes + follow other constraints
    unsafe fn emit_byte(&mut self, byte: impl Into<u8>, span: impl Into<Span>) {
        self.write_byte(byte, span.into());
    }

    fn emit_constant(&mut self, value: Value, span: impl Into<Span>) {
        let constant = self.add_constant(value);
        unsafe { emit_bytes!(self, span; OpCode::Constant, constant) }
    }

    fn emit_return(&mut self) {
        unsafe {
            self.emit_byte(OpCode::Return, 0..0);
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF { expected: &'static str },
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary
}

impl ParseError {
    pub fn print(&self, source: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        let offset = 12;
        let report = match self {
            Self::InvalidToken(span) => Report::build(ReportKind::Error, (), offset)
                .with_message("Lexing error")
                .with_label(
                    Label::new(*span)
                        .with_color(Color::Red)
                        .with_message("Invalid token"),
                ),
            Self::UnexpectedEOF { expected } => {
                let end = source.len().saturating_sub(1);
                Report::build(ReportKind::Error, (), offset)
                    .with_message("Unexpected end of file")
                    .with_label(
                        Label::new(Span::from(end..source.len()))
                            .with_color(Color::Green)
                            .with_message(format!("Expected {expected}")),
                    )
            }
            Self::ExpectError { expected, got } => Report::build(ReportKind::Error, (), offset)
                .with_message("Parse error")
                .with_label(
                    Label::new(*got)
                        .with_color(Color::Green)
                        .with_message(format!("Expected {expected}")),
                ),
        };
        report.finish().eprint(Source::from(source)).unwrap();
    }
}

pub type ParseRes<T> = Result<T, ParseError>;

impl<'src> Parser<'src> {
    fn new(source: &'src str) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self { lexer }
    }

    fn pop(&mut self, expected: &'static str) -> ParseRes<Spanned<Token>> {
        match self.lexer.next() {
            Some(Ok(t)) => Ok(t),
            Some(Err(t)) => Err(ParseError::InvalidToken(t)),
            None => Err(ParseError::UnexpectedEOF { expected }),
        }
    }

    fn peek(&mut self, expected: &'static str) -> ParseRes<Spanned<Token>> {
        match self.lexer.peek() {
            Some(Ok(t)) => Ok(*t),
            Some(Err(t)) => Err(ParseError::InvalidToken(*t)),
            None => Err(ParseError::UnexpectedEOF { expected }),
        }
    }

    fn grouping(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        self.expression(chunk, Precedence::None)?;
        expect!(self, ") after expression", Token::RParen)
    }

    fn unary(&mut self, chunk: &mut Chunk, op: Spanned<Token>) -> Result<(), ParseError> {
        self.primary(chunk)?;
        match op.data {
            Token::Minus => unsafe { chunk.emit_byte(OpCode::Sub, op.span) },
            _ => todo!(),
        }
        Ok(())
    }

    fn primary(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        let token = self.pop("primary")?;
        match token.data {
            // Token::Minus => self.unary(chunk, token)?,
            _ => todo!()
        }
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk, min: Precedence) -> Result<(), ParseError> {
        let tok = self.pop("expression")?;
        match tok.data {
            Token::Num(n) => chunk.emit_constant(n, tok.span),
            _ => todo!(),
        }
        Ok(())
    }
}

pub fn compile(source: &str) -> Result<Chunk, ParseError> {
    let mut chunk = Chunk::new();
    let mut parser = Parser::new(source);
    parser.expression(&mut chunk, Precedence::None)?;
    chunk.emit_return();
    Ok(chunk)
}
