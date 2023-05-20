use std::iter::Peekable;

#[cfg(feature = "verbose_parsing")]
use tracing::trace;
#[cfg(not(feature = "verbose_parsing"))]
use crate::noop as trace;

use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::lex::Lexer;
use crate::lex::Token;

use crate::ui;
use crate::ui::*;
use crate::value::Value;

struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
    source: &'src str,
}

macro_rules! expect {
    ($self:expr, $msg:expr, $pat:pat => $ret:expr) => {{
        let tok = $self.pop(stringify!($pat))?;
        match tok.data {
            $pat => Ok(Spanned::new($ret, tok.span)),
            _ => return Err(ParseError::ExpectError {
                expected: $msg,
                got: tok.span
            }),
        }
    }};
    ($self:expr, $msg:expr, $pat:pat) => {
        expect!($self, $msg, $pat => ())
    }
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

    fn emit_constant(&mut self, value: Value, span: impl Into<Span>) {
        let constant = self.add_constant(value);
        // SAFETY: constant
        unsafe { emit_bytes!(self, span; OpCode::Constant, constant) }
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
    EOF { expected: &'static str },
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
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
    pub fn print(&self, source: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        let report = match self {
            Self::InvalidToken(span) => Report::build(ReportKind::Error, (), ui::OFFSET)
                .with_message("Lexing error")
                .with_label(
                    Label::new(*span)
                        .with_color(Color::Red)
                        .with_message("Invalid token"),
                ),
            Self::EOF { expected } => {
                let end = source.len().saturating_sub(1);
                Report::build(ReportKind::Error, (), ui::OFFSET)
                    .with_message("Unexpected end of file")
                    .with_label(
                        Label::new(Span::from(end..source.len()))
                            .with_color(Color::Green)
                            .with_message(format!("Expected {expected}")),
                    )
            }
            Self::ExpectError { expected, got } => Report::build(ReportKind::Error, (), ui::OFFSET)
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
        Self { lexer, source }
    }

    fn pop(&mut self, expected: &'static str) -> ParseRes<Spanned<Token>> {
        match self.lexer.next() {
            Some(Ok(t)) => {
                trace!("popping '{}'", &self.source[t.span]);
                Ok(t)
            }
            Some(Err(t)) => Err(ParseError::InvalidToken(t)),
            None => Err(ParseError::EOF { expected }),
        }
    }

    fn peek(&mut self) -> Option<ParseRes<Spanned<Token>>> {
        let tok = self.lexer.peek()?;
        Some(match tok {
            Ok(t) => Ok(*t),
            Err(t) => Err(ParseError::InvalidToken(*t)),
        })
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        let token = self.pop("primary")?;
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
                self.expression(chunk, Precedence::Start)?;
                expect!(self, ") after expression", Token::RParen)?;
            }
            Token::Num => {
                let n = self.source[token.span].parse::<f64>().unwrap();
                chunk.emit_constant(Value::Num(n), token.span)
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
                chunk.emit_constant(
                    Value::from(str),
                    token.span,
                )
            }
            _ => todo!(),
        }
        Ok(())
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn expression(&mut self, chunk: &mut Chunk, min: Precedence) -> Result<(), ParseError> {
        self.primary(chunk)?;
        loop {
            let Some(token) = self.peek() else {
                break;
            };
            let operation = token?;

            let prec = Precedence::from(operation.data); // todo: everything left associative
            if prec < min {
                break;
            }

            self.pop("unreachable").unwrap();
            self.expression(chunk, prec)?;

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

    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn statement(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        let tok = self.pop("statement")?;
        let opcode = match tok.data {
            Token::Print => OpCode::Print,
            Token::Return => OpCode::Return,
            _ => return Err(ParseError::ExpectError {
                expected: "statement",
                got: tok.span
            })
        };
        self.expression(chunk, Precedence::Start)?;
        unsafe {
            chunk.emit_byte(opcode, tok.span);
        }
        Ok(())
    }
}

pub fn compile(source: &str) -> Result<Chunk, ParseError> {
    let mut chunk = Chunk::new();
    let mut parser = Parser::new(source);
    parser.statement(&mut chunk)?;
    if let Some(t) = parser.peek() {
        let t = t?;
        return Err(ParseError::ExpectError {
            expected: "EOF",
            got: t.span,
        });
    }
    chunk.emit_return();
    Ok(chunk)
}
