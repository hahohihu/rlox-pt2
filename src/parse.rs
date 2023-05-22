use std::io::Write;
use std::iter::Peekable;

#[cfg(not(feature = "verbose_parsing"))]
use crate::noop as trace;
#[cfg(feature = "verbose_parsing")]
use tracing::trace;

use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::lex::Lexer;
use crate::lex::Token;

use crate::ui;
use crate::ui::*;
use crate::value::Value;

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

pub type ParseRes<T> = Result<T, ParseError>;

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

    fn pop(&mut self) -> ParseRes<Spanned<Token>> {
        match self.lexer.next() {
            Some(Ok(t)) => {
                trace!("popping '{}'", &self.source[t.span]);
                Ok(t)
            }
            Some(Err(t)) => Err(ParseError::InvalidToken(t)),
            None => Ok(self.eof()),
        }
    }

    fn peek(&mut self) -> ParseRes<Spanned<Token>> {
        match self.lexer.peek() {
            Some(Ok(t)) => Ok(*t),
            Some(Err(t)) => Err(ParseError::InvalidToken(*t)),
            None => Ok(self.eof()),
        }
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
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
                self.expression(chunk, Precedence::Start)?;
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
                chunk.emit_constant(Value::from(str), token.span)
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
    fn expression(&mut self, chunk: &mut Chunk, min: Precedence) -> Result<(), ParseError> {
        self.primary(chunk)?;
        loop {
            let operation = self.peek()?;

            let prec = Precedence::from(operation.data); // todo: everything left associative
            if prec < min {
                break;
            }

            self.pop().unwrap();
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
        let token = self.pop()?;
        let opcode = match token.data {
            Token::Print => OpCode::Print,
            Token::Return => OpCode::Return,
            _ => {
                return Err(ParseError::ExpectError {
                    expected: "statement",
                    got: token.span,
                })
            }
        };
        self.expression(chunk, Precedence::Start)?;
        let next = self.pop()?;
        if next.data != Token::Semicolon {
            self.mismatched_pair(
                token.span,
                "This statement should be terminated with ;",
                next.span,
                "There should be a ; here",
            );
            return Err(ParseError::Handled);
        }
        unsafe {
            chunk.emit_byte(opcode, token.span);
        }
        Ok(())
    }

    fn declaration(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        self.statement(chunk)
    }

    fn top(&mut self, chunk: &mut Chunk) -> Result<(), ParseError> {
        while self.peek()?.data != Token::Eof {
            self.declaration(chunk)?;
        }
        Ok(())
    }
}

pub fn compile(source: &str, output: impl Write) -> Result<Chunk, ParseError> {
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
    use std::error::Error;
    use std::result::Result;

    macro_rules! snap_err {
        ($name:ident, $input:literal) => {
            #[test]
            fn $name() -> Result<(), Box<dyn Error>> {
                let mut stderr = Vec::new();
                let _ = crate::interpret($input, &mut stderr);
                let stripped = strip_ansi_escapes::strip(stderr)?;
                let stderr = String::from_utf8(stripped)?;
                ::insta::assert_display_snapshot!(stderr);
                Ok(())
            }
        };
    }

    snap_err!(missing_op, "print 1 1;");
    snap_err!(missing_primary, "print ();\n");
    snap_err!(missing_parens, "print ((1);\n");
    snap_err!(rparens, "print 1);\n");
    snap_err!(missing_rhs, "print 1 + ;\n");
    snap_err!(missing_lhs, "print + 1;\n");
    snap_err!(invalid_token, "print $;");
    snap_err!(remaining_tokens, "print 1; x");
    snap_err!(floating_expr, "1;");
}
