use std::iter::Peekable;

use crate::chunk::Chunk;
use crate::lex::Lexer;
use crate::lex::Token;
use crate::ui::*;

struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

macro_rules! expect {
    ($self:expr, $pat:pat => $ret:expr) => {
        (|| {
            let tok = $self.pop(stringify!($pat))?;
            match tok.data {
                $pat => Ok($ret),
                _ => return Err(ParseError::ExpectError {
                    expected: stringify!($pat),
                    got: tok.span
                }),
            }
        })()
    };
    ($self:expr, $pat:pat) => {
        expect!($self, $pat => ())
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF { expected: &'static str },
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
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
    fn pop(&mut self, expected: &'static str) -> ParseRes<Spanned<Token>> {
        let tok = self
            .lexer
            .next()
            .ok_or(ParseError::UnexpectedEOF { expected })?;
        tok.map_err(ParseError::InvalidToken)
    }

    fn peek(&mut self, expected: &'static str) -> ParseRes<Spanned<Token>> {
        let tok = *self
            .lexer
            .peek()
            .ok_or(ParseError::UnexpectedEOF { expected })?;
        tok.map_err(ParseError::InvalidToken)
    }
}

pub fn compile(source: &str) -> Result<Chunk, ParseError> {
    let chunk = Chunk::new();
    Ok(chunk)
}
