use std::iter::Peekable;

use crate::lex::Lexer;
use crate::lex::Token;
use crate::ui::*;

struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

macro_rules! expect {
    ($self:expr, $pat:pat => $ret:expr) => {
        (|| {
            let tok = $self.pop()?;
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
enum ParseError {
    UnexpectedEOF,
    InvalidToken(Span),
    ExpectError { expected: &'static str, got: Span },
}

pub type ParseRes<T> = Result<T, ParseError>;

impl<'src> Parser<'src> {
    fn pop(&mut self) -> ParseRes<Spanned<Token>> {
        let tok = self.lexer.next().ok_or(ParseError::UnexpectedEOF)?;
        tok.map_err(ParseError::InvalidToken)
    }

    fn peek(&mut self) -> ParseRes<Spanned<Token>> {
        let tok = *self.lexer.peek().ok_or(ParseError::UnexpectedEOF)?;
        tok.map_err(ParseError::InvalidToken)
    }
}
