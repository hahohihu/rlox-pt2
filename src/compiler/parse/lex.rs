use crate::common::ui::{Span, Spanned};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \n\r\f]+")]
pub enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,

    #[token("!")]
    Bang,
    #[token("!=")]
    BangEq,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,

    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,
    #[regex(r#""[^"]*""#)]
    String,
    // leading digit is mandatory, possibly incongruent with book
    #[regex("[0-9]+(\\.[0-9]+)?")]
    Num,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("and")]
    And,
    #[token("class")]
    Class,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("fun")]
    Fun,
    #[token("if")]
    If,
    #[token("nil")]
    Nil,
    #[token("or")]
    Or,
    #[token("print")]
    Print,
    #[token("return")]
    Return,
    #[token("super")]
    Super,
    #[token("this")]
    This,
    #[token("var")]
    Var,
    #[token("while")]
    While,
    Eof,
}

pub struct Lexer<'src>(logos::SpannedIter<'src, Token>);

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self(Token::lexer(src).spanned())
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Spanned<Token>, Span>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(tok, span)| {
            let span = span.into();
            tok.map(|t| Spanned::new(t, span)) // rustfmt guard
                .map_err(|_| span)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    use crate::common::ui::Span;
    use Token::*;

    fn lex(src: &str) -> Result<Vec<Token>, Span> {
        Lexer::new(src)
            .map(|t| t.map(|t| t.data))
            .collect::<Result<Vec<_>, _>>()
    }

    fn lex_ok(src: &str) -> Vec<Token> {
        match lex(src) {
            Ok(t) => t,
            Err(s) => panic!("Mismatch at {:?} on {}", s, &src[s]),
        }
    }

    #[test]
    fn bools() {
        assert_eq!(lex_ok("true false"), &[True, False]);
    }

    #[test]
    fn comparisons() {
        assert_eq!(lex_ok("< <= == ="), &[Less, LessEq, EqEq, Eq]);
    }

    #[test]
    fn multiline_strings() {
        assert_eq!(lex_ok("\"1\n2\n3\n\""), &[String])
    }

    #[test]
    fn sequence_of_numbers() {
        assert_eq!(lex_ok("1 1"), &[Num, Num]);
    }
}
