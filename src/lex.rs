use crate::ui::{Span, Spanned};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \n\r\f]+")]
pub enum Token<'src> {
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
    Ident(&'src str),
    #[regex(r#""[^"]*""#, |lex| {
        let string = lex.slice();
        &string[1..string.len() - 1] // trim "
    })]
    String(&'src str),
    // leading digit is mandatory, possibly incongruent with book
    #[regex("[0-9]+(.[0-9]+)", |lex| lex.slice().parse().ok())]
    Num(f64),
    #[regex("true|false", |lex| lex.slice() == "true")]
    Bool(bool),

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
}

pub struct Lexer<'src>(logos::SpannedIter<'src, Token<'src>>);

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self(Token::lexer(src).spanned())
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Spanned<Token<'src>>, Span>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(tok, span)| {
            tok.map(|t| Spanned::new(t, span)) // rustfmt guard
                .map_err(|_| span.clone())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    use Token::*;

    fn lex(src: &str) -> Result<Vec<Token>, logos::Span> {
        Lexer::new(src)
            .map(|t| t.map(|t| t.data))
            .collect::<Result<Vec<_>, _>>()
    }

    fn lex_ok(src: &str) -> Vec<Token> {
        match lex(src) {
            Ok(t) => t,
            Err(s) => panic!("Mismatch at {:?} on {}", s.clone(), &src[s]),
        }
    }

    #[test]
    fn bools() {
        assert_eq!(lex_ok("true false"), &[Bool(true), Bool(false)]);
    }

    #[test]
    fn comparisons() {
        assert_eq!(lex_ok("< <= == ="), &[Less, LessEq, EqEq, Eq]);
    }

    #[test]
    fn multiline_strings() {
        assert_eq!(lex_ok("\"1\n2\n3\n\""), &[String("1\n2\n3\n")])
    }
}
