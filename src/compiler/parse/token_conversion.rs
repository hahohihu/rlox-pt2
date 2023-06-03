use super::lex::Token;

use super::BinaryKind;

impl TryFrom<Token> for BinaryKind {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Minus => Ok(BinaryKind::Minus),
            Token::Plus => Ok(BinaryKind::Plus),
            Token::Slash => Ok(BinaryKind::Divide),
            Token::Star => Ok(BinaryKind::Multiply),
            Token::BangEq => Ok(BinaryKind::NotEquals),
            Token::EqEq => Ok(BinaryKind::Equals),
            Token::Greater => Ok(BinaryKind::GreaterThan),
            Token::GreaterEq => Ok(BinaryKind::GreaterThanEqual),
            Token::Less => Ok(BinaryKind::LessThan),
            Token::LessEq => Ok(BinaryKind::LessThanEqual),
            Token::And => Ok(BinaryKind::And),
            Token::Or => Ok(BinaryKind::Or),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
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

impl From<BinaryKind> for Precedence {
    fn from(value: BinaryKind) -> Self {
        match value {
            BinaryKind::Minus => Self::Term,
            BinaryKind::Plus => Self::Term,
            BinaryKind::Divide => Self::Factor,
            BinaryKind::Multiply => Self::Factor,
            BinaryKind::NotEquals => Self::Equality,
            BinaryKind::Equals => Self::Equality,
            BinaryKind::GreaterThan => Self::Comparison,
            BinaryKind::GreaterThanEqual => Self::Comparison,
            BinaryKind::LessThan => Self::Comparison,
            BinaryKind::LessThanEqual => Self::Comparison,
            BinaryKind::And => Self::And,
            BinaryKind::Or => Self::Or,
        }
    }
}
