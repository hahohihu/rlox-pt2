use std::io::Write;
use std::iter::Peekable;

#[cfg(not(feature = "verbose_parsing"))]
use crate::noop as trace;
use crate::repr::interner::InternedU8;
use crate::repr::interner::Interner;

#[cfg(feature = "verbose_parsing")]
use tracing::trace;

use super::lex::Lexer;
use super::lex::Token;
use crate::repr::chunk::Chunk;
use crate::repr::chunk::OpCode;
use crate::repr::value::Value;

use crate::common::ui;
use crate::common::ui::*;

type LocalSymbol = InternedU8;
struct Parser<'src, StdErr: Write> {
    lexer: Peekable<Lexer<'src>>,
    source: &'src str,
    stderr: StdErr,
    interned_locals: Interner,
    defined_locals: Vec<LocalSymbol>,
    scope_size: Vec<LocalSymbol>,
}

/// The same rules around emit_byte applies
macro_rules! emit_bytes {
    ($chunk:expr, $span:expr; $($byte:expr),+ $(,)?) => {{
        let span: Span = $span.into();
        $($chunk.emit_byte($byte, span);)+
    }}
}

impl Chunk {
    unsafe fn emit_continuation_byte(&mut self, byte: impl Into<u8>) {
        let prev = self.spans.last().copied().unwrap_or(Span::from(0..0));
        self.write_byte(byte, prev);
    }

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
            Token::BangEq => Self::Equality,
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

macro_rules! scoped {
    ($self:expr, $chunk:expr, $body:expr) => {{
        $self.begin_scope();
        #[allow(clippy::redundant_closure_call)]
        let res = (|| $body)();
        $self.end_scope($chunk);
        res
    }};
}

pub type ParseResult<T> = Result<T, ParseError>;

impl<'src, StdErr: Write> Parser<'src, StdErr> {
    fn new(source: &'src str, stderr: StdErr) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            lexer,
            source,
            stderr,
            interned_locals: Interner::default(),
            defined_locals: Default::default(),
            scope_size: Default::default(),
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

    fn expect_identifier(&mut self) -> ParseResult<Span> {
        let token = self.pop()?;
        if token.data == Token::Ident {
            Ok(token.span)
        } else {
            Err(ParseError::ExpectError {
                expected: "identifier",
                got: token.span,
            })
        }
    }

    fn in_global_scope(&self) -> bool {
        self.scope_size.is_empty()
    }

    fn begin_scope(&mut self) {
        self.scope_size.push(0);
    }

    fn end_scope(&mut self, chunk: &mut Chunk) {
        let size = self.scope_size.pop().unwrap();
        for _ in 0..size {
            unsafe {
                chunk.emit_byte(OpCode::Pop, 0..0);
            }
            self.defined_locals.pop().unwrap();
        }
    }

    fn add_local(&mut self, name: &str) {
        let nameid = self.interned_locals.add_or_get(name);
        self.defined_locals.push(nameid);
        let size = self.scope_size.last_mut().unwrap();
        *size += 1;
    }

    fn resolve_local(&self, name: &str) -> Option<LocalSymbol> {
        let nameid = self.interned_locals.get(name)?;
        for (i, local) in self.defined_locals.iter().enumerate().rev() {
            if nameid == *local {
                return Some(i.try_into().unwrap());
            }
        }
        None
    }

    fn variable_access_or_assignment(
        &mut self,
        chunk: &mut Chunk,
        ident_span: Span,
        can_assign: bool,
    ) -> ParseResult<()> {
        let name = &self.source[ident_span];
        let follow_byte: u8;
        let set_opcode: OpCode;
        let get_opcode: OpCode;
        if let Some(position) = self.resolve_local(name) {
            follow_byte = position;
            set_opcode = OpCode::SetLocal;
            get_opcode = OpCode::GetLocal;
        } else {
            follow_byte = chunk.globals.add_or_get(name);
            set_opcode = OpCode::SetGlobal;
            get_opcode = OpCode::GetGlobal;
        }
        if let Some(eq) = self.matches(Token::Eq) {
            if !can_assign {
                self.simple_error(eq.span, "Invalid assignment at this expression depth");
                return Err(ParseError::Handled);
            }
            self.expression(chunk, can_assign)?;
            unsafe {
                emit_bytes!(chunk, ident_span; set_opcode, follow_byte);
            }
        } else {
            unsafe {
                emit_bytes!(chunk, ident_span; get_opcode, follow_byte);
            }
        }
        Ok(())
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, chunk: &mut Chunk, can_assign: bool) -> ParseResult<()> {
        let token = self.pop()?;
        match token.data {
            Token::Minus => {
                self.primary(chunk, can_assign)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                unsafe { chunk.emit_byte(OpCode::Negate, token.span) }
            }
            Token::Bang => {
                self.primary(chunk, can_assign)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                unsafe { chunk.emit_byte(OpCode::Not, token.span) }
            }
            Token::LParen => {
                self.expression(chunk, can_assign)?;
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
                let str = &self.source[token.span];
                let str = &str[1..str.len() - 1];
                chunk.emit_constant(Value::from(str), token.span);
            }
            Token::Ident => {
                self.variable_access_or_assignment(chunk, token.span, can_assign)?;
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
    fn expression_bp(
        &mut self,
        chunk: &mut Chunk,
        min: Precedence,
        mut can_assign: bool,
    ) -> ParseResult<()> {
        self.primary(chunk, can_assign)?;
        loop {
            let operation = self.peek()?;

            let prec = Precedence::from(operation.data); // todo: everything's currently left associative
            if prec <= min {
                break;
            }

            can_assign = prec <= Precedence::Assignment;

            self.pop().unwrap();
            self.expression_bp(chunk, prec, can_assign)?;

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

    fn expression(&mut self, chunk: &mut Chunk, can_assign: bool) -> ParseResult<()> {
        self.expression_bp(chunk, Precedence::Start, can_assign)
    }

    fn block(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let lbrace = self.pop().unwrap();
        if lbrace.data != Token::LBrace {
            return Err(ParseError::ExpectError {
                expected: "{",
                got: lbrace.span,
            });
        }
        loop {
            let token = self.peek()?;
            if let Token::RBrace | Token::Eof = token.data {
                break;
            }
            self.declaration(chunk)?;
        }

        let rbrace = self.pop()?;
        if rbrace.data == Token::RBrace {
            Ok(())
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

    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn statement(&mut self, chunk: &mut Chunk, can_assign: bool) -> ParseResult<()> {
        let token = self.peek()?;
        let opcode = match token.data {
            Token::Print => {
                self.pop().unwrap();
                OpCode::Print
            }
            _ => OpCode::Pop,
        };
        self.expression(chunk, can_assign)?;
        self.check_semicolon(token.span)?;
        unsafe {
            chunk.emit_byte(opcode, token.span);
        }
        Ok(())
    }

    fn var_declaration(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let var = self.pop().unwrap();
        debug_assert_eq!(var.data, Token::Var);

        let namespan = self.expect_identifier()?;

        if self.matches(Token::Eq).is_some() {
            self.expression(chunk, true)?;
        } else {
            chunk.emit_constant(Value::Nil, namespan);
        }

        self.check_semicolon(var.span)?;

        if self.in_global_scope() {
            let nameid = chunk.globals.add_or_get(&self.source[namespan]);
            unsafe {
                emit_bytes!(chunk, namespan; OpCode::DefineGlobal, nameid);
            }
        } else {
            self.add_local(&self.source[namespan]);
        }
        Ok(())
    }

    fn if_statement(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let if_token = self.pop().unwrap();
        debug_assert_eq!(if_token.data, Token::If);

        // intentionally omit mandatory parens for a Rust-like syntax
        self.expression(chunk, true)?;

        unsafe {
            emit_bytes!(chunk, if_token.span; OpCode::JumpRelIfFalse, 0xff, 0xff);
        }
        let addr_index = chunk.instructions.len() - 2;

        scoped!(self, chunk, self.block(chunk))?;

        let Ok(jump) = u16::try_from(chunk.instructions.len() - addr_index - 2) else {
            self.simple_error(if_token.span, "The body of this if statement is too long and would generate more instructions than is supported.");
            return Err(ParseError::Handled);
        };

        let jump = jump.to_le_bytes();
        chunk.instructions[addr_index..][..2].copy_from_slice(&jump);
        Ok(())
    }

    fn declaration(&mut self, chunk: &mut Chunk) -> ParseResult<()> {
        let token = self.peek()?;
        match token.data {
            Token::If => self.if_statement(chunk),
            Token::Var => self.var_declaration(chunk),
            Token::LBrace => {
                scoped!(self, chunk, self.block(chunk))
            }
            _ => self.statement(chunk, true),
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
    fn snap_bytecode(source: &str) -> String {
        crate::common::util::setup_test();
        let chunk = super::compile(source, stderr()).unwrap();
        let mut out = vec![];
        chunk.disassemble("test", source, &mut out);
        String::from_utf8(out).unwrap()
    }

    macro_rules! bytecode {
        ($name:ident, $input:literal) => {
            #[test]
            fn $name() {
                $crate::common::util::assert_snapshot!(snap_bytecode($input));
            }
        };
    }

    bytecode!(order, "print 1 / 2 * 2;");
}
