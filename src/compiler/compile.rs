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
    chunk: Chunk,
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
    /// This is a byte which has no corresponding span and is just an implementation detail
    fn emit_impl_byte(&mut self, byte: impl Into<u8>) {
        self.write_byte(byte, Chunk::impl_span());
    }

    /// This is a span intended for bytes that are only implementation details
    fn impl_span() -> Span {
        Span::from(0..0)
    }

    /// PRECONDITION: If an OpCode is emitted, it must have the specified number of follow bytes + follow other constraints
    fn emit_byte(&mut self, byte: impl Into<u8>, span: impl Into<Span>) {
        self.write_byte(byte, span.into());
    }

    fn emit_constant(&mut self, value: Value, span: impl Into<Span>) -> u8 {
        let constant = self.add_constant(value);
        // SAFETY: constant
        emit_bytes!(self, span; OpCode::Constant, constant);
        constant
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return, Chunk::impl_span());
    }

    fn emit_jump(&mut self, jump: OpCode, span: Span) -> usize {
        emit_bytes!(self, span; jump, 0xff, 0xff);
        self.instructions.len() - 2
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
            Token::And => Self::And,
            Token::Or => Self::Or,
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
            chunk: Chunk::new(),
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

    fn end_scope(&mut self) {
        let size = self.scope_size.pop().unwrap();
        for _ in 0..size {
            self.chunk.emit_impl_byte(OpCode::Pop);
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
            follow_byte = self.chunk.globals.add_or_get(name);
            set_opcode = OpCode::SetGlobal;
            get_opcode = OpCode::GetGlobal;
        }
        if let Some(eq) = self.matches(Token::Eq) {
            if !can_assign {
                self.simple_error(eq.span, "Invalid assignment at this expression depth");
                return Err(ParseError::Handled);
            }
            self.expression(can_assign)?;
            emit_bytes!(self.chunk, ident_span; set_opcode, follow_byte);
        } else {
            emit_bytes!(self.chunk, ident_span; get_opcode, follow_byte);
        }
        Ok(())
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn primary(&mut self, can_assign: bool) -> ParseResult<()> {
        let token = self.pop()?;
        match token.data {
            Token::Minus => {
                self.primary(can_assign)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                self.chunk.emit_byte(OpCode::Negate, token.span)
            }
            Token::Bang => {
                self.primary(can_assign)?;
                // SAFETY: a value must be on the stack for negate to work, which primary guarantees
                self.chunk.emit_byte(OpCode::Not, token.span)
            }
            Token::LParen => {
                self.expression(true)?;
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
                self.chunk.emit_constant(Value::Num(n), token.span);
            }
            Token::True => {
                // SAFETY: constant
                self.chunk.emit_byte(OpCode::True, token.span);
            }
            Token::False => {
                // SAFETY: constant
                self.chunk.emit_byte(OpCode::False, token.span);
            }
            Token::Nil => {
                // SAFETY: constant
                self.chunk.emit_byte(OpCode::Nil, token.span);
            }
            Token::String => {
                // remove parens
                let str = &self.source[token.span];
                let str = &str[1..str.len() - 1];
                self.chunk.emit_constant(Value::from(str), token.span);
            }
            Token::Ident => {
                self.variable_access_or_assignment(token.span, can_assign)?;
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

    fn and_expr(&mut self) -> ParseResult<()> {
        let and = self.pop()?;
        debug_assert_eq!(and.data, Token::And);

        let end = self.chunk.emit_jump(OpCode::JumpRelIfFalse, and.span);
        self.chunk.emit_impl_byte(OpCode::Pop);

        self.expression_bp(Precedence::from(and.data), false)?;
        self.patch_jump(end, and.span)?;

        Ok(())
    }

    fn or_expr(&mut self) -> ParseResult<()> {
        let or = self.pop()?;
        debug_assert_eq!(or.data, Token::Or);

        let end = self.chunk.emit_jump(OpCode::JumpRelIfTrue, or.span);
        self.chunk.emit_impl_byte(OpCode::Pop);

        self.expression_bp(Precedence::from(or.data), false)?;
        self.patch_jump(end, or.span)?;

        Ok(())
    }

    fn emit_loop(&mut self, span: Span, start: usize) -> ParseResult<()> {
        self.chunk.emit_byte(OpCode::Loop, span);
        let offset = self.chunk.instructions.len() - start + 2;
        let Ok(offset) = u16::try_from(offset) else {
            self.simple_error(span, "This loop would have a longer body than is supported");
            return Err(ParseError::Handled);
        };
        let offset = offset.to_ne_bytes();
        emit_bytes!(self.chunk, span; offset[0], offset[1]);
        Ok(())
    }

    fn while_loop(&mut self) -> ParseResult<()> {
        let while_token = self.pop().unwrap();
        debug_assert_eq!(while_token.data, Token::While);

        let start = self.chunk.instructions.len();
        self.expression(false)?;

        let exit = self.chunk.emit_jump(OpCode::JumpRelIfFalse, while_token.span);
        self.chunk.emit_impl_byte(OpCode::Pop);

        self.scoped_block()?;
        self.emit_loop(while_token.span, start)?;

        self.patch_jump(exit, while_token.span)?;
        self.chunk.emit_impl_byte(OpCode::Pop);
        Ok(())
    }

    fn for_loop(&mut self) -> ParseResult<()> {
        self.begin_scope();
        // A poor excuse for a try block
        let res = (|| {
            let for_token = self.pop().unwrap();
            debug_assert_eq!(for_token.data, Token::For);
    
            // initializer
            let peeked = self.peek()?;
            match peeked.data {
                Token::Semicolon => {
                    self.pop()?;
                }
                Token::Var => self.var_declaration()?,
                _ => self.expression_statement()?,
            }
    
            // condition
            let mut start = self.chunk.instructions.len();
            let mut exit = None;
            if self.matches(Token::Semicolon).is_none() {
                let peeked = self.peek()?;
                self.expression(true)?;
                self.check_semicolon(peeked.span)?;
                exit = Some(self.chunk.emit_jump(OpCode::JumpRelIfFalse, Chunk::impl_span()));
                self.chunk.emit_impl_byte(OpCode::Pop);
            }

            // ;_; should've ignored the book and made an AST
            // Because we emit bytecode immediately, we can't go back to this later and put it at the end of the body
            // So, we need to do some hacky jumps to make it work
            if self.peek()?.data != Token::LBrace {
                let body_jump = self.chunk.emit_jump(OpCode::JumpRel, Chunk::impl_span());
                let incremental_start = self.chunk.instructions.len();
                self.expression(true)?;
                self.chunk.emit_impl_byte(OpCode::Pop);

                self.emit_loop(Chunk::impl_span(), start)?;
                start = incremental_start;
                self.patch_jump(body_jump, Chunk::impl_span())?;
            }

            // body 
            self.scoped_block()?;
            self.emit_loop(for_token.span, start)?;

            if let Some(exit) = exit {
                self.patch_jump(exit, for_token.span)?;
                self.chunk.emit_impl_byte(OpCode::Pop);
            }

            Ok(())
        })();
        self.end_scope();
        res
    }

    fn binary_op(&mut self, can_assign: bool) -> ParseResult<()> {
        // ought to be a binary expression
        let operation = self.pop().unwrap();
        let prec = Precedence::from(operation.data);
        self.expression_bp(prec, can_assign)?;

        // SAFETY: There must be 2 prior values on the stack. This is guaranteed by primary + expression
        macro_rules! emit {
                ($($opcode:expr),+) => {
                    emit_bytes!(self.chunk, operation.span; $($opcode,)+)
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
            _ => unreachable!("Expected a binary op"),
        }
        Ok(())
    }

    /// ensures: Ok(_) ==> Exactly one additional value on the stack
    #[cfg_attr(feature = "instrument", tracing::instrument(skip(self, chunk)))]
    fn expression_bp(
        &mut self,
        min: Precedence,
        mut can_assign: bool,
    ) -> ParseResult<()> {
        self.primary(can_assign)?;
        loop {
            let operation = self.peek()?;

            let prec = Precedence::from(operation.data);
            if prec <= min {
                break;
            }

            can_assign = prec <= Precedence::Assignment;

            match operation.data {
                Token::And => self.and_expr(),
                Token::Or => self.or_expr(),
                _ => self.binary_op(can_assign),
            }?
        }
        Ok(())
    }

    fn expression(&mut self, can_assign: bool) -> ParseResult<()> {
        self.expression_bp(Precedence::Start, can_assign)
    }

    fn block(&mut self) -> ParseResult<()> {
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
            self.declaration()?;
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
    fn print_statement(&mut self) -> ParseResult<()> {
        let print_token = self.pop()?;
        debug_assert_eq!(print_token.data, Token::Print);
        self.expression(false)?;
        self.check_semicolon(print_token.span)?;
        self.chunk.emit_byte(OpCode::Print, print_token.span);
        Ok(())
    }

    fn expression_statement(&mut self) -> ParseResult<()> {
        let expr_span = self.peek();
        self.expression(true)?;
        let expr_span = expr_span.unwrap().span;
        self.check_semicolon(expr_span)?;
        self.chunk.emit_impl_byte(OpCode::Pop);
        Ok(())
    }

    fn var_declaration(&mut self) -> ParseResult<()> {
        let var = self.pop().unwrap();
        debug_assert_eq!(var.data, Token::Var);

        let namespan = self.expect_identifier()?;

        if self.matches(Token::Eq).is_some() {
            self.expression(true)?;
        } else {
            self.chunk.emit_constant(Value::Nil, namespan);
        }

        self.check_semicolon(var.span)?;

        if self.in_global_scope() {
            let nameid = self.chunk.globals.add_or_get(&self.source[namespan]);
            emit_bytes!(self.chunk, namespan; OpCode::DefineGlobal, nameid);
        } else {
            self.add_local(&self.source[namespan]);
        }
        Ok(())
    }

    fn scoped_block(&mut self) -> ParseResult<()> {
        self.begin_scope();
        let res = self.block();
        self.end_scope();
        res
    }

    fn patch_jump(&mut self, addr: usize, span: Span) -> ParseResult<()> {
        let Ok(jump) = u16::try_from(self.chunk.instructions.len() - addr - 2) else {
            self.simple_error(span, "The body of this branch is too long and would generate more instructions than is supported.");
            return Err(ParseError::Handled);
        };
        let offset = jump.to_ne_bytes();
        self.chunk.instructions[addr..][..2].copy_from_slice(&offset);
        Ok(())
    }

    fn if_statement(&mut self) -> ParseResult<()> {
        let if_token = self.pop().unwrap();
        debug_assert_eq!(if_token.data, Token::If);
        // intentionally omit mandatory parens for a Rust-like syntax
        self.expression(false)?;

        let then_jump = self.chunk.emit_jump(OpCode::JumpRelIfFalse, if_token.span);
        self.chunk.emit_impl_byte(OpCode::Pop);
        self.scoped_block()?;
        let else_jump = self.chunk.emit_jump(OpCode::JumpRel, Chunk::impl_span());
        self.patch_jump(then_jump, if_token.span)?;

        self.chunk.emit_impl_byte(OpCode::Pop);
        let else_span = if let Some(else_token) = self.matches(Token::Else) {
            self.scoped_block()?;
            else_token.span
        } else {
            Chunk::impl_span()
        };
        self.patch_jump(else_jump, else_span)?;

        Ok(())
    }

    fn declaration(&mut self) -> ParseResult<()> {
        let token = self.peek()?;
        match token.data {
            Token::If => self.if_statement(),
            Token::Var => self.var_declaration(),
            Token::While => self.while_loop(),
            Token::For => self.for_loop(),
            Token::LBrace => self.scoped_block(),
            Token::Print => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn top(mut self) -> ParseResult<Chunk> {
        while self.peek()?.data != Token::Eof {
            self.declaration()?;
        }
        let next = self.peek()?;
        if next.data != Token::Eof {
            return Err(ParseError::ExpectError {
                expected: "EOF",
                got: next.span,
            });
        }
        self.chunk.emit_return();
        Ok(self.chunk)
    }
}

pub fn compile(source: &str, output: impl Write) -> ParseResult<Chunk> {
    let parser = Parser::new(source, output);
    parser.top()
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
