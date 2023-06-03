use std::fmt::Binary;
use std::io::Write;

use crate::repr::interner::InternedU8;
use crate::repr::interner::Interner;

use crate::repr::chunk::Chunk;
use crate::repr::chunk::OpCode;
use crate::repr::value::Value;

use crate::common::ui;
use crate::common::ui::*;

use super::parse::BinaryExpr;
use super::parse::BinaryKind;
use super::parse::Expression;
use super::parse::Literal;
use super::parse::Statement;
use super::parse::Statements;
use super::parse::UnaryKind;

type LocalSymbol = InternedU8;
struct Parser<'src, StdErr: Write> {
    chunk: Chunk,
    source: &'src str,
    stderr: StdErr,
    interned_locals: Interner,
    defined_locals: Vec<LocalSymbol>,
    scope_size: Vec<LocalSymbol>,
}

pub type CodegenResult<T> = Result<T, ()>;

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

impl<'src, StdErr: Write> Parser<'src, StdErr> {
    fn new(source: &'src str, stderr: StdErr) -> Self {
        Self {
            chunk: Chunk::new(),
            source,
            stderr,
            interned_locals: Interner::default(),
            defined_locals: Default::default(),
            scope_size: Default::default(),
        }
    }

    fn simple_error(&mut self, span: ui::Span, msg: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_label(Label::new(span).with_color(Color::Red).with_message(msg))
            .finish()
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
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

    fn patch_jump(&mut self, addr: usize, span: Span) -> CodegenResult<()> {
        let Ok(jump) = u16::try_from(self.chunk.instructions.len() - addr - 2) else {
            self.simple_error(span, "The body of this branch is too long and would generate more instructions than is supported.");
            return Err(());
        };
        let offset = jump.to_ne_bytes();
        self.chunk.instructions[addr..][..2].copy_from_slice(&offset);
        Ok(())
    }

    fn emit_loop(&mut self, span: Span, start: usize) -> CodegenResult<()> {
        self.chunk.emit_byte(OpCode::Loop, span);
        let offset = self.chunk.instructions.len() - start + 2;
        let Ok(offset) = u16::try_from(offset) else {
            self.simple_error(span, "This loop would have a longer body than is supported");
            return Err(());
        };
        let offset = offset.to_ne_bytes();
        emit_bytes!(self.chunk, span; offset[0], offset[1]);
        Ok(())
    }

    fn literal(&mut self, literal: &Spanned<Literal>) -> CodegenResult<()> {
        match &literal.data {
            Literal::Number(n) => {
                self.chunk.emit_constant(Value::Num(*n), literal.span);
            }
            Literal::String(s) => {
                self.chunk
                    .emit_constant(Value::from(s.as_str()), literal.span);
            }
            Literal::Boolean(b) => {
                if *b {
                    self.chunk.emit_byte(OpCode::True, literal.span);
                } else {
                    self.chunk.emit_byte(OpCode::False, literal.span);
                }
            }
            Literal::Nil => self.chunk.emit_byte(OpCode::Nil, literal.span),
        };
        Ok(())
    }

    fn and_expr(&mut self, expr: &BinaryExpr) -> CodegenResult<()> {
        self.expression(&expr.lhs.data)?;
        let end = self.chunk.emit_jump(OpCode::JumpRelIfFalse, expr.kind.span);
        self.chunk.emit_impl_byte(OpCode::Pop);

        self.expression(&expr.rhs.data)?;
        self.patch_jump(end, expr.kind.span)?;
        Ok(())
    }

    fn or_expr(&mut self, expr: &BinaryExpr) -> CodegenResult<()> {
        self.expression(&expr.lhs.data)?;
        let end = self.chunk.emit_jump(OpCode::JumpRelIfTrue, expr.kind.span);
        self.chunk.emit_impl_byte(OpCode::Pop);

        self.expression(&expr.rhs.data)?;
        self.patch_jump(end, expr.kind.span)?;
        Ok(())
    }

    fn binary_op(&mut self, expr: &BinaryExpr) -> CodegenResult<()> {
        self.expression(&expr.lhs.data)?;
        self.expression(&expr.rhs.data)?;
        macro_rules! emit {
            ($($opcode:expr),+) => {
                emit_bytes!(self.chunk, expr.kind.span; $($opcode,)+)
            };
        }
        match expr.kind.data {
            BinaryKind::Minus => emit!(OpCode::Sub),
            BinaryKind::Plus => emit!(OpCode::Add),
            BinaryKind::Divide => emit!(OpCode::Div),
            BinaryKind::Multiply => emit!(OpCode::Mul),
            BinaryKind::Equals => emit!(OpCode::Equal),
            BinaryKind::NotEquals => emit!(OpCode::Equal, OpCode::Not),
            BinaryKind::GreaterThan => emit!(OpCode::Greater),
            BinaryKind::GreaterThanEqual => emit!(OpCode::Less, OpCode::Not),
            BinaryKind::LessThan => emit!(OpCode::Less),
            BinaryKind::LessThanEqual => emit!(OpCode::Greater, OpCode::Not),
            _ => unreachable!("Should be handled by function preconditions"),
        }
        Ok(())
    }

    fn expression(&mut self, expression: &Expression) -> CodegenResult<()> {
        match expression {
            Expression::Binary(bin@BinaryExpr { kind, .. }) => {
                match kind.data {
                    BinaryKind::And => self.and_expr(bin)?,
                    BinaryKind::Or => self.or_expr(bin)?,
                    _ => self.binary_op(bin)?
                }
            },
            Expression::Unary { kind, val } => {
                self.expression(&val.data)?;
                let opcode = match kind.data {
                    UnaryKind::Not => OpCode::Not,
                    UnaryKind::Neg => OpCode::Negate,
                };
                self.chunk.emit_byte(opcode, kind.span);
            }
            Expression::Literal(lit) => self.literal(lit)?,
            Expression::Assignment { id, rhs } => {
                let (opcode, follow_byte) = if let Some(pos) = self.resolve_local(&id.data) {
                    (OpCode::SetLocal, pos)
                } else {
                    (OpCode::SetGlobal, self.chunk.globals.add_or_get(&id.data))
                };
                self.expression(&rhs.data)?;
                emit_bytes!(self.chunk, id.span; opcode, follow_byte);
            },
            Expression::Identifier(id) => {
                let (opcode, follow_byte) = if let Some(pos) = self.resolve_local(&id.data) {
                    (OpCode::GetLocal, pos)
                } else {
                    (OpCode::GetGlobal, self.chunk.globals.add_or_get(&id.data))
                };
                emit_bytes!(self.chunk, id.span; opcode, follow_byte);
            },
        }
        Ok(())
    }

    fn block(&mut self, stmts: &Statements) -> CodegenResult<()> {
        for stmt in stmts.0.iter() {
            self.statement(&stmt.data)?;
        }
        Ok(())
    }

    fn scoped_block(&mut self, stmts: &Statements) -> CodegenResult<()> {
        self.begin_scope();
        let res = self.block(stmts);
        self.end_scope();
        res
    }

    fn statement(&mut self, statement: &Statement) -> CodegenResult<()> {
        match statement {
            Statement::Expr(expr) => {
                self.expression(&expr.data)?;
                self.chunk.emit_impl_byte(OpCode::Pop);
            }
            Statement::Print(expr) => {
                self.expression(&expr.data)?;
                self.chunk.emit_impl_byte(OpCode::Print);
            }
            Statement::VarDeclaration { id, rhs } => {
                if let Some(rhs) = rhs {
                    self.expression(&rhs.data)?;
                } else {
                    self.chunk.emit_constant(Value::Nil, id.span);
                }
                if self.in_global_scope() {
                    let nameid = self.chunk.globals.add_or_get(&id.data);
                    emit_bytes!(self.chunk, id.span; OpCode::DefineGlobal, nameid);
                } else {
                    self.add_local(&id.data);
                }
            }
            Statement::Block(statements) => self.scoped_block(&statements.data)?,
            Statement::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                self.expression(&cond.data)?;
                let then_jump = self.chunk.emit_jump(OpCode::JumpRelIfFalse, cond.span);
                self.chunk.emit_impl_byte(OpCode::Pop);
                self.scoped_block(&then_branch.data)?;
                let else_jump = self.chunk.emit_jump(OpCode::JumpRel, Chunk::impl_span());
                self.patch_jump(then_jump, Chunk::impl_span())?;

                self.chunk.emit_impl_byte(OpCode::Pop);
                if let Some(else_branch) = else_branch {
                    self.scoped_block(&else_branch.data)?;
                }
                self.patch_jump(else_jump, Chunk::impl_span())?;
            }
            Statement::While { cond, body } => {
                let start = self.chunk.instructions.len();
                self.expression(&cond.data)?;

                let exit = self.chunk.emit_jump(OpCode::JumpRelIfFalse, cond.span);
                self.chunk.emit_impl_byte(OpCode::Pop);

                self.scoped_block(&body.data)?;
                self.emit_loop(cond.span, start)?;

                self.patch_jump(exit, cond.span)?;
                self.chunk.emit_impl_byte(OpCode::Pop);
            }
            Statement::Return { span, value } => todo!(),
        }
        Ok(())
    }

    fn top(mut self, top: &Statements) -> CodegenResult<Chunk> {
        for statement in top.0.iter() {
            self.statement(&statement.data)?
        }
        self.chunk.emit_return();
        Ok(self.chunk)
    }
}

pub fn generate(source: &str, stderr: impl Write, ast: &Statements) -> CodegenResult<Chunk> {
    let compiler = Parser::new(source, stderr);
    compiler.top(ast)
}
