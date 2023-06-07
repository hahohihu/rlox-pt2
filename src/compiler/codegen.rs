mod scope;

use std::io::Write;

use std::slice::SliceIndex;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use crate::repr::interner::InternedU8;
use crate::repr::interner::Interner;

use crate::repr::chunk::Chunk;
use crate::repr::chunk::OpCode;

use crate::common::ui;
use crate::common::ui::*;
use crate::value::function::ObjFunction;
use crate::value::native_function::CallError;
use crate::value::native_function::NativeFunction;
use crate::value::string::UnsafeString;
use crate::value::Value;

use self::scope::Scope;

use super::parse::BinaryExpr;
use super::parse::BinaryKind;
use super::parse::Call;
use super::parse::Expression;
use super::parse::FunctionDeclaration;
use super::parse::Literal;
use super::parse::Statement;
use super::parse::Statements;
use super::parse::UnaryKind;

struct StaticCallFrame {
    base_pointer: usize,
    upvalues: Vec<Upvalue>,
}

#[derive(Debug, PartialEq, Eq)]
struct Upvalue {
    local: bool,
    index: u8,
}

struct Local {
    depth: u8,
    captured: bool,
}

type LocalSymbol = InternedU8;
struct Compiler<'src, StdErr: Write> {
    chunk: Chunk,
    source: &'src str,
    stderr: StdErr,
    interned_locals: Interner,
    defined_locals: Vec<Local>,
    scope_size: Vec<usize>,
    static_call_stack: Vec<StaticCallFrame>,
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
        emit_bytes!(self, Chunk::impl_span(); OpCode::Nil, OpCode::Return);
    }

    fn emit_jump(&mut self, jump: OpCode, span: Span) -> usize {
        emit_bytes!(self, span; jump, 0xff, 0xff);
        self.instructions.len() - 2
    }
}

impl<'src, StdErr: Write> Compiler<'src, StdErr> {
    fn new(source: &'src str, stderr: StdErr) -> Self {
        Self {
            chunk: Chunk::new(),
            source,
            stderr,
            interned_locals: Interner::default(),
            defined_locals: Default::default(),
            scope_size: Default::default(),
            static_call_stack: vec![StaticCallFrame {
                base_pointer: 0,
                upvalues: vec![],
            }],
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

    fn begin_function_scope(&mut self) {
        self.static_call_stack.push(StaticCallFrame {
            base_pointer: self.defined_locals.len(),
            upvalues: vec![],
        });

        self.scope_size.push(0);
    }

    fn end_function_scope(&mut self) {
        let size = self.scope_size.pop().unwrap();
        for _ in 0..size {
            // todo: not clear this works in light of upvalues
            // The pop opcodes are redundant because return will handle this for functions
            self.defined_locals.pop().unwrap();
        }
        self.chunk.emit_return();
    }

    fn end_scope(&mut self) {
        let size = self.scope_size.pop().unwrap();
        for _ in 0..size {
            let local = self.defined_locals.pop().unwrap();
            if local.captured {
                self.chunk.emit_impl_byte(OpCode::CloseUpvalue);
            } else {
                self.chunk.emit_impl_byte(OpCode::Pop);
            }
        }
    }

    fn define_local(&mut self, name: &str) -> u8 {
        self.interned_locals.add_or_get(name)
    }

    fn add_local(&mut self, name: &str) {
        let nameid = self.define_local(name);
        self.defined_locals.push(Local {
            depth: nameid,
            captured: false,
        });
        let size = self.scope_size.last_mut().unwrap();
        *size += 1;
    }

    fn resolve_local(
        &self,
        name: &str,
        local_range: impl SliceIndex<[Local], Output = [Local]>,
    ) -> Option<LocalSymbol> {
        let nameid = self.interned_locals.get(name)?;
        // offset is needed to handle functions
        // i - base_pointer for offset, which the VM will use with base_pointer + i
        // there is conceptual overlap, but the previous bp is static, where the latter bp is dynamic
        for (i, local) in self.defined_locals[local_range].iter().enumerate().rev() {
            if nameid == local.depth {
                // putting function code inline + jumping over it is slightly sus with closures or self-modifying code, but I don't think the latter will happen
                // and iirc, closure code will be modified such that they don't need duplication
                // todo: this will actually crash if it doesn't fit
                return Some(i.try_into().unwrap());
            }
        }
        None
    }

    fn add_upvalue(&mut self, upvalue: Upvalue, callframe_index: usize) -> u8 {
        let upvalues = &mut self.static_call_stack[callframe_index].upvalues;
        if let Some((i, _)) = upvalues
            .iter()
            .enumerate()
            .find(|(_, upval)| **upval == upvalue)
        {
            return i as u8;
        }
        upvalues.push(upvalue);
        // this probably also crashes but only in extremely degenerate cases (256 static closure captures)
        (upvalues.len() - 1).try_into().unwrap()
    }

    fn _inner_resolve_upvalue(&mut self, name: &str, callframe_index: usize) -> Option<u8> {
        if callframe_index == 0 {
            return None;
        }
        let enclosing_callframe = &self.static_call_stack[callframe_index - 1];
        let callframe = &self.static_call_stack[callframe_index];

        if let Some(local_index) = self.resolve_local(
            name,
            enclosing_callframe.base_pointer..callframe.base_pointer,
        ) {
            self.defined_locals[local_index as usize].captured = true;
            return Some(self.add_upvalue(
                Upvalue {
                    local: true,
                    index: local_index,
                },
                callframe_index,
            ));
        }

        if let Some(upvalue) = self._inner_resolve_upvalue(name, callframe_index - 1) {
            return Some(self.add_upvalue(
                Upvalue {
                    local: false,
                    index: upvalue,
                },
                callframe_index,
            ));
        }

        None
    }

    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        self._inner_resolve_upvalue(name, self.static_call_stack.len() - 1)
    }

    fn resolve(&mut self, name: &str) -> (Scope, u8) {
        let base_pointer = self.static_call_stack.last().unwrap().base_pointer;
        if let Some(pos) = self.resolve_local(name, base_pointer..) {
            (Scope::Local, pos)
        } else if let Some(pos) = self.resolve_upvalue(name) {
            (Scope::Upvalue, pos)
        } else {
            (Scope::Global, self.chunk.globals.add_or_get(name))
        }
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
            Expression::Binary(bin @ BinaryExpr { kind, .. }) => match kind.data {
                BinaryKind::And => self.and_expr(bin)?,
                BinaryKind::Or => self.or_expr(bin)?,
                _ => self.binary_op(bin)?,
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
                let (scope, follow_byte) = self.resolve(&id.data);
                let opcode = scope.set_opcode();
                self.expression(&rhs.data)?;
                emit_bytes!(self.chunk, id.span; opcode, follow_byte);
            }
            Expression::Identifier(id) => {
                let (scope, follow_byte) = self.resolve(&id.data);
                let opcode = scope.get_opcode();
                emit_bytes!(self.chunk, id.span; opcode, follow_byte);
            }
            Expression::Call(call) => self.function_call(call)?,
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

    fn function_call(&mut self, call: &Call) -> CodegenResult<()> {
        let Call { callee, args } = call;
        self.expression(&callee.data)?;
        for arg in args {
            self.expression(&arg.data)?;
        }
        emit_bytes!(self.chunk, callee.span; OpCode::Call, args.len() as u8);
        Ok(())
    }

    fn function_declaration(&mut self, declaration: &FunctionDeclaration) -> CodegenResult<()> {
        let FunctionDeclaration { name, args, body } = declaration;

        // We aren't making separate chunks to keep everything in the same allocation
        // So skip the function
        let skip = self.chunk.emit_jump(OpCode::JumpRel, Chunk::impl_span());

        self.begin_function_scope(); // fyi: if we ever add recovery, this may break because of early-returns
        let function_start = self.chunk.instructions.len();

        // mark self so recursive calls work
        self.add_local(&name.data);
        // callee must initialize the args, this is just to make the offsets work
        for arg in args {
            self.add_local(&arg.data);
        }

        self.block(&body.data)?;
        self.end_function_scope();

        let callframe = self.static_call_stack.pop().unwrap();

        self.patch_jump(skip, Chunk::impl_span())?;

        let function = ObjFunction {
            arity: args.len().try_into().unwrap(),
            upvalues: callframe.upvalues.len() as u8,
            addr: function_start,
            name: UnsafeString::from(name.data.as_str()),
        };

        let constant = self.chunk.add_constant(function.into());
        emit_bytes!(self.chunk, name.span; OpCode::Closure, constant);

        for upvalue in callframe.upvalues {
            emit_bytes!(self.chunk, Chunk::impl_span(); upvalue.local as u8, upvalue.index);
        }

        if self.in_global_scope() {
            let nameid = self.chunk.globals.add_or_get(&name.data);
            emit_bytes!(self.chunk, name.span; OpCode::DefineGlobal, nameid);
        } else {
            self.add_local(&name.data);
        }

        Ok(())
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
                let if_false_jump = self.chunk.emit_jump(OpCode::JumpRelIfFalse, cond.span);
                self.chunk.emit_impl_byte(OpCode::Pop);
                self.scoped_block(&then_branch.data)?;
                let end_jump = if let Some(else_branch) = else_branch {
                    // skip else after if
                    let end_else_jump = self.chunk.emit_jump(OpCode::JumpRel, Chunk::impl_span());
                    self.patch_jump(if_false_jump, Chunk::impl_span())?;
                    // pop the condition
                    self.chunk.emit_impl_byte(OpCode::Pop);
                    self.scoped_block(&else_branch.data)?;
                    end_else_jump
                } else {
                    if_false_jump
                };

                self.patch_jump(end_jump, Chunk::impl_span())?;
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
            Statement::Return { span, value } => {
                if let Some(value) = value {
                    self.expression(&value.data)?;
                    self.chunk.emit_byte(OpCode::Return, *span);
                } else {
                    self.chunk.emit_return();
                }
            }
            Statement::FunctionDeclaration(declaration) => {
                self.function_declaration(declaration)?
            }
        }
        Ok(())
    }

    fn top(mut self, top: &Statements) -> CodegenResult<Chunk> {
        self.define_native_function("clock", |values| {
            if !values.is_empty() {
                return Err(CallError::ArityMismatch(0));
            }
            let time = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_or(f64::NAN, |n| n.as_secs_f64());
            Ok(Value::Num(time))
        });
        for statement in top.0.iter() {
            self.statement(&statement.data)?
        }
        self.chunk.emit_return();
        Ok(self.chunk)
    }

    fn define_native_function(
        &mut self,
        name: &str,
        function: fn(&[Value]) -> Result<Value, CallError>,
    ) {
        let nameid = self.chunk.globals.add_or_get(name);
        self.chunk.add_native(
            nameid,
            Value::from(NativeFunction {
                name: UnsafeString::from(name),
                function,
            }),
        );
    }
}

pub fn generate(source: &str, stderr: impl Write, ast: &Statements) -> CodegenResult<Chunk> {
    let compiler = Compiler::new(source, stderr);
    compiler.top(ast)
}

#[cfg(test)]
mod tests {
    use crate::snap_codegen;
    snap_codegen! {
        calls_and_other_operator,
        "
        fun foo() {
            return 1;
        }
        print foo() + foo();
        "
    }

    snap_codegen! {
        scoped_recursion,
        "
        {
            fun foo(n) {
                if n == 0 {
                    return 0;
                } else {
                    return 1 + foo(n - 1);
                }
            }
            print foo(1);
        }
        "
    }

    snap_codegen! {
        nil_return_and_other_operator,
        "
        fun ni() {}
        print ni() or ni();
        "
    }

    snap_codegen! {
        if_then_nil,
        "
        fun foo() {
            if false {
                return 0;
            }
        }
        print foo();
        "
    }

    snap_codegen! {
        closure_local,
        "
        fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
              var c = 3;
              var d = 4;
              fun inner() {
                print a + c + b + d;
              }
            }
          }
        "
    }

    snap_codegen! {
        closure_global,
        "
        var a = 1;
        fun closure() {
            print a;
        }
        "
    }
}
