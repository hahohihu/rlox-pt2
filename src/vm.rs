use std::{io::Write, mem::size_of, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};
use bytemuck::{pod_read_unaligned, AnyBitPattern};

use crate::{
    common::ui::{self, Span},
    compiler::compile,
    repr::{
        chunk::{Chunk, OpCode},
        function::{ObjClosure, ObjFunction},
        string::UnsafeString,
        try_as::{TryAs, TryCast},
    },
    repr::{
        native_function::CallError,
        object::{Object, ObjectKind},
    },
    repr::{native_function::NativeFunction, value::Value},
};

#[derive(Copy, Clone, Debug)]
struct CallFrame {
    base_pointer: usize,
    return_addr: usize,
}

struct VM<'src, Stderr, Stdout> {
    chunk: Chunk,
    ip: usize,
    callframe: Vec<CallFrame>,
    stack: Vec<Value>,
    source: &'src str,
    stderr: Stderr,
    stdout: Stdout,
    /// SAFETY INVARIANT: All objects in objects are valid, and there are no duplicate allocations
    /// This is used to look for inaccessible objects to free
    objects: Vec<Object>,
    /// Chunk is the source of truth for indices
    globals: Vec<Option<Value>>,
}

impl<'src, Stderr, Stdout> Drop for VM<'src, Stderr, Stdout> {
    fn drop(&mut self) {
        for object in &self.objects {
            unsafe {
                // SAFETY: See safety invariant on objects
                object.free();
            }
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError = 1,
    RuntimeError = 2,
}

type InterpretResult = Result<(), InterpretError>;

impl<'src, Stderr: Write, Stdout: Write> VM<'src, Stderr, Stdout> {
    fn new(chunk: Chunk, source: &'src str, stderr: Stderr, stdout: Stdout) -> Self {
        debug_assert!(!chunk.instructions.is_empty());
        Self {
            callframe: vec![CallFrame {
                base_pointer: 0,
                return_addr: 0,
            }],
            ip: 0,
            chunk,
            source,
            stack: vec![],
            objects: vec![],
            stderr,
            stdout,
            globals: vec![],
        }
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.chunk.instructions[self.ip];
        self.jump(1);
        byte
    }

    /// This gets the span over the range relative to the current IP
    /// Note that this is a bit wonky since instructions aren't in the same order as code
    /// (and are variable-length)
    fn get_span(&self, range: Range<isize>) -> Span {
        let start = self.ip_offset() as isize + range.start;
        let end = self.ip_offset() as isize + range.end;
        debug_assert!(start < end);
        if start < 0 {
            // The bytecode will always have at least a return
            return self.chunk.spans[0];
        }
        let range = Range {
            start: start.max(0) as usize,
            end: (end as usize).min(self.chunk.spans.len()),
        };
        Span::unite_many(&self.chunk.spans[range])
    }

    fn read_constant(&mut self) -> Value {
        let i = self.next_byte();
        self.chunk.get_constant(i)
    }

    fn binary_num_op(&mut self, name: &str, op: impl Fn(f64, f64) -> Value) -> InterpretResult {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => self.stack.push(op(a, b)),
            (a, b) => {
                let span = self.get_span(-2..1);
                self.runtime_error(
                    span,
                    format!(
                        "Operator '{name}' takes two numbers. Got a {} ({a}) and a {} ({b}).",
                        a.typename(),
                        b.typename()
                    ),
                );
                return Err(InterpretError::RuntimeError);
            }
        }
        Ok(())
    }

    fn runtime_error(&mut self, span: Span, message: String) -> InterpretError {
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_message(message)
            .with_label(Label::new(span).with_color(Color::Red))
            .finish()
            // this mutable borrow infects everything it touches, hence &mut self
            // it isn't currently presenting an issue, but perhaps DI was a mistake
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
        InterpretError::RuntimeError
    }

    fn define_global(&mut self, index: u8, value: Value) {
        let index = index as usize;
        while self.globals.len() <= index {
            self.globals.push(None);
        }
        self.globals[index] = Some(value);
    }

    fn get_global(&mut self, index: u8) -> Result<Value, InterpretError> {
        match self.globals.get(index as usize) {
            Some(Some(value)) => Ok(*value),
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Undefined variable: {}", self.chunk.globals.get_name(index)),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn set_global(&mut self, index: u8, value: Value) -> InterpretResult {
        match self.globals.get_mut(index as usize) {
            Some(v) if v.is_some() => {
                *v = Some(value);
                Ok(())
            }
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Undefined variable: {}", self.chunk.globals.get_name(index)),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn negate(&mut self) -> InterpretResult {
        let val = self.stack.pop().unwrap();
        match val {
            Value::Num(n) => {
                self.stack.push(Value::Num(-n));
            }
            val => {
                let span = self.get_span(-1..0);
                self.runtime_error(
                    span,
                    format!("Tried to negate a {} ({val})", val.typename()),
                );
                return Err(InterpretError::RuntimeError);
            }
        }
        Ok(())
    }

    fn add(&mut self) -> InterpretResult {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => {
                self.stack.push(Value::Num(a + b));
                return Ok(());
            }
            (Value::Object(a), Value::Object(b)) => {
                let a = UnsafeString::try_cast(a);
                let b = UnsafeString::try_cast(b);
                if let (Some(a), Some(b)) = (a, b) {
                    let concatenated = Object::from(a + b);
                    self.objects.push(concatenated);
                    self.stack.push(Value::Object(concatenated));
                    return Ok(());
                }
            }
            _ => {}
        }
        let span = self.get_span(-2..1);
        self.runtime_error(
            span,
            format!(
                "Operator '+' takes two numbers. Got a {} ({a}) and a {} ({b}).",
                a.typename(),
                b.typename()
            ),
        );
        Err(InterpretError::RuntimeError)
    }

    fn jump(&mut self, offset: isize) {
        self.ip = (self.ip as isize + offset) as usize;
    }

    fn read<T: AnyBitPattern>(&mut self) -> T {
        let size = size_of::<T>();
        let bytes = &self.chunk.instructions[self.ip..][..size];
        self.ip += size;
        pod_read_unaligned(bytes)
    }

    fn ip_offset(&self) -> usize {
        self.ip
    }

    #[cfg(feature = "verbose_vm")]
    fn show_debug_trace(&self) {
        self.chunk
            .disassemble_instruction(self.ip_offset(), self.source, std::io::stdout());
        println!("==== STACK ====");
        let stack_len = self.callframe.last().unwrap().base_pointer;
        for value in &self.stack[stack_len..] {
            println!("{value}");
        }
        println!("=================");
        println!("==== GLOBALS ====");
        for (i, v) in self.globals.iter().enumerate() {
            if let Some(v) = v {
                println!("{} = {}", self.chunk.globals.get_name(i as u8), v);
            }
        }
        println!("=================");
    }

    fn peek(&self, i: usize) -> Value {
        self.stack[self.stack.len() - i - 1]
    }

    fn function_call(&mut self, closure: ObjClosure, arg_count: u8) -> InterpretResult {
        let function = closure.function;
        if function.arity != arg_count {
            let span = self.get_span(-2..0);
            self.runtime_error(
                span,
                format!(
                    "Function {} expects {} arguments, but got {}",
                    function.name, function.arity, arg_count
                ),
            );
            return Err(InterpretError::RuntimeError);
        }
        self.callframe.push(CallFrame {
            // -1 to include function itself
            base_pointer: self.stack.len() - arg_count as usize - 1,
            return_addr: self.ip,
        });
        self.ip = function.addr;
        Ok(())
    }

    fn native_function_call(&mut self, function: NativeFunction, arg_count: u8) -> InterpretResult {
        match function.call(&self.stack[self.stack.len() - arg_count as usize..]) {
            Ok(value) => {
                self.stack.push(value);
                Ok(())
            }
            Err(CallError::ArityMismatch(arity)) => {
                let span = self.get_span(-2..0);
                Err(self.runtime_error(
                    span,
                    format!(
                        "Function {} expects {} arguments, but got {}",
                        function.name, arity, arg_count
                    ),
                ))
            }
            Err(CallError::TypeMismatch(index, expected)) => {
                let span = self.get_span(-2..0);
                Err(self.runtime_error(span, format!("Argument {} expected {}", index, expected)))
            }
        }
    }

    fn call(&mut self, arg_count: u8) -> InterpretResult {
        let value = self.peek(arg_count.into());
        match value.try_as() {
            Some(ObjectKind::Closure { fun }) => self.function_call(fun, arg_count),
            Some(ObjectKind::NativeFunction { fun }) => self.native_function_call(fun, arg_count),
            _ => {
                let span = self.get_span(-2..0);
                self.runtime_error(
                    span,
                    format!("Canot call a value of type {}", value.typename()),
                );
                Err(InterpretError::RuntimeError)
            }
        }
    }

    fn run(&mut self) -> InterpretResult {
        if self.chunk.instructions.is_empty() {
            return Ok(());
        }

        for i in 0..self.chunk.native_globals.len() {
            let (id, value) = self.chunk.native_globals[i];
            self.define_global(id, value);
        }

        loop {
            #[cfg(feature = "verbose_vm")]
            self.show_debug_trace();
            let callframe = *self.callframe.last().unwrap();
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    let res = self.stack.pop().unwrap();
                    let callframe = self.callframe.pop().unwrap();
                    if self.callframe.is_empty() {
                        return Ok(());
                    }
                    while self.stack.len() > callframe.base_pointer {
                        self.stack.pop().unwrap();
                    }
                    self.stack.push(res);
                    self.ip = callframe.return_addr;
                }
                OpCode::Closure => {
                    let function: ObjFunction = self.read_constant().unwrap_as();
                    let closure = ObjClosure { function };
                    self.stack.push(closure.into());
                }
                OpCode::Call => {
                    let arg_count = self.next_byte();
                    self.call(arg_count)?;
                }
                OpCode::Pop => {
                    self.stack.pop().unwrap();
                }
                OpCode::JumpRelIfFalse => {
                    let offset = self.read::<u16>();
                    if self.stack.last().unwrap().falsey() {
                        self.jump(offset as isize);
                    }
                }
                OpCode::JumpRelIfTrue => {
                    let offset = self.read::<u16>();
                    if !self.stack.last().unwrap().falsey() {
                        self.jump(offset as isize);
                    }
                }
                OpCode::JumpRel => {
                    let offset = self.read::<u16>();
                    self.jump(offset as isize);
                }
                OpCode::Loop => {
                    let offset = self.read::<u16>();
                    self.jump(-(offset as isize));
                }
                OpCode::DefineGlobal => {
                    let index = self.next_byte();
                    let value = self.stack.last().unwrap();
                    self.define_global(index, *value);
                    self.stack.pop().unwrap();
                }
                OpCode::GetGlobal => {
                    let index = self.next_byte();
                    let value = self.get_global(index)?;
                    self.stack.push(value);
                }
                OpCode::SetGlobal => {
                    let index = self.next_byte();
                    let value = self.stack.last().unwrap();
                    self.set_global(index, *value)?;
                }
                OpCode::SetLocal => {
                    let slot = self.next_byte();
                    self.stack[callframe.base_pointer + slot as usize] =
                        *self.stack.last().unwrap();
                }
                OpCode::GetLocal => {
                    let slot = self.next_byte();
                    self.stack
                        .push(self.stack[callframe.base_pointer + slot as usize]);
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OpCode::Nil => {
                    self.stack.push(Value::Nil);
                }
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Negate => self.negate()?,
                OpCode::Not => {
                    let value = Value::Bool(self.stack.pop().unwrap().falsey());
                    self.stack.push(value);
                }
                OpCode::Print => {
                    let value = self.stack.pop().unwrap();
                    writeln!(self.stdout, "{value}").unwrap();
                }
                OpCode::Add => self.add()?,
                OpCode::Sub => self.binary_num_op("-", |a, b| Value::Num(a - b))?,
                OpCode::Mul => self.binary_num_op("*", |a, b| Value::Num(a * b))?,
                OpCode::Div => self.binary_num_op("/", |a, b| Value::Num(a / b))?,
                OpCode::Less => self.binary_num_op("<", |a, b| Value::Bool(a < b))?,
                OpCode::Greater => self.binary_num_op(">", |a, b| Value::Bool(a > b))?,
                OpCode::Equal => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                OpCode::Invalid => {
                    unreachable!("Reached invalid opcode at {}", self.ip_offset())
                }
            }
        }
    }
}

pub fn interpret(source: &str, mut stderr: impl Write, mut stdout: impl Write) -> InterpretResult {
    let Some(chunk) = compile(source, &mut stderr) else {
        return Err(InterpretError::CompileError);
    };
    let mut vm = VM::new(chunk, source, &mut stderr, &mut stdout);
    vm.run()
}
