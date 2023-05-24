use std::{io::Write, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    common::ui::{self, Span},
    compiler::compile,
    repr::chunk::{Chunk, OpCode},
    repr::object::Object,
    repr::value::Value,
};

struct VM<'src, Stderr, Stdout> {
    chunk: Chunk,
    ip: usize,
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
        Self {
            chunk,
            source,
            stack: vec![],
            ip: 0,
            objects: vec![],
            stderr,
            stdout,
            globals: vec![],
        }
    }

    fn next_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.instructions[self.ip - 1]
    }

    /// This gets the span over the range relative to the current IP
    /// Note that this is a bit wonky since instructions aren't in the same order as code
    /// (and are variable-length)
    fn get_span(&self, range: Range<isize>) -> Span {
        let start = self.ip as isize + range.start;
        let end = self.ip as isize + range.end;
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

    fn runtime_error(&mut self, span: Span, message: String) {
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_message(message)
            .with_label(Label::new(span).with_color(Color::Red))
            .finish()
            .write(Source::from(self.source), &mut self.stderr)
            .unwrap();
    }

    fn set_global(&mut self, index: u8, value: Value) {
        let index = index as usize;
        while self.globals.len() <= index {
            self.globals.push(None);
        }
        self.globals[index] = Some(value);
    }

    fn run(&mut self) -> InterpretResult {
        if self.chunk.instructions.is_empty() {
            return Ok(());
        }
        loop {
            #[cfg(feature = "verbose_vm")]
            {
                self.chunk
                    .disassemble_instruction(self.ip, self.source, std::io::stdout());
                println!("==== STACK ====");
                let stack_len = self.stack.len().saturating_sub(8);
                for value in &self.stack[stack_len..] {
                    println!("{value}");
                }
                println!("=================");
                println!("==== GLOBALS ====");
                for (k, v) in &self.globals {
                    println!("{k} = {v}");
                }
                println!("=================");
            }
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Pop => {
                    self.stack.pop().unwrap();
                }
                OpCode::DefineGlobal => {
                    let index = self.next_byte();
                    let value = self.stack.last().unwrap();
                    self.set_global(index, *value);
                    self.stack.pop().unwrap();
                }
                OpCode::GetGlobal => {
                    let index = self.next_byte();
                    let Some(Some(value)) = self.globals.get(index as usize) else {
                        let span = self.get_span(-2..0);
                        self.runtime_error(span, format!("Undefined variable: {}", self.chunk.globals.get_name(index)));
                        return Err(InterpretError::RuntimeError);
                    };
                    self.stack.push(*value);
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
                OpCode::Negate => {
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Num(n) => {
                            self.stack.push(Value::Num(-n));
                        }
                        val => {
                            let span = self.get_span(-3..0);
                            self.runtime_error(
                                span,
                                format!("Tried to negate a {} ({val})", val.typename()),
                            );
                            return Err(InterpretError::RuntimeError);
                        }
                    }
                }
                OpCode::Not => {
                    let value = Value::Bool(self.stack.pop().unwrap().falsey());
                    self.stack.push(value);
                }
                OpCode::Print => {
                    let value = self.stack.pop().unwrap();
                    writeln!(self.stdout, "{value}").unwrap();
                }
                OpCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Num(a), Value::Num(b)) => self.stack.push(Value::Num(a + b)),
                        (Value::Object(a), Value::Object(b)) if a.is_string() && b.is_string() => {
                            let (a, b) = unsafe { (a.assume_string(), b.assume_string()) };
                            let concatenated = Object::from(a + b);
                            self.objects.push(concatenated);
                            self.stack.push(Value::Object(concatenated));
                        }
                        (a, b) => {
                            let span = self.get_span(-2..1);
                            self.runtime_error(
                                span,
                                format!(
                                    "Operator '+' takes two numbers. Got a {} ({a}) and a {} ({b}).",
                                    a.typename(),
                                    b.typename()
                                ),
                            );
                            return Err(InterpretError::RuntimeError);
                        }
                    }
                    Ok(())
                }?,
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
                OpCode::Invalid => unreachable!("Reached invalid opcode at {}", self.ip),
            }
        }
    }
}

pub fn interpret(source: &str, mut stderr: impl Write, mut stdout: impl Write) -> InterpretResult {
    let chunk = match compile(source, &mut stderr) {
        Ok(chunk) => chunk,
        Err(e) => {
            e.print(&mut stderr, source);
            return Err(InterpretError::CompileError);
        }
    };
    let mut vm = VM::new(chunk, source, &mut stderr, &mut stdout);
    vm.run()
}
