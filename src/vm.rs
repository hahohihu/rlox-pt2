use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    chunk::{Chunk, OpCode},
    parse::compile,
    ui::{self, Span},
    value::Value,
};

struct VM<'src> {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    source: &'src str,
}

pub enum InterpretError {
    CompileError = 1,
    RuntimeError = 2,
}

type InterpretResult = Result<(), InterpretError>;

impl<'src> VM<'src> {
    fn new(chunk: Chunk, source: &'src str) -> Self {
        Self {
            chunk,
            source,
            stack: vec![],
            ip: 0,
        }
    }

    fn next_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.instructions[self.ip - 1]
    }

    /// This gets the span over the range relative to the current IP
    /// Note that this is a bit wonky since instructions aren't in the same order as code
    /// (and are variable-length)
    fn get_span(&self, range: Range<isize>) -> Option<Span> {
        let start = self.ip as isize + range.start;
        let end = self.ip as isize + range.end;
        debug_assert!(start < end);
        if start < 0 {
            return self.chunk.spans.get(0).copied();
        }
        let range = Range {
            start: start as usize,
            end: end as usize,
        };
        self.chunk.spans.get(range).map(Span::unite_many)
    }

    fn read_constant(&mut self) -> Value {
        let i = self.next_byte() as usize;
        self.chunk.constants[i]
    }

    fn binary_num_op(&mut self, name: &str, op: impl Fn(f64, f64) -> f64) -> InterpretResult {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        match (a, b) {
            (Value::Num(a), Value::Num(b)) => {
                self.stack.push(Value::Num(op(a, b)))
            }
            (a, b) => {
                let span = self.get_span(-2..1).unwrap();
                self.runtime_error(span, format!("Operator '{name}' takes two numbers. Got a {} ({a}) and a {} ({b}).", a.typename(), b.typename()));
                return Err(InterpretError::RuntimeError);
            }
        }
        Ok(())
    }

    fn runtime_error(&self, span: Span, message: String) {
        Report::build(ReportKind::Error, (), ui::OFFSET)
            .with_message(message)
            .with_label(Label::new(span).with_color(Color::Red))
            .finish()
            .eprint(Source::from(self.source))
            .unwrap();
    }

    fn run(&mut self) -> InterpretResult {
        if self.chunk.instructions.is_empty() {
            return Ok(());
        }
        loop {
            #[cfg(feature = "verbose")]
            {
                self.chunk.disassemble_instruction(self.ip, self.source);
                println!("==== STACK ====");
                let stack_len = self.stack.len().saturating_sub(8);
                for value in &self.stack[stack_len..] {
                    println!("{value}");
                }
                println!("==============================");
            }
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    if let Some(v) = self.stack.pop() {
                        println!("Returning: {v}");
                    }
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OpCode::Negate => {
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Bool(b) => {
                            let span = self.get_span(-3..0).unwrap();
                            self.runtime_error(span, format!("Tried to negate a boolean ({b})"));
                            return Err(InterpretError::RuntimeError);
                        }
                        Value::Num(n) => {
                            self.stack.push(Value::Num(-n));
                        }
                    }
                }
                OpCode::Add => self.binary_num_op("+", |a, b| a + b)?,
                OpCode::Sub => self.binary_num_op("-", |a, b| a - b)?,
                OpCode::Mul => self.binary_num_op("*", |a, b| a * b)?,
                OpCode::Div => self.binary_num_op("/", |a, b| a / b)?,
                OpCode::Invalid => unreachable!("Reached invalid opcode at {}", self.ip),
            }
        }
    }
}

pub fn interpret(source: &str) -> InterpretResult {
    let chunk = match compile(source) {
        Ok(chunk) => chunk,
        Err(e) => {
            e.print(source);
            return Err(InterpretError::CompileError);
        }
    };
    let mut vm = VM::new(chunk, source);
    vm.run()
}
