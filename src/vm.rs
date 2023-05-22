use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    chunk::{Chunk, OpCode},
    object::Object,
    parse::compile,
    ui::{self, Span},
    value::Value,
};

struct VM<'src> {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    source: &'src str,
    // SAFETY INVARIANT: All objects in objects are valid, and there are no duplicate allocations
    objects: Vec<Object>,
}

impl<'src> Drop for VM<'src> {
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

type TestInterpretResult<'src> = Result<VM<'src>, InterpretError>;
type InterpretResult = Result<(), InterpretError>;

impl<'src> VM<'src> {
    fn new(chunk: Chunk, source: &'src str) -> Self {
        Self {
            chunk,
            source,
            stack: vec![],
            ip: 0,
            objects: vec![],
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
            #[cfg(feature = "verbose_vm")]
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
                    return Ok(());
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
                    println!("{value}");
                }
                OpCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Num(a), Value::Num(b)) => self.stack.push(Value::Num(a + b)),
                        (Value::Object(a), Value::Object(b)) if a.is_string() && b.is_string() => {
                            let concatenated = Object::concatenate_strings(a, b);
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

fn test_interpret(source: &str) -> TestInterpretResult {
    let chunk = match compile(source) {
        Ok(chunk) => chunk,
        Err(e) => {
            e.print(source);
            return Err(InterpretError::CompileError);
        }
    };
    let mut vm = VM::new(chunk, source);
    vm.run()?;
    Ok(vm)
}

pub fn interpret(source: &str) -> InterpretResult {
    test_interpret(source).map(|_| ())
}

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use tracing::Level;

    use crate::value::{Comparable, Value};

    use super::test_interpret;

    pub fn setup_test() {
        static LOGGING: Once = Once::new();
        LOGGING.call_once(|| {
            tracing_subscriber::fmt()
                .with_max_level(Level::TRACE)
                .init();
        })
    }

    fn check_stack(source: &str, result: impl Comparable + std::fmt::Display) {
        setup_test();
        match test_interpret(source) {
            Ok(v) => {
                assert_eq!(v.stack.len(), 1);
                if !result.compare(&v.stack[0]) {
                    panic!("Expected: '{}'\nGot: '{}'", result, &v.stack[0])
                }
            }
            Err(e) => {
                panic!("{e:?}");
            }
        }
    }

    fn failure(source: &str) {
        setup_test();
        assert!(test_interpret(source).is_err());
    }

    #[test]
    fn primaries() {
        check_stack("return 1;", 1.0);
        check_stack("return 0.1;", 0.1);
        check_stack("return false;", false);
        check_stack("return true;", true);
        check_stack("return nil;", Value::Nil);
    }

    #[test]
    fn arithmetic() {
        check_stack("return 1 + 2 * 3;", 7.0);
        check_stack("return 6 * 6 / 3;", 12.0);
        check_stack("return 20 * 5 / 0.5 - 100.0;", 100.0);
    }

    #[test]
    fn parens() {
        check_stack("return 2 * (6 + 1) / (2) -- 100;", 107.0);
        check_stack("return (((1 + 1) / 2) * 3);", 3.0);
    }

    #[test]
    fn falsey() {
        check_stack("return !nil;", true);
        check_stack("return !false;", true);
        check_stack("return !0;", false);
        check_stack("return !true;", false);
        check_stack("return !\"\";", false);
    }

    #[test]
    fn numeric_comparison() {
        check_stack("return 1 > 1;", false);
        check_stack("return 1 >= 1;", true);
        check_stack("return 1 < 1;", false);
        check_stack("return 1 <= 1;", true);
        check_stack("return 1 == 1;", true);
    }

    #[test]
    fn strings() {
        check_stack(r#"return "foo";"#, "foo");
    }

    #[test]
    fn concatenation() {
        check_stack(r#"return "foo" + "bar";"#, "foobar");
    }

    #[test]
    fn string_comparison() {
        check_stack(r#"return "foo" == "foo";"#, true);
    }

    #[test]
    fn compound_string() {
        check_stack(r#"return "foo" + "bar" == "f" + "oo" + "bar";"#, true);
    }

    #[test]
    fn unicode() {
        check_stack(
            r#"return "💩" + "👪" + "༕" + "갍" + "⑯" + "ฒ" + "ڦ";"#,
            "💩👪༕갍⑯ฒڦ",
        );
    }

    #[test]
    fn sequence() {
        failure("return 1 1;");
    }
}
