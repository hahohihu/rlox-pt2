use crate::{chunk::{Chunk, OpCode}, value::Value};

struct VM<'src> {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    source: &'src str
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

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

    fn read_constant(&mut self) -> Value {
        let i = self.next_byte() as usize;
        self.chunk.constants[i]
    }

    fn run(&mut self) -> InterpretResult {
        if self.chunk.instructions.is_empty() {
            return InterpretResult::Ok;
        }
        loop {
            #[cfg(debug_assertions)]
            {
                self.chunk.disassemble_instruction(self.ip, self.source);
                println!("==== STACK ====");
                let stack_len = self.stack.len().saturating_sub(8);
                for value in &self.stack[stack_len..] {
                    println!("{value}");
                }
                println!("===============");
            }
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    println!("returning: {}", self.stack.pop().unwrap());
                    return InterpretResult::Ok;
                },
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                },
                OpCode::Negate => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(-val);
                }
                OpCode::Invalid => unreachable!("Reached invalid opcode at {}", self.ip),
            }
        }
    }
}

pub fn interpret(chunk: Chunk, source: &str) -> InterpretResult {
    let mut vm = VM::new(chunk, source);
    vm.run()
}