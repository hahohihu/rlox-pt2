use crate::{chunk::{Chunk, OpCode}, value::Value};

struct VM<'src> {
    chunk: Chunk,
    ip: usize,
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
            }
            let instruction: OpCode = self.next_byte().into();
            match instruction {
                OpCode::Return => {
                    return InterpretResult::Ok;
                },
                OpCode::Constant => {
                    println!("{}", self.read_constant());
                },
                OpCode::Invalid => unreachable!("Reached invalid opcode at {}", self.ip),
            }
        }
    }
}

pub fn interpret(chunk: Chunk, source: &str) -> InterpretResult {
    let mut vm = VM::new(chunk, source);
    vm.run()
}