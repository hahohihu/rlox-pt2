use num_enum::FromPrimitive;

use crate::{value::Value, ui::Span};

#[derive(Debug, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return, // 0 follow bytes
    Constant, // 1 follow byte for a constant index
    Negate, // 0 follow bytes
    #[num_enum(default)]
    Invalid,
}

#[derive(Default, Debug, Clone)]
pub struct Chunk {
    // INVARIANT: An OpCode must be followed by however many bytes are specified
    pub instructions: Vec<u8>,
    pub spans: Vec<Span>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
    
    pub fn disassemble(&self, name: &str, source: &str) {
        println!("==== {name} ====");
        let mut i = 0;
        while i < self.instructions.len() {
            i = self.disassemble_instruction(i, source);
        }
    }

    /// SAFETY: OpCode invariants must be upheld. If an opcode is n bytes, n bytes _must_ be inserted
    pub unsafe fn write_byte(&mut self, byte: u8, origin: Span) {
        self.instructions.push(byte);
        self.spans.push(origin);
    }

    fn simple_instruction(name: &str, offset: &mut usize) {
        println!("{name}");
        *offset += 1;
    }

    fn constant_instruction(&self, name: &str, offset: &mut usize) {
        let index = self.instructions[*offset + 1];
        let value = self.constants[index as usize];
        println!("{name:<16} {index:>4} '{value}'");
        *offset += 2;
    }

    pub fn disassemble_instruction(&self, mut offset: usize, source: &str) -> usize {
        print!("{:0>4} ", offset);
        if offset > 0 && self.spans[offset] == self.spans[offset - 1] {
            print!("{:<16}", "|");
        } else {
            print!("{:<16}", &source[self.spans[offset]]);
        }

        let chunk = self.instructions[offset];
        let instruction: OpCode = chunk.into();
        match instruction {
            OpCode::Return => Chunk::simple_instruction("RETURN", &mut offset),
            OpCode::Constant => self.constant_instruction("CONSTANT", &mut offset),
            OpCode::Negate => Chunk::simple_instruction("NEGATE", &mut offset),
            OpCode::Invalid => {
                println!("INVALID OPCODE: {chunk}");
                offset += 1;
            },
        }
        offset
    }
}
