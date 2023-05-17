use num_enum::FromPrimitive;

use crate::value::Value;

#[derive(Debug, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return, // 0 follow bytes
    Constant, // 1 follow byte for a constant index
    #[num_enum(default)]
    Invalid,
}

#[derive(Default, Debug, Clone)]
pub struct Chunk {
    // INVARIANT: An OpCode must be followed by however many bytes are specified
    instructions: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
    
    pub fn disassemble(&self, name: &str) {
        println!("==== {name} ====");
        let mut i = 0;
        while i < self.instructions.len() {
            self.disassemble_instruction(&mut i);
        }
    }

    /// SAFETY: OpCode invariants must be upheld. If an opcode is n bytes, n bytes _must_ be inserted
    pub unsafe fn write_byte(&mut self, byte: u8) {
        self.instructions.push(byte)
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

    fn disassemble_instruction(&self, offset: &mut usize) {
        print!("{:0>4} ", *offset);

        let chunk = self.instructions[*offset];
        let instruction: OpCode = chunk.into();
        match instruction {
            OpCode::Return => Chunk::simple_instruction("RETURN", offset),
            OpCode::Constant => self.constant_instruction("CONSTANT", offset),
            OpCode::Invalid => {
                println!("INVALID OPCODE: {chunk}");
                *offset += 1;
            },
        }
    }
}
