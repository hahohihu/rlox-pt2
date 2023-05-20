use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{ui::Span, value::Value};

#[derive(Debug, Eq, PartialEq, FromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // 0 follow bytes ====
    Return,
    Nil,
    True,
    False,
    // 1 follow bytes ====
    Constant, // 1: a constant index
    // No follow bytes but data-dependent
    // Unary
    Negate,
    Not,
    Print,
    // Binary
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Greater,
    Less,
    #[num_enum(default)]
    Invalid,
}

#[derive(Default, Debug, Clone)]
pub struct Chunk {
    // INVARIANT: An OpCode must be followed by however many bytes are specified
    pub instructions: Vec<u8>,
    pub spans: Vec<Span>,
    // SAFETY INVARIANT: All object values are valid, and there are no duplicate allocations
    constants: Vec<Value>,
}

impl Drop for Chunk {
    fn drop(&mut self) {
        for constant in &self.constants {
            if let Value::Object(obj) = constant {
                unsafe {
                    obj.free();
                }
            }
        }
    }
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        let index = self.constants.len() - 1;
        index.try_into().expect("Too many constants")
    }

    pub fn get_constant(&mut self, index: u8) -> Value {
        self.constants[index as usize]
    }

    pub fn disassemble(&self, name: &str, source: &str) {
        println!("==== {name} ====");
        let mut i = 0;
        while i < self.instructions.len() {
            i = self.disassemble_instruction(i, source);
        }
    }

    /// SAFETY: OpCode invariants must be upheld. If an opcode is n bytes, n bytes _must_ be inserted
    pub unsafe fn write_byte(&mut self, byte: impl Into<u8>, origin: Span) {
        self.instructions.push(byte.into());
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
            print!("{:<8}", "|");
        } else {
            print!("{:<8}", &source[self.spans[offset]]);
        }

        let chunk = self.instructions[offset];
        let instruction: OpCode = chunk.into();
        match instruction {
            OpCode::Return => Chunk::simple_instruction("RETURN", &mut offset),
            OpCode::Constant => self.constant_instruction("CONSTANT", &mut offset),
            OpCode::Negate => Chunk::simple_instruction("NEGATE", &mut offset),
            OpCode::Add => Chunk::simple_instruction("ADD", &mut offset),
            OpCode::Sub => Chunk::simple_instruction("SUBTRACT", &mut offset),
            OpCode::Mul => Chunk::simple_instruction("MULTIPLY", &mut offset),
            OpCode::Div => Chunk::simple_instruction("DIVIDE", &mut offset),
            OpCode::Nil => Chunk::simple_instruction("NIL", &mut offset),
            OpCode::Not => Chunk::simple_instruction("NOT", &mut offset),
            OpCode::True => Chunk::simple_instruction("TRUE", &mut offset),
            OpCode::False => Chunk::simple_instruction("FALSE", &mut offset),
            OpCode::Equal => Chunk::simple_instruction("EQUAL", &mut offset),
            OpCode::Greater => Chunk::simple_instruction("GREATER", &mut offset),
            OpCode::Less => Chunk::simple_instruction("LESS", &mut offset),
            OpCode::Print => Chunk::simple_instruction("PRINT", &mut offset),
            OpCode::Invalid => {
                println!("INVALID OPCODE: {chunk}");
                offset += 1;
            }
        }
        offset
    }
}
