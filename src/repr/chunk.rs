use std::io::Write;

use num_enum::{FromPrimitive, IntoPrimitive};

use super::{interner::Interner, value::Value};
use crate::common::ui::Span;

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
    Pop,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
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
    // Owned by this
    constants: Vec<Value>,
    // Owned by this
    pub globals: Interner,
}

impl Drop for Chunk {
    fn drop(&mut self) {
        for constant in &self.constants {
            if let Value::Object(obj) = constant {
                unsafe {
                    // SAFETY: See safety invariant on constants
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

    pub fn get_constant(&self, index: u8) -> Value {
        self.constants[index as usize]
    }

    /// SAFETY: OpCode invariants must be upheld. If an opcode is n bytes, n bytes _must_ be inserted
    pub unsafe fn write_byte(&mut self, byte: impl Into<u8>, origin: Span) {
        self.instructions.push(byte.into());
        self.spans.push(origin);
    }

    pub fn disassemble(&self, name: &str, source: &str, mut stdout: impl Write) {
        writeln!(stdout, "==== {name} ====").unwrap();
        let mut i = 0;
        while i < self.instructions.len() {
            i = self.disassemble_instruction(i, source, &mut stdout);
        }
    }

    fn simple_instruction(name: &str, offset: &mut usize, mut stdout: impl Write) {
        writeln!(stdout, "{name}").unwrap();
        *offset += 1;
    }

    fn constant_instruction(&self, name: &str, offset: &mut usize, mut stdout: impl Write) {
        let index = self.instructions[*offset + 1];
        let value = self.constants[index as usize];
        writeln!(stdout, "{name:<16} {index:>4} '{value}'").unwrap();
        *offset += 2;
    }

    fn global_instruction(&self, name: &str, offset: &mut usize, mut stdout: impl Write) {
        let index = self.instructions[*offset + 1];
        let value = self.globals.get_name(index);
        writeln!(stdout, "{name:<16} {index:>4} '{value}'").unwrap();
        *offset += 2;
    }

    pub fn disassemble_instruction(
        &self,
        mut offset: usize,
        source: &str,
        mut stdout: impl Write,
    ) -> usize {
        write!(stdout, "{:0>4} ", offset).unwrap();
        if offset > 0 && self.spans[offset] == self.spans[offset - 1] {
            write!(stdout, "{:<8}", "|").unwrap();
        } else {
            write!(stdout, "{:<8}", &source[self.spans[offset]]).unwrap();
        }

        let chunk = self.instructions[offset];
        let instruction: OpCode = chunk.into();
        let mut simple = |str| Chunk::simple_instruction(str, &mut offset, &mut stdout);
        match instruction {
            OpCode::Return => simple("RETURN"),
            OpCode::Constant => self.constant_instruction("CONSTANT", &mut offset, stdout),
            OpCode::Negate => simple("NEGATE"),
            OpCode::Add => simple("ADD"),
            OpCode::Sub => simple("SUBTRACT"),
            OpCode::Mul => simple("MULTIPLY"),
            OpCode::Div => simple("DIVIDE"),
            OpCode::Nil => simple("NIL"),
            OpCode::Not => simple("NOT"),
            OpCode::True => simple("TRUE"),
            OpCode::False => simple("FALSE"),
            OpCode::Equal => simple("EQUAL"),
            OpCode::Greater => simple("GREATER"),
            OpCode::Less => simple("LESS"),
            OpCode::Print => simple("PRINT"),
            OpCode::Pop => simple("POP"),
            OpCode::DefineGlobal => self.global_instruction("DEFINE_GLOBAL", &mut offset, stdout),
            OpCode::GetGlobal => self.global_instruction("GET_GLOBAL", &mut offset, stdout),
            OpCode::SetGlobal => self.global_instruction("SET_GLOBAL", &mut offset, stdout),
            OpCode::Invalid => {
                writeln!(stdout, "INVALID OPCODE: {chunk}").unwrap();
                offset += 1;
            }
        }
        offset
    }
}
