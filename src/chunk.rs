use num_enum::FromPrimitive;

#[derive(Debug, Eq, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
    #[num_enum(default)]
    Invalid,
}

#[derive(Default, Debug, Clone)]
pub struct Chunk(Vec<u8>);

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn disassemble(&self, name: &str) {
        println!("==== {name} ====");
        let mut i = 0;
        while i < self.0.len() {
            self.disassemble_instruction(&mut i);
        }
    }

    /// SAFETY: OpCode invariants must be upheld. If an opcode is n bytes, n bytes _must_ be inserted
    pub unsafe fn write_byte(&mut self, byte: u8) {
        self.0.push(byte)
    }

    fn simple_instruction(name: &str, offset: &mut usize) {
        println!("{name}");
        *offset += 1;
    }

    fn disassemble_instruction(&self, offset: &mut usize) {
        print!("{:0>4} ", *offset);

        let chunk = self.0[*offset];
        let instruction: OpCode = chunk.into();
        match instruction {
            OpCode::Return => Self::simple_instruction("RETURN", offset),
            OpCode::Invalid => {
                println!("INVALID OPCODE: {chunk}");
                *offset += 1;
            },
        }
    }
}
