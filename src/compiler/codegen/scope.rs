use crate::bytecode::chunk::OpCode;

#[derive(Copy, Clone)]
pub enum Scope {
    Local,
    Upvalue,
    Global,
}

impl Scope {
    pub fn set_opcode(self) -> OpCode {
        match self {
            Self::Local => OpCode::SetLocal,
            Self::Upvalue => OpCode::SetUpvalue,
            Self::Global => OpCode::SetGlobal,
        }
    }

    pub fn get_opcode(self) -> OpCode {
        match self {
            Self::Local => OpCode::GetLocal,
            Self::Upvalue => OpCode::GetUpvalue,
            Self::Global => OpCode::GetGlobal,
        }
    }
}
