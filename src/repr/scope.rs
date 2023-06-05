use super::chunk::OpCode;

#[derive(Copy, Clone)]
pub enum Scope {
    Local,
    Global,
}

impl Scope {
    pub fn set_opcode(self) -> OpCode {
        match self {
            Self::Local => OpCode::SetLocal,
            Self::Global => OpCode::SetGlobal,
        }
    }

    pub fn get_opcode(self) -> OpCode {
        match self {
            Self::Local => OpCode::GetLocal,
            Self::Global => OpCode::GetGlobal,
        }
    }
}
