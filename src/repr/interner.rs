use super::string::UnsafeString;
use std::collections::HashMap;

pub type InternedU8 = u8;
#[derive(Default, Debug, Clone)]
pub struct Interner {
    names: Vec<UnsafeString>,
    indices: HashMap<UnsafeString, InternedU8>,
}

impl Drop for Interner {
    fn drop(&mut self) {
        for name in &self.names {
            unsafe { name.free() }
        }
    }
}

impl Interner {
    pub fn add_or_get(&mut self, literal: &str) -> InternedU8 {
        // Getting an entry requires ownership, which is more expensive in the common-case of finding a duplicate string
        if let Some(i) = self.indices.get(literal) {
            *i
        } else {
            let literal = UnsafeString::from(literal);
            self.names.push(literal);
            let index = (self.names.len() - 1)
                .try_into()
                .expect("More interned strings than is supported");
            self.indices.insert(literal, index);
            index
        }
    }

    pub fn get_name(&self, index: InternedU8) -> UnsafeString {
        self.names[index as usize]
    }
}
