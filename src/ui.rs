use std::{slice::SliceIndex, ops::Index};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
    begin: u32,
    end: u32,
}

impl From<logos::Span> for Span {
    fn from(value: logos::Span) -> Self {
        Self {
            begin: value.start.try_into().unwrap(),
            end: value.end.try_into().unwrap()
        }
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[index.begin as usize..index.end as usize]
    }
}

impl ariadne::Span for Span {
    type SourceId = ();
    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.begin as usize
    }

    fn end(&self) -> usize {
        self.end as usize
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T> Spanned<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}