use std::ops::{Index, Add};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
    begin: u32,
    end: u32,
}

impl From<logos::Span> for Span {
    fn from(value: logos::Span) -> Self {
        Self {
            begin: value.start.try_into().unwrap(),
            end: value.end.try_into().unwrap(),
        }
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[index.begin as usize..index.end as usize]
    }
}

impl Span {
    pub fn unite(self, other: Span) -> Span {
        Span {
            begin: self.begin.min(other.begin),
            end: self.end.max(other.end),
        }
    }

    pub fn unite_many(spans: &[Span]) -> Span {
        debug_assert!(spans.len() > 0);
        if spans.len() > 1 {
            let init = spans[0];
            spans[1..].into_iter().fold(init, |a, b| a.unite(*b))
        } else {
            spans[0]
        }
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

impl<T> From<Spanned<T>> for Span {
    fn from(value: Spanned<T>) -> Self {
        value.span
    }
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T> Spanned<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}

pub const OFFSET: usize = 12;
