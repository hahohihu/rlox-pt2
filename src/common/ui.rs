use std::{fmt::Display, ops::Index};

use arbitrary::Arbitrary;
use tracing::warn;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Arbitrary)]
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
        debug_assert!(!spans.is_empty());
        if let Some(span) = spans.get(0) {
            spans[1..].iter().fold(*span, |a, b| a.unite(*b))
        } else {
            warn!("Empty set of spans should never happen");
            Span::from(0..0)
        }
    }

    pub fn maybe_unite<T>(self, other: Option<Spanned<T>>) -> Span {
        if let Some(span) = other {
            self.unite(span.span)
        } else {
            self
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

#[derive(Clone, Debug, PartialEq, Eq, Arbitrary)]
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

    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

pub trait WithSpanExt: Sized {
    fn with_span(self, span: Span) -> Spanned<Self>;
}

impl<T> WithSpanExt for T {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned { data: self, span }
    }
}

pub const OFFSET: usize = 12;
