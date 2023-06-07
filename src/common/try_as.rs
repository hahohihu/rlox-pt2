// TryFrom doesn't work well transitively because of orphan rules, plus Result is overkill
pub trait TryAs<To>: Sized {
    fn try_as(self) -> Option<To>;

    fn unwrap_as(self) -> To {
        self.try_as().unwrap()
    }
}

impl<T> TryAs<T> for T {
    fn try_as(self) -> Option<Self> {
        Some(self)
    }
}

pub trait TryCast<From>: Sized {
    fn try_cast(value: From) -> Option<Self>;
    fn unwrap_cast(value: From) -> Self {
        Self::try_cast(value).unwrap()
    }
}

impl<From, To> TryCast<From> for To
where
    From: TryAs<To>,
{
    fn try_cast(value: From) -> Option<Self> {
        value.try_as()
    }
}
