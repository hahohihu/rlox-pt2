#[macro_export]
macro_rules! noop {
    ($($tt:tt)*) => {};
}

#[macro_export]
macro_rules! black_box {
    ($expr:expr) => {
        ::std::hint::black_box($expr);
    };
}
