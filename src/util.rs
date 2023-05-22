use std::sync::Once;
use tracing::Level;

#[macro_export]
macro_rules! noop {
    ($($tt:tt)*) => {};
}

pub fn setup_test() {
    static LOGGING: Once = Once::new();
    LOGGING.call_once(|| {
        tracing_subscriber::fmt()
            .with_max_level(Level::TRACE)
            .init();
    })
}

#[cfg(test)]
pub fn mock_interpret(source: &str) -> String {
    let mut stderr = vec![];
    let mut stdout = vec![];
    crate::interpret(source, &mut stderr, &mut stdout);
    let stderr = String::from_utf8(strip_ansi_escapes::strip(stderr).unwrap()).unwrap();
    let stdout = String::from_utf8(strip_ansi_escapes::strip(stdout).unwrap()).unwrap();
    format!("stdout:\n{stdout}\n\nstderr:\n{stderr}\n")
}

#[cfg(test)]
#[macro_export]
macro_rules! snap {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            ::insta::assert_display_snapshot!($crate::util::mock_interpret($input));
        }
    };
}
