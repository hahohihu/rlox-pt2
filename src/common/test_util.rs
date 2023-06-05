pub fn setup_test() {
    use std::sync::Once;
    use tracing::Level;
    static LOGGING: Once = Once::new();
    LOGGING.call_once(|| {
        tracing_subscriber::fmt()
            .with_max_level(Level::TRACE)
            .init();
    })
}

#[cfg(feature = "snap")]
pub use ::insta::assert_snapshot;
#[cfg(not(feature = "snap"))]
pub use black_box as assert_snapshot;

pub fn mock_interpret(source: &str) -> String {
    setup_test();
    let mut stderr = vec![];
    let mut stdout = vec![];
    let _ = crate::interpret(source, &mut stderr, &mut stdout);
    let stderr = String::from_utf8(strip_ansi_escapes::strip(stderr).unwrap()).unwrap();
    let stdout = String::from_utf8(strip_ansi_escapes::strip(stdout).unwrap()).unwrap();
    format!("stdout:\n{stdout}\n\nstderr:\n{stderr}\n")
}

#[macro_export]
macro_rules! snap_interpret {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            $crate::common::test_util::assert_snapshot!($crate::common::test_util::mock_interpret(
                $input
            ));
        }
    };
}

pub fn mock_parse(source: &str) -> String {
    let mut stderr = vec![];
    let ast = crate::compiler::parse(source, &mut stderr);
    let stderr = String::from_utf8(strip_ansi_escapes::strip(stderr).unwrap()).unwrap();
    if let Some(ast) = ast {
        format!("ast:\n{}\n\n{stderr}", ast)
    } else {
        format!("stderr:\n{stderr}\n")
    }
}

#[macro_export]
macro_rules! snap_parse {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            $crate::common::test_util::assert_snapshot!($crate::common::test_util::mock_parse(
                $input
            ));
        }
    };
}

pub fn mock_codegen(source: &str) -> String {
    let mut stderr = vec![];
    let chunk = crate::compiler::compile(source, &mut stderr);
    let stderr = String::from_utf8(strip_ansi_escapes::strip(stderr).unwrap()).unwrap();
    if let Some(chunk) = chunk {
        let mut bytecode = vec![];
        chunk.disassemble("test.lox", source, &mut bytecode);
        let stdout = String::from_utf8(strip_ansi_escapes::strip(bytecode).unwrap()).unwrap();
        format!("bytecode:\n{stdout}\n\n{stderr}")
    } else {
        format!("stderr:\n{stderr}\n")
    }
}

#[macro_export]
macro_rules! snap_codegen {
    ($name:ident, $input:literal) => {
        #[test]
        fn $name() {
            $crate::common::test_util::assert_snapshot!($crate::common::test_util::mock_codegen(
                $input
            ));
        }
    };
}

#[macro_export]
macro_rules! snap_all {
    ($name:ident, $input:literal) => {
        paste::paste! {
            $crate::snap_parse!([<parse_ $name>], $input);
            $crate::snap_codegen!([<codegen_ $name>], $input);
            $crate::snap_interpret!([<interpret_ $name>], $input);
        }
    };
}
