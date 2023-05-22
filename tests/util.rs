#[macro_export]
macro_rules! snap_input {
    ($name:ident, $code:literal) => {
        #[test]
        fn $name() -> ::std::result::Result<(), Box<dyn ::std::error::Error>> {
            use ::assert_cmd::prelude::CommandCargoExt;
            use ::assert_fs::prelude::FileWriteStr;
            let file = ::assert_fs::NamedTempFile::new(stringify!($name))?;
            file.write_str($code)?;

            let output = ::std::process::Command::cargo_bin("rlox")?.arg(file.path()).output()?;
            let stdout = ::std::string::String::from_utf8(output.stdout).unwrap();
            let stderr = ::std::string::String::from_utf8(output.stderr).unwrap();
            let merged = ::std::format!("stdout:\n{stdout}\n\nstderr:\n{stderr}\n\n");
            let merged = ::std::string::String::from_utf8(::strip_ansi_escapes::strip(&merged)?)?;
            ::insta::assert_display_snapshot!(merged);
            Ok(())
        }
    };
}
