use std::process::Command;
use assert_cmd::prelude::CommandCargoExt;

#[test]
fn file_not_found() {
    let output = Command::cargo_bin("rlox").unwrap().output().unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    insta::assert_yaml_snapshot!(stderr);
}
