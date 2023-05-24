#![allow(clippy::redundant_pattern_matching)]
use std::{
    env::args,
    fs::File,
    io::{stderr, stdout, Read},
    process::ExitCode,
};

use vm::interpret;

mod common;
mod compiler;
mod repr;
mod vm;

fn read_file(filename: &str) -> std::io::Result<String> {
    let mut file = File::open(filename)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    Ok(source)
}

fn main() -> ExitCode {
    tracing_subscriber::fmt::init();
    let mut args = args();
    args.next();
    let Some(filename) = args.next() else {
        eprintln!("Usage: rlox <filename>");
        return ExitCode::FAILURE;
    };
    let source = match read_file(&filename) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Failed to read {filename} with error: {:?}", e);
            return ExitCode::FAILURE;
        }
    };
    match interpret(&source, stderr(), stdout()) {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}

#[cfg(test)]
mod test_errors {
    use crate::snap;
    snap!(missing_op, "print 1 1;");
    snap!(missing_primary, "print ();\n");
    snap!(missing_parens, "print ((1);\n");
    snap!(rparens, "print 1);\n");
    snap!(missing_rhs, "print 1 + ;\n");
    snap!(missing_lhs, "print + 1;\n");
    snap!(invalid_token, "print $;");
    snap!(remaining_tokens, "print 1; x");
    snap!(floating_expr, "1;");
}

#[cfg(test)]
mod test_runtime {
    use crate::snap;
    snap!(mismatched_add, "print true + 1;");
    snap!(mismatched_sub, "print true - 1;");
    snap!(negate_bool, "print -true;");
    snap!(one, "print 1;");
    snap!(point_one, "print 0.1;");
    snap!(print_false, "print false;");
    snap!(print_nil, "print nil;");
    snap!(add_mul, "print 1 + 2 * 3;");
    snap!(mul_div, "print 6 * 6 / 3;");
    snap!(complex_arithmetic, "print 20 * 5 / 0.5 - 100.0;");
    snap!(div_0, "print 1 / 0;");
    snap!(parens, "print 2 * (6 + 1) / (2) -- 100;");
    snap!(nested_parens, "print ((1) / (1 + (1 / 0.5)) * 3);");
    snap!(unary, "print -1 - -2 == --1 == true;");
    snap!(lots_of_negs, "print ---------------------------------------------------------------------------------------------------------1;");

    snap!(
        falsey,
        "
        print !nil;
        print !false;
        print !0;
        print !true;
        print !\"\";
    "
    );

    snap!(less_than, "print 1 < 1; print 0 < 1; print 2 < 1;");
    snap!(less_equal, "print 1 <= 1; print 0 <= 1; print 2 <= 1;");
    snap!(equality, "print 1 == 1; print 0 == 1; print 2 == 1;");
    snap!(greater_than, "print 1 > 1; print 0 > 1; print 2 > 1;");
    snap!(greater_equal, "print 1 >= 1; print 0 >= 1; print 2 >= 1;");

    snap!(string, r#"print "foo";"#);
    snap!(concatenation, r#"print "foo" + "bar";"#);
    snap!(string_comparison, r#"print "foo" == "foo";"#);
    snap!(
        compound_string_comparison,
        r#"print "foo" + "bar" == "f" + "oo" + "bar";"#
    );
    snap!(
        unicode,
        r#"print "💩" + "👪" + "༕" + "갍" + "⑯" + "ฒ" + "ڦ";"#
    );
    snap!(
        globals,
        "
        var answer = 42;
        print answer;
        "
    );
    snap!(
        global_strings,
        r#"
        var foo = "foo";
        var bar = "bar";
        print foo + bar;
        "#
    );
    snap!(
        uninitialized_is_nil,
        "
        var foo;
        print foo;
        "
    );
    snap!(missing_global, "var bar; print foo;");
    snap!(
        shadowing,
        "
        var a = 1;
        var a;
        print a;
        "
    );
    snap!(declaration_is_not_expression, "var a == 1;");
}
