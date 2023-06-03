#![allow(clippy::redundant_pattern_matching)]
#![deny(unused_must_use)]
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

// These tests being all glommed together is a bit terrifying, but snapshot results are bound to location, so refactoring isn't worth it
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
    use crate::{snap, snap_success};
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
    snap_success! {precedence, "print 1 * 2 == 4 / 2;"}

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
    snap!(inequality, "print 1 != 1; print 0 != 1;");
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

    snap!(
        assignment,
        "
        var a = 0;
        a = \"a\";
        print a;
        "
    );
    snap!(
        invalid_lvalue,
        "
        a * b = c + d;
        "
    );
    snap!(
        nested_assign,
        "
        var a;
        var b = a = 1;
        print a;
        print b;
        "
    );
    snap!(
        assign_parens,
        "
        var a;
        print 1 * (a = 2);  
        "
    );
    snap!(
        assign_a_string,
        "
        var s = \"foo\";
        s = s + \"bar\";
        print s;
        "
    );
    snap! {
        print_assign,
        "
        var s;
        print s = \"s\";
        "
    }

    snap! {
        basic_scope,
        "
        {
            var a = 1;
            print a;
        }
        "
    }

    snap! {
        nested_scope,
        "
        {
            var a = 1;
            {
                var b = 2;
                print a;
                print b;
            }
            print a;
        }
        "
    }

    snap! {
        inaccessible_scope,
        "
        {
            var a = 1;
        }
        print a;
        "
    }

    snap! {
        global_and_local_scope,
        "
        var a = 1;
        var b = 2;
        {
            print a;
            print b;
            var a = 3;
            print a;
        }
        print a;
        "
    }

    snap! {
        // I diverge from the book and allow this because it's fine and useful
        // though somewhat inefficient since it takes up extra stack space
        same_scope_shadowing,
        "
        {
            var a = 1;
            print a;
            var a = 2;
            print a;
        }
        "
    }

    snap! {
        scoped_assignment,
        "
        var a;
        {
            print a;
            a = 1;
            print a;
        }
        print a;
        "
    }

    snap! {
        multi_local_assign,
        "
        {
            var a;
            var b;
            a = b = a = 1;
            print a;
            print b;
        }
        "
    }

    snap! {
        invalid_declaration,
        "
        var a = var b;
        "
    }

    snap! {
        // todo: this error message could be better
        unterminated_scope,
        "
        {
            var a = 1;
            print a;
        "
    }

    snap! {
        global_declaration_without_identifier,
        "
        var 1;
        "
    }

    snap! {
        local_declaration_without_identifier,
        "
        {
            var 1;
        }
        "
    }

    snap! {
        set_undeclared_global,
        "
        a = 1;
        "
    }

    snap! {
        eof_after_variable,
        "
        var a
        "
    }

    snap! {invalid_expr_in_parens, "(1 1"}
    snap! {
        not_a_number,
        "
        var n = 0 / 0;
        print n == n;
        "
    }
    snap! {
        equality_usually_reflexive,
        r#"
        var n = 0;
        print n == n;
        n = "foo";
        print n == n;
        var m = "foo";
        print n == m;
        n = true;
        print n == n;
        n = nil;
        print n == n;
        "#
    }

    snap! {
        shadow_and_reuse,
        "
        var a = 0;
        {
            var a = a;
            print a;
        }
        print a;
        "
    }

    snap! {
        cannot_use_global_var_in_declaration,
        "
        var a = a;
        "
    }

    snap! {
        cannot_use_local_var_in_declaration,
        "
        {
            var a = a;
        }
        "
    }

    snap! {
        invalid_place,
        "
        1 = 1;
        "
    }

    snap! {
        basic_if,
        r#"
        if true {
            print "TRUE";
        }
        if false {
            print "FALSE";
        }
        "#
    }

    snap! {
        nested_if,
        "
        if true {
            if true {
                if true {
                    print 1;
                }
                print 2;
            }
            print 3;
        }
        print 4;
        "
    }

    snap! {
        if_is_scope,
        "
        var a;
        if true {
            var a = 1;
            print a;
        }
        print a;
        "
    }

    snap! {
        parens_ok,
        "
        if (true) {
            print 1;
        }
        "
    }

    snap! {
        brackets_mandatory,
        "
        if (true) print 1;
        "
    }

    snap! {
        if_sequence,
        "
        if false {
            print 1;
            print 2;
            print 3;
            print 4;
            print 5;
            print 6;
        }
        if true {
            print 1;
            print 2;
        }
        "
    }

    snap! {
        invalid_if_cond,
        "
        if print a; {
            print 1;
        }
        "
    }

    snap! {
        invalid_block,
        "
        if true {
            print 1
        }
        "
    }

    snap! {
        basic_else,
        "
        if false {
            print 1;
        } else {
            print 2;
        }
        "
    }

    snap! {
        basic_then,
        "
        if true {
            print 1;
        } else {
            print 2;
        }
        "
    }

    snap! {
        assignment_in_condition,
        "
        var a = true;
        if a = false {
            print 1;
        } else {
            print 2;
        }
        "
    }

    snap! {
        assignment_in_parens,
        "
        var a;
        a = 1 + (a = 1);
        print a;
        "
    }

    snap! {
        nested_if_else,
        "
        print 0;
        if true {
            if false {
                print 1;
            } else {
                print 2;
            }
        } else {
            if true {
                print 3;
            } else {
                print 4;
            }
        }
        print 5;
        "
    }

    snap! {
        and_condition,
        "
        var a;
        if true and (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap! {
        and_short_circuit,
        "
        var a;
        if false and (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap! {
        or_condition,
        "
        var a;
        if false or (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap! {
        or_short_circuit,
        "
        var a;
        if true or (a = 1) {
            print a;
        }
        "
    }

    snap! {
        basic_while_loop,
        "
        var a = 0;
        while a < 10 {
            a = a + 1;
            print a;
        }
        print a;
        "
    }

    snap! {
        nested_while_loops,
        "
        var a = 0;
        while a < 3 {
            var b = 3;
            while b > 0 {
                print b;
                b = b - 1;
            }
            print a;
            a = a + 1;
        }
        "
    }

    snap! {
        basic_for_loop,
        "
        for var a = 0; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap! {
        bigger_scope_for_loop,
        "
        var a;
        for a = 0; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap! {
        empty_initializer_for_loop,
        "
        var a = 0;
        for ; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap! {
        empty_update_for_loop,
        "
        for var a = 0; a < 3; {
            print a;
            a = a + 1;
        }
        "
    }

    snap! {
        nested_for_loops,
        "
        for var a = 0; a < 3; a = a + 1 {
            for var b = 1; b < 3; b = b * 2 {
                print b;
            }
        }
        "
    }

    snap! {
        for_loop_is_scoped,
        "
        for var a = 0; a < 3; a = a + 1 {}
        print a;
        "
    }

    snap! {
        function_declaration,
        "
        fun foo() {}

        print foo;
        "
    }

    snap! {
        basic_function_call,
        r#"
        fun hello_world() {
            print "Hello, World!"; 
        }
        
        hello_world();
        "#
    }

    snap! {
        function_call_args,
        "
        fun show(n) {
            print n;
        }

        show(1);
        "
    }

    snap! {
        many_args_function,
        "
        fun show(a, b, c) {
            print a;
            print b;
            print c;
        }

        show(1, 2, 3);
        "
    }

    snap! {
        scoped_function,
        r#"
        {
            fun foo() {
                print "foo";
            }
            foo();
        }
        "#
    }

    snap! {
        scoped_function_args,
        r#"
        {
            fun show(a, b) {
                print a;
                print b;
            }
            show(1, 2);
        }
        "#
    }

    snap! {
        function_out_of_scope,
        "
        {
            fun foo() {}
        }
        foo();
        "
    }

    snap! {
        nested_function,
        "
        fun foo() {
            fun bar() {
                print 1;
            }
            bar();
        }
        foo();
        "
    }

    snap! {
        early_return,
        "
        fun foo() {
            print 1;
            return;
            print 2;
        }
        foo();
        "
    }

    snap! {
        return_value,
        "
        fun foo() {
            return 1;
        }

        print foo();
        "
    }

    snap! {
        return_function,
        "
        fun foo() {
            fun bar() {
                return 1;
            }
            return bar;
        }

        print foo()();
        "
    }

    snap! {
        return_function_with_args,
        "
        fun foo() {
            fun bar(a) {
                return a;
            }
            return bar;
        }

        print foo()(1);
        "
    }

    snap! {
        functions_and_global_state,
        "
        var cond = true;
        fun foo() {
            fun id(a) {
                return a;
            }
            fun show(a) {
                print a;
            }
            if cond {
                return show;
            } else {
                return id;
            }
        }

        print foo()(2);
        print 3;
        cond = false;
        print foo()(1);
        "
    }

    snap! {
        non_returning_end,
        "
        fun foo() {
            if false {
                return 1;
            }
        }

        print foo();
        "
    }

    snap! { // intentionally deviates - no harm in allowing this
        top_level_return,
        "
        return 1;
        "
    }

    snap! {
        recursive_global,
        "
        fun add(a, b) {
            if a == 0 {
                return b;
            } else {
                return add(a - 1, b + 1);
            }
        }

        print add(2, 1);
        "
    }
}
