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

#[cfg(test)]
mod test_runtime {
    use crate::{snap_all, snap_interpret};
    snap_interpret!(floating_expr, "1;");
    snap_interpret!(mismatched_add, "print true + 1;");
    snap_interpret!(mismatched_sub, "print true - 1;");
    snap_interpret!(negate_bool, "print -true;");
    snap_interpret!(one, "print 1;");
    snap_interpret!(point_one, "print 0.1;");
    snap_interpret!(print_false, "print false;");
    snap_interpret!(print_nil, "print nil;");
    snap_interpret!(add_mul, "print 1 + 2 * 3;");
    snap_interpret!(mul_div, "print 6 * 6 / 3;");
    snap_interpret!(complex_arithmetic, "print 20 * 5 / 0.5 - 100.0;");
    snap_interpret!(div_0, "print 1 / 0;");
    snap_interpret!(parens, "print 2 * (6 + 1) / (2) -- 100;");
    snap_interpret!(nested_parens, "print ((1) / (1 + (1 / 0.5)) * 3);");
    snap_interpret!(unary, "print -1 - -2 == --1 == true;");
    snap_interpret!(lots_of_negs, "print ---------------------------------------------------------------------------------------------------------1;");
    snap_interpret! {precedence, "print 1 * 2 == 4 / 2;"}

    snap_interpret!(
        falsey,
        "
        print !nil;
        print !false;
        print !0;
        print !true;
        print !\"\";
    "
    );

    snap_interpret!(less_than, "print 1 < 1; print 0 < 1; print 2 < 1;");
    snap_interpret!(less_equal, "print 1 <= 1; print 0 <= 1; print 2 <= 1;");
    snap_interpret!(equality, "print 1 == 1; print 0 == 1; print 2 == 1;");
    snap_interpret!(inequality, "print 1 != 1; print 0 != 1;");
    snap_interpret!(greater_than, "print 1 > 1; print 0 > 1; print 2 > 1;");
    snap_interpret!(greater_equal, "print 1 >= 1; print 0 >= 1; print 2 >= 1;");

    snap_interpret!(string, r#"print "foo";"#);
    snap_interpret!(concatenation, r#"print "foo" + "bar";"#);
    snap_interpret!(string_comparison, r#"print "foo" == "foo";"#);
    snap_interpret!(
        compound_string_comparison,
        r#"print "foo" + "bar" == "f" + "oo" + "bar";"#
    );
    snap_interpret!(
        unicode,
        r#"print "💩" + "👪" + "༕" + "갍" + "⑯" + "ฒ" + "ڦ";"#
    );
    snap_interpret!(
        globals,
        "
        var answer = 42;
        print answer;
        "
    );
    snap_interpret!(
        global_strings,
        r#"
        var foo = "foo";
        var bar = "bar";
        print foo + bar;
        "#
    );
    snap_interpret!(
        uninitialized_is_nil,
        "
        var foo;
        print foo;
        "
    );
    snap_interpret!(missing_global, "var bar; print foo;");
    snap_interpret!(
        shadowing,
        "
        var a = 1;
        var a;
        print a;
        "
    );

    snap_interpret!(
        assignment,
        "
        var a = 0;
        a = \"a\";
        print a;
        "
    );

    snap_interpret!(
        nested_assign,
        "
        var a;
        var b = a = 1;
        print a;
        print b;
        "
    );
    snap_interpret!(
        assign_parens,
        "
        var a;
        print 1 * (a = 2);  
        "
    );
    snap_interpret!(
        assign_a_string,
        "
        var s = \"foo\";
        s = s + \"bar\";
        print s;
        "
    );
    snap_interpret! {
        print_assign,
        "
        var s;
        print s = \"s\";
        "
    }

    snap_interpret! {
        basic_scope,
        "
        {
            var a = 1;
            print a;
        }
        "
    }

    snap_interpret! {
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

    snap_interpret! {
        inaccessible_scope,
        "
        {
            var a = 1;
        }
        print a;
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        set_undeclared_global,
        "
        a = 1;
        "
    }

    snap_interpret! {invalid_expr_in_parens, "(1 1"}
    snap_interpret! {
        not_a_number,
        "
        var n = 0 / 0;
        print n == n;
        "
    }
    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        cannot_use_global_var_in_declaration,
        "
        var a = a;
        "
    }

    snap_interpret! {
        cannot_use_local_var_in_declaration,
        "
        {
            var a = a;
        }
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        parens_ok,
        "
        if (true) {
            print 1;
        }
        "
    }

    snap_interpret! {
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

    snap_interpret! {
        invalid_if_cond,
        "
        if print a; {
            print 1;
        }
        "
    }

    snap_interpret! {
        invalid_block,
        "
        if true {
            print 1
        }
        "
    }

    snap_interpret! {
        basic_else,
        "
        if false {
            print 1;
        } else {
            print 2;
        }
        "
    }

    snap_interpret! {
        basic_then,
        "
        if true {
            print 1;
        } else {
            print 2;
        }
        "
    }

    snap_interpret! {
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

    snap_interpret! {
        assignment_in_parens,
        "
        var a;
        a = 1 + (a = 1);
        print a;
        "
    }

    snap_interpret! {
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

    snap_interpret! {
        and_condition,
        "
        var a;
        if true and (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap_interpret! {
        and_short_circuit,
        "
        var a;
        if false and (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap_interpret! {
        or_condition,
        "
        var a;
        if false or (a = 1) {
            print a;
        }
        print a;
        "
    }

    snap_interpret! {
        or_short_circuit,
        "
        var a;
        if true or (a = 1) {
            print a;
        }
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        basic_for_loop,
        "
        for var a = 0; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap_interpret! {
        bigger_scope_for_loop,
        "
        var a;
        for a = 0; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap_interpret! {
        empty_initializer_for_loop,
        "
        var a = 0;
        for ; a < 3; a = a + 1 {
            print a;
        }
        "
    }

    snap_interpret! {
        empty_update_for_loop,
        "
        for var a = 0; a < 3; {
            print a;
            a = a + 1;
        }
        "
    }

    snap_interpret! {
        nested_for_loops,
        "
        for var a = 0; a < 3; a = a + 1 {
            for var b = 1; b < 3; b = b * 2 {
                print b;
            }
        }
        "
    }

    snap_interpret! {
        for_loop_is_scoped,
        "
        for var a = 0; a < 3; a = a + 1 {}
        print a;
        "
    }

    snap_interpret! {
        function_declaration,
        "
        fun foo() {}

        print foo;
        "
    }

    snap_interpret! {
        basic_function_call,
        r#"
        fun hello_world() {
            print "Hello, World!"; 
        }
        
        hello_world();
        "#
    }

    snap_interpret! {
        function_call_args,
        "
        fun show(n) {
            print n;
        }

        show(1);
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        function_out_of_scope,
        "
        {
            fun foo() {}
        }
        foo();
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
        return_value,
        "
        fun foo() {
            return 1;
        }

        print foo();
        "
    }

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! {
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

    snap_interpret! { // intentionally deviates - no harm in allowing this
        top_level_return,
        "
        return 1;
        "
    }

    snap_interpret! {
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

    snap_interpret! {
        clock_call,
        // time is variable and I don't want to constantly accept this
        "
        var time = clock();
        print time / time;
        "
    }

    snap_interpret! {
        clock_call_wrong_arity,
        "
        clock(1);
        "
    }

    snap_interpret! {
        clock_scoped,
        "
        {
            clock();
        }
        "
    }

    snap_interpret! {
        clock_overload,
        "
        fun clock() {
            return 1;
        }
        print clock();
        "
    }

    snap_interpret! {
        comments,
        "
        // comment
        print 1; // number
        // comment print 2;
        "
    }

    snap_interpret! {
        calls_and_other_operator,
        "
        fun foo() {
            return 1;
        }
        print foo() + foo();
        "
    }

    snap_interpret! {
        scoped_recursion,
        "
        {
            fun foo(n) {
                if n == 0 {
                    return 0;
                } else {
                    return 1 + foo(n - 1);
                }
            }
            print foo(1);
        }
        "
    }

    snap_interpret! {
        if_then_nil,
        "
        fun foo() {
            if false {
                return 0;
            }
        }
        print foo();
        "
    }

    snap_all! {
        stack_closure,
        r#"
        fun outer() {
            var x = "outside";
            fun inner() {
              print x;
            }
            inner();
          }
          outer();
        "#
    }

    snap_interpret! {
        escaping_closure,
        r#"
        fun outer() {
            var x = 1;
            fun inner() {
                print x;
            }
            return inner;
        }
        var filler1 = 0;
        var filler2 = 0;
        var filler3 = 0;
        var filler4 = 0;
        outer()();
        "#
    }

    snap_interpret! {
        escaping_argument,
        "
        fun outer(n) {
            fun inner() {
                print n;
            }
            return inner;
        }
        var filler1 = 0;
        var filler2 = 0;
        var filler3 = 0;
        var filler4 = 0;
        outer(1)();
        "
    }

    snap_interpret! {
        various_nested_closures,
        "
        fun a(n) {
            print n;
            fun b(n) {
                fun c(x, y) {
                    print a;
                    print b;
                    print x;
                    print y;
                    print n;
                }
                print n;
                return c;
            }
            return b;
        }
        var f = a(1);
        var f2 = f(2);
        f2(3, 4);
        "
    }

    snap_interpret! {
        shadowing_function_with_parameter,
        "
        fun a(a) {
            print a;
        }
        a(1);
        "
    }

    snap_interpret! {
        more_nested_closures,
        "
        fun a(a1, a2) {
            fun b(b3, b4) {
                fun c(c5, c6) {
                    print a;
                    print a1;
                    print a2;
                    print b;
                    print b3;
                    print b4;
                    print c;
                    print c5;
                    print c6;
                }
                return c;
            }
            return b;
        }
        a(1, 2)(3, 4)(5, 6);
        "
    }
}
