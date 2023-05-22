mod util;

snap_input!(missing_primary, "print ();\n");
snap_input!(missing_parens, "print ((1);\n");
snap_input!(rparens, "print 1);\n");
snap_input!(missing_rhs, "print 1 + ;\n");
snap_input!(missing_lhs, "print + 1;\n");
snap_input!(missing_op, "print 1 1;\n");