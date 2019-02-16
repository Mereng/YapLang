Expression* parse_expression();
Typespec *parse_type();

const char *parse_name() {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

Expression* parse_expression_compound(Typespec *type) {
    expect_token('{');
    Expression **args = NULL;
    if (!is_token('}')) {
        buf_push(args, parse_expression());
        while (match_token(',')) {
            buf_push(args, parse_expression());
        }
    }
    expect_token('}');
    return expression_compound(type, ast_dup(args, buf_sizeof(args)), buf_len(args));
}

Expression* parse_expression_operand() {
    if (is_token(TOKEN_INT)) {
        uint64_t val = token.int_val;
        next_token();
        return expression_int(val);
    } else if (is_token(TOKEN_FLOAT)) {
        double val = token.float_val;
        next_token();
        return expression_float(val);
    } else if (is_token(TOKEN_STR)) {
        const char *val = token.str_val;
        next_token();
        return expression_str(val);
    } else if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        if (is_token('{')) {
            return parse_expression_compound(typespec_name(name));
        } else {
            return expression_name(name);
        }
    } else if (is_token('{')) {
        return parse_expression_compound(NULL);
    } else if (match_token('(')) {
        if (is_token(':')) {
            Typespec *type = parse_type();
            expect_token(')');
            return parse_expression_compound(type);
        } else {
            Expression *expr = parse_expression();
            expect_token(')');
            return expr;
        }
    } else if(match_keywortd(keywords.sizeof_keyword)) {
        expect_token('(');
        if (match_token(':')) {
            Typespec *type = parse_type();
            expect_token(')');
            return expression_sizeof_type(type);
        } else {
            Expression *expr = parse_expression();
            expect_token(')');
            return expression_sizeof_expr(expr);
        }
    } else {
        char buf[256];
        copy_kind_name(buf, 256, token.kind);
        fatal("Unexpected token %s in expression", buf);
        return NULL;
    }
}

Expression* parse_expression_ternary() {
    Expression *expr =
}

Expression* parse_expression() {

}

Typespec *parse_type_func() {
    Typespec **args = NULL;
    expect_token('(');
    if (!is_token(')')) {
        buf_push(args, parse_type());
        while (match_token(',')) {
            buf_push(args, parse_type());
        }
        expect_token(')');
    }
    Typespec *ret = NULL;
    if (match_token(':')) {
        ret = parse_type();
    }

    return typespec_func(ast_dup(args, buf_sizeof(args)), buf_len(args), ret);
}

Typespec *parse_type_base() {
    if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        return typespec_name(name);
    } else if (match_keywortd(keywords.func_keyword)) {

    } else if (match_token('(')) {
        return parse_type();
    } else {
        char buf[256];
        copy_kind_name(buf, 256, token.kind);
        fatal("Unexpected token %s in type", buf);
    }
}

Typespec *parse_type() {
    Typespec *type = parse_type_base();

    while (is_token('[') || is_token('*')) {
        if (match_token('[')) {
            Expression *expr = NULL;
            if (!is_token('[')) {
                expr =
            }
        }
    }
}

FuncParam parse_func_param() {
    const char *name = parse_name();
    expect_token(':');
    Typespec *type =
}

Declaration* parse_declaration_func() {
    const char *name = parse_name();
    expect_token('(');
    FuncParam *params = NULL;
    if (!is_token(')')) {
        buf_push()
    }
}

Declaration* parse_declaration() {
    if (match_keywortd(keywords.enum_keyword)) {

    } else if (match_keywortd(keywords.struct_keyword)) {

    } else if (match_keywortd(keywords.union_keyword)) {

    } else if (match_keywortd(keywords.var_keyword)) {

    } else if (match_keywortd(keywords.const_keyword)) {

    } else if (match_keywortd(keywords.typedef_keyword)) {

    } else if (match_keywortd(keywords.func_keyword)) {

    } else {
        char buf[256];
        copy_kind_name(buf, 256, token.kind);
        fatal("Expected declaration keyword, got %s ", buf);
    }
}