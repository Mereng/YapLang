Expression* parse_expression();
Typespec *parse_type();
Statement* parse_statement();

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
    } else if(match_keyword(keywords.sizeof_keyword)) {
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

Expression* parse_expression_base() {
    Expression *expr = parse_expression_operand();
    while (is_token('(') || is_token('[') || is_token('.')) {
        if (match_token('(')) {
            Expression **args = NULL;
            if (!is_token(')')) {
                buf_push(args, parse_expression());
                while (match_token(',')) {
                    buf_push(args, parse_expression());
                }
            }
            expect_token(')');
            expr = expression_call(expr, ast_dup(args, buf_sizeof(args)), buf_len(args));
       } else if (match_token('[')) {
            Expression *index = parse_expression();
            expect_token(']');
            expr = expression_index(expr, index);
        } else {
            next_token();
            const char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = expression_field(expr, field);
        }
    }
    return expr;
}

Expression* parse_expression_unary() {
    if (is_token('+') || is_token('-') || is_token('*') || is_token('&')) {
        TokenKind op = token.kind;
        next_token();
        return expression_unary(op, parse_expression_unary());
    } else {
        return parse_expression_base();
    }
}

Expression* parse_expression_mul() {
    Expression *expr = parse_expression_unary();
    while (is_token('*') || is_token('/') || is_token('%') || is_token('&') || is_token(TOKEN_LSHIFT)
        || is_token(TOKEN_RSHIFT)) {
        TokenKind op = token.kind;
        next_token();
        expr = expression_binary(op, expr, parse_expression_unary());
    }
    return expr;
}

Expression* parse_expression_add() {
    Expression *expr = parse_expression_mul();
    while (is_token('+') || is_token('-') || is_token('|') || is_token('^')) {
        TokenKind op = token.kind;
        next_token();
        expr = expression_binary(op, expr, parse_expression_mul());
    }
    return expr;
}

Expression* parse_expression_cmp() {
    Expression* expr = parse_expression_add();
    while (is_token('<') || is_token('>') || is_token(TOKEN_EQ) || is_token(TOKEN_NOTEQ) || is_token(TOKEN_LTEQ)
        || is_token(TOKEN_GTEQ)) {
        TokenKind op = token.kind;
        next_token();
        expr = expression_binary(op, expr, parse_expression_add());
    }
    return expr;
}

Expression* parse_expression_and() {
    Expression *expr = parse_expression_cmp();
    while (match_token(TOKEN_AND)) {
        expr = expression_binary(TOKEN_AND, expr, parse_expression_cmp());
    }
    return expr;
}

Expression* parse_expression_or() {
    Expression *expr = parse_expression_and();
    while (match_token(TOKEN_OR)) {
        expr = expression_binary(TOKEN_OR, expr, parse_expression_and());
    }
    return expr;
}

Expression* parse_expression_ternary() {
    Expression *expr = parse_expression_or();
    if (match_token('?')) {
        Expression *then = parse_expression_ternary();
        expect_token(':');
        Expression *else_ex = parse_expression_ternary();
        expr = expression_ternary(expr, then, else_ex);
    }
    return expr;
}

Expression* parse_expression() {
    parse_expression_ternary();
}

Expression* parse_brackets_expression() {
    expect_token('(');
    Expression *expr = parse_expression();
    expect_token(')');
    return expr;
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
    } else if (match_keyword(keywords.func_keyword)) {

    } else if (match_token('(')) {
        Typespec *type = parse_type();
        expect_token(')');
        return type;
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
            if (!is_token(']')) {
                expr = parse_expression();
            }
            expect_token(']');
            type = typespec_array(type, expr);
        } else {
            next_token();
            type = typespec_pointer(type);
        }
    }
    return type;
}

StatementBlock parse_statement_block() {
    expect_token('{');
    Statement **stmts = NULL;
    while (!is_token(TOKEN_EOF) && !is_token('}')) {
        buf_push(stmts, parse_statement());
    }
    expect_token('}');
    return (StatementBlock){ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts)};
}

Statement* parse_statement_simple() {
    Expression *expr = parse_expression();
    Statement *stmt;
    if (match_token(TOKEN_AUTO_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            fatal("operator := must be preceded by a name");
        }
        stmt = statement_auto_assign(expr->name, parse_expression());
    } else if (TOKEN_START_ASSIGN <= token.kind && token.kind <= TOKEN_END_ASSIGN) {
        TokenKind op = token.kind;
        next_token();
        stmt = statement_assign(op, expr, parse_expression());
    } else if (is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
        TokenKind op = token.kind;
        next_token();
        stmt = statement_assign(op, expr, NULL);
    } else {
        stmt = statement_expr(expr);
    }
    return stmt;
}

Statement* parse_statement_if() {
    Expression *cond = parse_brackets_expression();
    StatementBlock then = parse_statement_block();
    StatementBlock else_block = {0};
    ElseIf *else_ifs = NULL;

    while (match_keyword(keywords.else_keyword)) {
        if (!match_keyword(keywords.if_keyword)) {
            else_block = parse_statement_block();
            break;
        }
        Expression *else_if_cond = parse_brackets_expression();
        StatementBlock else_if_block = parse_statement_block();
        buf_push(else_ifs, ((ElseIf){else_if_cond, else_if_block}));
    }
    return statement_if(cond, then, ast_dup(else_ifs, buf_sizeof(else_ifs)), buf_len(else_ifs), else_block);
}

Statement* parse_statement_for() {
    expect_token('(');
    Statement *init = NULL;
    if (!is_token(';')) {
        init = parse_statement_simple();
    }
    expect_token(';');
    Expression *cond = NULL;
    if (!is_token(';')) {
        cond = parse_expression();
    }
    expect_token(';');
    Statement *next = NULL;
    if (!is_token(')')) {
        next = parse_statement_simple();
        if (next->kind == STMT_AUTO_ASSIGN) {
            syntax_error("Auto assign statement not allowed in for statement's next clause");
        }
    }
    expect_token(')');
    return statement_for(init, cond, next, parse_statement_block());
}

Statement* parse_statement_while() {
    Expression *cond = parse_brackets_expression();
    return statement_while(cond, parse_statement_block());
}

Statement* parse_statement_do_while() {
    StatementBlock block = parse_statement_block();
    if (!match_keyword(keywords.while_keyword)) {
        fatal("Expected 'while' after 'do' block");
        return NULL;
    }
    Statement *stmt = statement_do_while(parse_brackets_expression(), block);
    expect_token(';');
    return stmt;
}

SwitchCase parse_statement_switch_case() {
    Expression **exprs = NULL;
    bool is_default = false;
    while (is_keyword(keywords.case_keyword) || is_keyword(keywords.default_keyword)) {
        if (match_keyword(keywords.case_keyword)) {
            buf_push(exprs, parse_expression());
        } else {
            next_token();
            if (is_default) {
                syntax_error("Duplicate default not allowed in switch clause");
            }
            is_default = true;
        }
        expect_token(':');
    }
    Statement **stmts = NULL;
    while (!is_token(TOKEN_EOF) && !is_token('}') && !is_keyword(keywords.case_keyword) &&
        !is_keyword(keywords.default_keyword)) {
        buf_push(stmts, parse_statement());
    }
    StatementBlock block = {ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts)};
    return (SwitchCase){ast_dup(exprs, buf_sizeof(exprs)), buf_len(exprs), block, is_default};
}

Statement* parse_statement_switch() {
    Expression *expr = parse_brackets_expression();
    SwitchCase *cases = NULL;
    expect_token('{');
    while (!is_token(TOKEN_EOF) && !is_token('}')) {
        buf_push(cases, parse_statement_switch_case());
    }
    expect_token('}');
    return statement_switch(expr, ast_dup(cases, buf_sizeof(cases)), buf_len(cases));
}

Statement* parse_statement() {
    if (match_keyword(keywords.if_keyword)) {
        return parse_statement_if();
    } else if (match_keyword(keywords.for_keyword)) {
        return parse_statement_for();
    } else if (match_keyword(keywords.while_keyword)) {
        return parse_statement_while();
    } else if (match_keyword(keywords.do_keyword)) {
        return parse_statement_do_while();
    } else if (match_keyword(keywords.switch_keyword)) {
        return parse_statement_switch();
    } else if (match_keyword(keywords.return_keyword)) {
        Statement *stmt = statement_return(parse_expression());
        expect_token(';');
        return stmt;
    } else if (match_keyword(keywords.break_keyword)) {
        expect_token(';');
        return statement_break();
    } else if (match_keyword(keywords.continue_keyword)) {
        expect_token(';');
        return statement_continue();
    } else if (is_token('{')) {
        return statement_block(parse_statement_block());
    } else {
        Statement *stmt = parse_statement_simple();
        expect_token(';');
        return stmt;
    }
}

EnumItem parse_declaration_enum_item() {
    const char *name = parse_name();
    Expression *init = NULL;
    if (match_token('=')) {
        init = parse_expression();
    }
    return (EnumItem){name, init};
}

Declaration* parse_declaration_enum() {
    const char *name = parse_name();
    expect_token('{');
    EnumItem *items = NULL;
    if (!is_token('}')) {
        buf_push(items, parse_declaration_enum_item());
        while (match_token(',')) {
            buf_push(items, parse_declaration_enum_item());
        }
    }
    expect_token('}');
    return declaration_enum(name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

AggregateItem parse_declaration_aggregate_item() {
    const char **names = NULL;
    buf_push(names, parse_name());
    while (match_token(',')) {
        buf_push(names, parse_name());
    }
    expect_token(':');
    Typespec *type = parse_type();
    expect_token(';');
    return (AggregateItem){ast_dup(names, buf_sizeof(names)), buf_len(names), type};
}

Declaration* parse_declaration_aggregate(DeclarationKind kind) {
    const char *name = parse_name();
    expect_token('{');
    AggregateItem *items = NULL;
    while (!is_token(TOKEN_EOF) && !is_token('}')) {
        buf_push(items, parse_declaration_aggregate_item());
    }
    expect_token('}');
    return declaration_aggregate(kind, name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

Declaration* parse_declaration_var() {
    const char *name = parse_name();
    if (match_token('=')) {
        return declaration_var(name, NULL, parse_expression());
    } else if (match_token(':')) {
        Typespec *type = parse_type();
        Expression *expr = NULL;
        if (match_token('=')) {
            expr = parse_expression();
        }
        return declaration_var(name, type, expr);
    } else {
        char buf[256];
        copy_kind_name(buf, 256, token.kind);
        fatal("Expected : or = after var, got %s", buf);
        return NULL;
    }
}

Declaration* parse_declaration_const() {
    const char *name = parse_name();
    expect_token('=');
    return declaration_const(name, parse_expression());
}

Declaration* parse_declaration_typedef() {
    const char *name = parse_name();
    expect_token('=');
    return declaration_typedef(name, parse_type());
}

FuncParam parse_declaration_func_param() {
    const char *name = parse_name();
    expect_token(':');
    Typespec *type = parse_type();
    return (FuncParam){name, type};
}

Declaration* parse_declaration_func() {
    const char *name = parse_name();
    expect_token('(');
    FuncParam *params = NULL;
    if (!is_token(')')) {
        buf_push(params, parse_declaration_func_param());
        while (match_token(',')) {
            buf_push(params, parse_declaration_func_param());
        }
    }
    expect_token(')');
    Typespec *ret = NULL;
    if (match_token(':')) {
        ret = parse_type();
    }
    StatementBlock block = parse_statement_block();
    return declaration_func(name, ast_dup(params, buf_sizeof(params)), buf_len(params), ret, block);
}

Declaration* parse_declaration() {
    if (match_keyword(keywords.enum_keyword)) {
        return parse_declaration_enum();
    } else if (match_keyword(keywords.struct_keyword)) {
        return parse_declaration_aggregate(DECL_STRUCT);
    } else if (match_keyword(keywords.union_keyword)) {
        return parse_declaration_aggregate(DECL_UNION);
    } else if (match_keyword(keywords.var_keyword)) {
        return parse_declaration_var();
    } else if (match_keyword(keywords.const_keyword)) {
        return parse_declaration_const();
    } else if (match_keyword(keywords.typedef_keyword)) {
        return parse_declaration_typedef();
    } else if (match_keyword(keywords.func_keyword)) {
        return parse_declaration_func();
    } else {
        char buf[256];
        copy_kind_name(buf, 256, token.kind);
        fatal("Expected declaration keyword, got %s ", buf);
    }
}

void parser_test() {
    const char *code[] = {
            "func fib(n:int):int {if (n == 0) { return 0; } else if (n == 1) { return 1; } else { return fib(n - 1) + fib(n - 2); } }"
    };

    for (const char **it = code; it != code + sizeof(code) / sizeof(*code); it++) {
        init_stream(*it);
        Declaration *d = parse_declaration();
        print_declaration(d);
        printf("\n");
    }
}