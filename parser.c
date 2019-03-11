Expression* parse_expression();
Typespec *parse_type();
Statement* parse_statement();
Declaration* try_parse_declaration();

const char *parse_name() {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

CompoundField parse_expression_compound_field() {
    if (match_token(TOKEN_LBRACKET)) {
        Expression *index = parse_expression();
        expect_token(TOKEN_RBRACKET);
        expect_token(TOKEN_ASSIGN);
        return (CompoundField){COMPOUNDFIELD_INDEX, parse_expression(), .index = index};
    } else {
        Expression *expr = parse_expression();
        if (match_token(TOKEN_ASSIGN)) {
            if (expr->kind != EXPR_NAME) {
                fatal_syntax("Named initializer in compoound literal must be field name");
            }
            return (CompoundField){COMPOUNDFIELD_NAME, parse_expression(), .name = expr->name};
        } else {
            return (CompoundField){COMPOUNDFIELD_DEFAULT, expr};
        }
    }
}

Expression* parse_expression_compound(Typespec *type) {
    expect_token(TOKEN_LBRACE);
    CompoundField *fields = NULL;
    if (!is_token(TOKEN_RBRACE)) {
        buf_push(fields, parse_expression_compound_field());
        while (match_token(TOKEN_COMMA)) {
            buf_push(fields, parse_expression_compound_field());
        }
    }
    expect_token(TOKEN_RBRACE);
    return expression_compound(type, ast_dup(fields, buf_sizeof(fields)), buf_len(fields));
}

Expression* parse_expression_operand() {
    if (is_token(TOKEN_INT)) {
        int64_t val = token.int_val;
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
        if (is_token(TOKEN_LBRACE)) {
            return parse_expression_compound(typespec_name(name));
        } else {
            return expression_name(name);
        }
    } else if (is_token(TOKEN_LBRACE)) {
        return parse_expression_compound(NULL);
    } else if (match_token(TOKEN_LPAREN)) {
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return parse_expression_compound(type);
        } else {
            Expression *expr = parse_expression();
            expect_token(TOKEN_RPAREN);
            return expr;
        }
    } else if (match_keyword(keywords.sizeof_keyword)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return expression_sizeof_type(type);
        } else {
            Expression *expr = parse_expression();
            expect_token(TOKEN_RPAREN);
            return expression_sizeof_expr(expr);
        }
    } else if (match_keyword(keywords.cast_keyword)) {
        expect_token(TOKEN_LPAREN);
        Typespec *type = parse_type();
        expect_token(TOKEN_COMMA);
        Expression *cast_expr = parse_expression();
        expect_token(TOKEN_RPAREN);
        return expression_cast(type, cast_expr);
    } else {
        fatal_syntax("Unexpected token %s in expression", token_str(token));
        return NULL;
    }
}

Expression* parse_expression_base() {
    Expression *expr = parse_expression_operand();
    while (is_token(TOKEN_LPAREN) || is_token(TOKEN_LBRACKET) || is_token(TOKEN_DOT)) {
        if (match_token(TOKEN_LPAREN)) {
            Expression **args = NULL;
            if (!is_token(TOKEN_RPAREN)) {
                buf_push(args, parse_expression());
                while (match_token(TOKEN_COMMA)) {
                    buf_push(args, parse_expression());
                }
            }
            expect_token(TOKEN_RPAREN);
            expr = expression_call(expr, ast_dup(args, buf_sizeof(args)), buf_len(args));
       } else if (match_token(TOKEN_LBRACKET)) {
            Expression *index = parse_expression();
            expect_token(TOKEN_RBRACKET);
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
    if (is_token(TOKEN_ADD) || is_token(TOKEN_SUB) || is_token(TOKEN_MUL) || is_token(TOKEN_BIN_AND) || is_token(TOKEN_BIN_NOT)) {
        TokenKind op = token.kind;
        next_token();
        return expression_unary(op, parse_expression_unary());
    } else {
        return parse_expression_base();
    }
}

Expression* parse_expression_mul() {
    Expression *expr = parse_expression_unary();
    while (is_token(TOKEN_MUL) || is_token(TOKEN_DIV) || is_token(TOKEN_MOD) || is_token(TOKEN_BIN_AND) || is_token(TOKEN_LSHIFT)
        || is_token(TOKEN_RSHIFT)) {
        TokenKind op = token.kind;
        next_token();
        expr = expression_binary(op, expr, parse_expression_unary());
    }
    return expr;
}

Expression* parse_expression_add() {
    Expression *expr = parse_expression_mul();
    while (is_token(TOKEN_ADD) || is_token(TOKEN_SUB) || is_token(TOKEN_BIN_OR) || is_token(TOKEN_XOR)) {
        TokenKind op = token.kind;
        next_token();
        expr = expression_binary(op, expr, parse_expression_mul());
    }
    return expr;
}

Expression* parse_expression_cmp() {
    Expression* expr = parse_expression_add();
    while (is_token(TOKEN_LT) || is_token(TOKEN_GT) || is_token(TOKEN_EQ) || is_token(TOKEN_NOTEQ) || is_token(TOKEN_LTEQ)
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
    if (match_token(TOKEN_QUESTION_MARK)) {
        Expression *then = parse_expression_ternary();
        expect_token(TOKEN_COLON);
        Expression *else_ex = parse_expression_ternary();
        expr = expression_ternary(expr, then, else_ex);
    }
    return expr;
}

Expression* parse_expression() {
    parse_expression_ternary();
}

Expression* parse_expression_paren() {
    expect_token(TOKEN_LPAREN);
    Expression *expr = parse_expression();
    expect_token(TOKEN_RPAREN);
    return expr;
}

Typespec *parse_type_func() {
    Typespec **args = NULL;
    expect_token(TOKEN_LPAREN);
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(args, parse_type());
        while (match_token(TOKEN_COMMA)) {
            buf_push(args, parse_type());
        }
        expect_token(TOKEN_RPAREN);
    }
    Typespec *ret = NULL;
    if (match_token(TOKEN_COLON)) {
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
        return parse_type_func();
    } else if (match_token(TOKEN_LPAREN)) {
        Typespec *type = parse_type();
        expect_token(TOKEN_RPAREN);
        return type;
    } else {
        fatal_syntax("Unexpected token %s in typespec", token_str(token));
    }
}

Typespec *parse_type() {
    Typespec *type = parse_type_base();

    while (is_token(TOKEN_LBRACKET) || is_token(TOKEN_MUL)) {
        if (match_token(TOKEN_LBRACKET)) {
            Expression *expr = NULL;
            if (!is_token(TOKEN_RBRACKET)) {
                expr = parse_expression();
            }
            expect_token(TOKEN_RBRACKET);
            type = typespec_array(type, expr);
        } else {
            next_token();
            type = typespec_pointer(type);
        }
    }
    return type;
}

StatementBlock parse_statement_block() {
    expect_token(TOKEN_LBRACE);
    Statement **stmts = NULL;
    while (!is_token(TOKEN_EOF) && !is_token(TOKEN_RBRACE)) {
        buf_push(stmts, parse_statement());
    }
    expect_token(TOKEN_RBRACE);
    return (StatementBlock){ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts)};
}

Statement* parse_statement_simple() {
    Expression *expr = parse_expression();
    Statement *stmt;
    if (match_token(TOKEN_AUTO_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            fatal_syntax("operator := must be preceded by a name");
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
    Expression *cond = parse_expression_paren();
    StatementBlock then = parse_statement_block();
    StatementBlock else_block = {0};
    ElseIf *else_ifs = NULL;

    while (match_keyword(keywords.else_keyword)) {
        if (!match_keyword(keywords.if_keyword)) {
            else_block = parse_statement_block();
            break;
        }
        Expression *else_if_cond = parse_expression_paren();
        StatementBlock else_if_block = parse_statement_block();
        buf_push(else_ifs, ((ElseIf){else_if_cond, else_if_block}));
    }
    return statement_if(cond, then, ast_dup(else_ifs, buf_sizeof(else_ifs)), buf_len(else_ifs), else_block);
}

Statement* parse_statement_for() {
    expect_token(TOKEN_LPAREN);
    Statement *init = NULL;
    if (!is_token(TOKEN_SEMICOLON)) {
        init = parse_statement_simple();
    }
    expect_token(TOKEN_SEMICOLON);
    Expression *cond = NULL;
    if (!is_token(TOKEN_SEMICOLON)) {
        cond = parse_expression();
    }
    expect_token(TOKEN_SEMICOLON);
    Statement *next = NULL;
    if (!is_token(TOKEN_RPAREN)) {
        next = parse_statement_simple();
        if (next->kind == STMT_AUTO_ASSIGN) {
            syntax_error("Auto assign statement not allowed in for statement's next clause");
        }
    }
    expect_token(TOKEN_RPAREN);
    return statement_for(init, cond, next, parse_statement_block());
}

Statement* parse_statement_while() {
    Expression *cond = parse_expression_paren();
    return statement_while(cond, parse_statement_block());
}

Statement* parse_statement_do_while() {
    StatementBlock block = parse_statement_block();
    if (!match_keyword(keywords.while_keyword)) {
        fatal_syntax("Expected 'while' after 'do' block");
        return NULL;
    }
    Statement *stmt = statement_do_while(parse_expression_paren(), block);
    expect_token(TOKEN_SEMICOLON);
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
        expect_token(TOKEN_COLON);
    }
    Statement **stmts = NULL;
    while (!is_token(TOKEN_EOF) && !is_token(TOKEN_RBRACE) && !is_keyword(keywords.case_keyword) &&
        !is_keyword(keywords.default_keyword)) {
        buf_push(stmts, parse_statement());
    }
    StatementBlock block = {ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts)};
    return (SwitchCase){ast_dup(exprs, buf_sizeof(exprs)), buf_len(exprs), block, is_default};
}

Statement* parse_statement_switch() {
    Expression *expr = parse_expression_paren();
    SwitchCase *cases = NULL;
    expect_token(TOKEN_LBRACE);
    while (!is_token(TOKEN_EOF) && !is_token(TOKEN_RBRACE)) {
        buf_push(cases, parse_statement_switch_case());
    }
    expect_token(TOKEN_RBRACE);
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
        Statement *stmt = NULL;
        if (!is_token(TOKEN_SEMICOLON)) {
           stmt = statement_return(parse_expression());
        } else {
            stmt = statement_return(NULL);
        }
        expect_token(TOKEN_SEMICOLON);
        return stmt;
    } else if (match_keyword(keywords.break_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        return statement_break();
    } else if (match_keyword(keywords.continue_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        return statement_continue();
    } else if (is_token(TOKEN_LBRACE)) {
        return statement_block(parse_statement_block());
    } else {
        Declaration *d = try_parse_declaration();
        if (d) {
            return statement_decl(d);
        }
        Statement *stmt = parse_statement_simple();
        expect_token(TOKEN_SEMICOLON);
        return stmt;
    }
}

EnumItem parse_declaration_enum_item() {
    const char *name = parse_name();
    Expression *init = NULL;
    if (match_token(TOKEN_ASSIGN)) {
        init = parse_expression();
    }
    return (EnumItem){name, init};
}

Declaration* parse_declaration_enum() {
    const char *name = parse_name();
    expect_token(TOKEN_LBRACE);
    EnumItem *items = NULL;
    if (!is_token(TOKEN_RBRACE)) {
        buf_push(items, parse_declaration_enum_item());
        while (match_token(TOKEN_COMMA)) {
            buf_push(items, parse_declaration_enum_item());
        }
    }
    expect_token(TOKEN_RBRACE);
    return declaration_enum(name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

AggregateItem parse_declaration_aggregate_item() {
    const char **names = NULL;
    buf_push(names, parse_name());
    while (match_token(TOKEN_COMMA)) {
        buf_push(names, parse_name());
    }
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    expect_token(TOKEN_SEMICOLON);
    return (AggregateItem){ast_dup(names, buf_sizeof(names)), buf_len(names), type};
}

Declaration* parse_declaration_aggregate(DeclarationKind kind) {
    const char *name = parse_name();
    expect_token(TOKEN_LBRACE);
    AggregateItem *items = NULL;
    while (!is_token(TOKEN_EOF) && !is_token(TOKEN_RBRACE)) {
        buf_push(items, parse_declaration_aggregate_item());
    }
    expect_token(TOKEN_RBRACE);
    return declaration_aggregate(kind, name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

Declaration* parse_declaration_var() {
    const char *name = parse_name();
    if (match_token(TOKEN_ASSIGN)) {
        return declaration_var(name, NULL, parse_expression());
    } else if (match_token(TOKEN_COLON)) {
        Typespec *type = parse_type();
        Expression *expr = NULL;
        if (match_token(TOKEN_ASSIGN)) {
            expr = parse_expression();
        }
        expect_token(TOKEN_SEMICOLON);
        return declaration_var(name, type, expr);
    } else {
        fatal_syntax("Expected : or = after var, got %s", token_str(token));
        return NULL;
    }
}

Declaration* parse_declaration_const() {
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    return declaration_const(name, parse_expression());
}

Declaration* parse_declaration_typedef() {
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    return declaration_typedef(name, parse_type());
}

FuncParam parse_declaration_func_param() {
    const char *name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    return (FuncParam){name, type};
}

Declaration* parse_declaration_func() {
    const char *name = parse_name();
    expect_token(TOKEN_LPAREN);
    FuncParam *params = NULL;
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(params, parse_declaration_func_param());
        while (match_token(TOKEN_COMMA)) {
            buf_push(params, parse_declaration_func_param());
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret = NULL;
    if (match_token(TOKEN_COLON)) {
        ret = parse_type();
    }
    StatementBlock block = parse_statement_block();
    return declaration_func(name, ast_dup(params, buf_sizeof(params)), buf_len(params), ret, block);
}

Declaration* try_parse_declaration() {
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
        return NULL;
    }
}

Declaration* parse_declaration() {
    Declaration *decl = try_parse_declaration();
    if (!decl) {
        fatal_syntax("Expected declaration keyword, got %s ", token_str(token));
    }

    return decl;
}

DeclarationList* parse_file() {
    Declaration **decls = NULL;
    while (!is_token(TOKEN_EOF)) {
        buf_push(decls, parse_declaration());
    }
    return declaration_list_new(ast_dup(decls, buf_sizeof(decls)), buf_len(decls));
}
