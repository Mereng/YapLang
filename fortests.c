void print_declaration(Declaration *d);
int indent = 0;

void printnl() {
    printf("\n%*c", 5*indent, '\t');
}

void print_expression(Expression *expr);
void print_type(Typespec *type) {
    switch (type->kind) {
        case TYPESPEC_NAME:
            printf("%s", type->name);
            break;
        case TYPESPEC_POINTER:
            printf("(pointer ");
            print_type(type->ptr.base);
            printf(")");
            break;
        case TYPESPEC_ARRAY:
            printf("(array ");
            print_type(type->arr.base);
            printf(" ");
            print_expression(type->arr.size);
            printf(")");
            break;
        case TYPESPEC_FUNC:
            printf("(func (");
            for (Typespec **it = type->func.args_types; it != type->func.args_types + type->func.num_args_types; it++) {
                printf(" ");
                print_type(*it);
            }
            printf(") ");
            print_type(type->func.return_type);
            printf(" )");
            break;
        default:
            break;
    }
}


void print_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT:
            printf("%"PRIu64, expr->int_val);
            break;
        case EXPR_FLOAT:
            printf("%f", expr->float_val);
            break;
        case EXPR_STR:
            printf("\"%s\"", expr->str_val);
            break;
        case EXPR_NAME:
            printf("%s", expr->name);
            break;
        case EXPR_COMPOUND:
            printf("(compound ");
            if (expr->compound.type) {
                print_type(expr->compound.type);
            } else {
                printf("nil");
            }
            for (Expression **it = expr->compound.args; it != expr->compound.args + expr->compound.num_args; it++) {
                printf(" ");
                print_expression(*it);
            }
            break;
        case EXPR_CAST:
            printf("(cast ");
            print_type(expr->cast.type);
            print_expression(expr->cast.expr);
            printf(")");
            break;
        case EXPR_CALL:
            printf("(call ");
            print_expression(expr->call.operand);
            for (Expression **it = expr->call.args; it != expr->call.num_args + expr->call.args; it++){
                printf(" ");
                print_expression(*it);
            }
            printf(")");
            break;
        case EXPR_INDEX:
            printf("(index ");
            print_expression(expr->index.operand);
            printf(" ");
            print_expression(expr->index.index);
            printf(")");
            break;
        case EXPR_FIELD:
            printf("(field ");
            print_expression(expr->field.operand);
            printf(" %s)", expr->field.name);
            break;
        case EXPR_UNARY:
            printf("(%s ", token_kind_names[expr->unary.op]);
            print_expression(expr->unary.operand);
            printf(")");
            break;
        case EXPR_BINARY:
            printf("(%s ", token_kind_names[expr->binary.op]);
            print_expression(expr->binary.left);
            printf(" ");
            print_expression(expr->binary.right);
            printf(")");
            break;
        case EXPR_TERNARY:
            printf("(");
            print_expression(expr->ternary.cond);
            printf(" ? ");
            print_expression(expr->ternary.then_ex);
            printf(" : ");
            print_expression(expr->ternary.else_ex);
            printf(")");
            break;
        case EXPR_SIZEOF_TYPE:
            printf("(sizeof type ");
            print_type(expr->size_of_type);
            printf(")");
            break;
        case EXPR_SIZEOF_EXPR:
            printf("(sizeof expr");
            print_expression(expr->size_of_expr);
            printf(")");
            break;
        default:
            break;
    }
}

void print_statement(Statement *stmt);

void print_statement_block(StatementBlock block) {
    for (Statement **it = block.statements; it != block.statements + block.num_statements; it++) {
        printnl();
        print_statement(*it);
    }
}

void print_statement(Statement *stmt) {
    switch (stmt->kind) {
        case STMT_IF:
            printf("(if ");
            print_expression(stmt->if_stmt.cond);
            indent++;
            print_statement_block(stmt->if_stmt.then);
            indent--;
            for (ElseIf *it = stmt->if_stmt.else_ifs; it != stmt->if_stmt.else_ifs + stmt->if_stmt.num_else_ifs; it++) {
                printnl();
                printf("elseif ");
                print_expression(it->cond);
                indent++;
                print_statement_block(it->body);
                indent--;
            }

            if (stmt->if_stmt.else_body.num_statements > 0) {
                printnl();
                printf("else ");
                indent++;
                print_statement_block(stmt->if_stmt.else_body);
                indent--;
            }
            printf(")");
            break;
        case STMT_FOR:
            printf("(for ");
            print_statement(stmt->for_stmt.init);
            printf(";");
            print_expression(stmt->for_stmt.cond);
            printf(";");
            print_statement(stmt->for_stmt.next);
            indent++;
            print_statement_block(stmt->for_stmt.body);
            indent--;
            printf(")");
            break;
        case STMT_WHILE:
            printf("(while ");
            print_expression(stmt->while_stmt.cond);
            indent++;
            print_statement_block(stmt->while_stmt.body);
            indent--;
            printf(")");
            break;
        case STMT_DO_WHILE:
            printf("(do while ");
            print_expression(stmt->while_stmt.cond);
            indent++;
            print_statement_block(stmt->while_stmt.body);
            indent--;
            printf(")");
            break;
        case STMT_SWITCH:
            printf("(switch ");
            print_expression(stmt->switch_stmt.expr);
            for (SwitchCase *it = stmt->switch_stmt.cases; it != stmt->switch_stmt.cases + stmt->switch_stmt.num_cases; it++) {
                printnl();
                printf("(case ");
                if (it->is_default) {
                    printf("default ");
                }

                for (Expression **jt = it->expressions; jt != it->expressions + it->num_expressions; jt++) {
                    printf(" ");
                    print_expression(*jt);
                }
                indent++;
                print_statement_block(it->body);
                indent--;
            }
            printf(")");
            break;
        case STMT_ASSIGN:
            printf("(%s ", token_kind_names[stmt->assign.op]);
            print_expression(stmt->assign.left);
            if (stmt->assign.right) {
                printf(" ");
                print_expression(stmt->assign.right);
            }
            printf(")");
            break;
        case STMT_AUTO_ASSIGN:
            printf("(:= %s ", stmt->auto_assign.name);
            print_expression(stmt->auto_assign.init);
            printf(")");
            break;
        case STMT_BLOCK:
            indent++;
            print_statement_block(stmt->block);
            indent--;
            break;
        case STMT_EXPR:
            print_expression(stmt->expr);
            break;
        case STMT_RETURN:
            printf("(return");
            if (stmt->return_stmt.expr) {
                printf(" ");
                print_expression(stmt->return_stmt.expr);
            }
            printf(")");
            break;
        case STMT_BREAK:
            printf("(break)");
            break;
        case STMT_CONTINUE:
            printf("(continue)");
            break;
        case STMT_DECL:
            print_declaration(stmt->decl);
            break;
        default:
            break;
    }
}

void print_declaration_agg(Declaration *d) {
    for (AggregateItem *it = d->agg.items; it != d->agg.items + d->agg.num_items; it++) {
        printnl();
        printf("(");
        print_type(it->type);
        for (const char **name = it->names; name != it->names + it->num_names; name++) {
            printf(" %s", *name);
        }
        printf(")");
    }
}
void print_declaration(Declaration *d) {
    switch (d->kind) {
        case DECL_FUNC:
            printf("(func %s (", d->name);
            for (FuncParam *it = d->func.params; it != d->func.params + d->func.num_params ; it++) {
                printf(" %s ", it->name);
                print_type(it->type);
            }
            printf(") ");
            if (d->func.return_type) {
                print_type(d->func.return_type);
            }
            indent++;
            print_statement_block(d->func.body);
            indent--;
            printf(")");
            break;
        case DECL_VAR:
            printf("(var %s ", d->name);
            if (d->var.type) {
                print_type(d->var.type);
            } else {
                printf("nil");
            }
            printf(" ");
            print_expression(d->var.expr);
            printf(")");
            break;
        case DECL_CONST:
            printf("(const %s ", d->name);
            print_expression(d->const_decl.expr);
            printf(")");
            break;
        case DECL_STRUCT:
            printf("(struct %s ", d->name);
            indent++;
            print_declaration_agg(d);;
            indent--;
            printf(")");
            break;
        case DECL_UNION:
            printf("(union %s ", d->name);
            indent++;
            print_declaration_agg(d);;
            indent--;
            printf(")");
            break;
        case DECL_ENUM:
            printf("(enum %s", d->name);
            indent++;
            for (EnumItem *it = d->enum_delc.items; it != d->enum_delc.items + d->enum_delc.num_items; it++) {
                printnl();
                printf("(%s ", it->name);
                if (it->init) {
                    print_expression(it->init);
                }  else {
                    printf("nil");
                }
                printf(")");
            }
            indent--;
            printf(")");
            break;
        case DECL_TYPEDEF:
            printf("(typedef %s ", d->name);
            print_type(d->typedef_decl.type);
            printf(")");
            break;
        default:
            return;
    }
}



void expr_test() {
    Expression *exprs[] = {
            expression_unary(TOKEN_SUB, expression_float(3.14)),
            expression_binary(TOKEN_DIV, expression_int(5), expression_int(10)),
            expression_ternary(expression_name("isTrue"), expression_str("yes"), expression_str("no")),
            expression_field(expression_name("user"), "name"),
            expression_cast(typespec_pointer(typespec_name("float")), expression_name("void_ptr")),
            expression_call(expression_name("sum"), (Expression*[]){expression_int(2), expression_int(5)}, 2),
            expression_index(expression_field(expression_name("user"), "photos"), expression_int(2)),
            expression_compound(typespec_name("Vec3"),(Expression*[]){expression_float(0.1), expression_float(1.0), expression_float(-0.5)}, 3)
    };

    for (Expression **it = exprs; it != exprs + sizeof(exprs) / sizeof(*exprs) ; it++) {
        print_expression(*it);
        printf("\n");
    }
}

void statement_test() {
    Statement *stmts[] = {
            statement_if(expression_name("isTrue"),
                         (StatementBlock){(Statement*[]) {statement_return(expression_int(54))}, 1},
                         (ElseIf[]){expression_name("isFoo"), (StatementBlock){(Statement*[]){statement_continue()}, 1}}, 1,
                         (StatementBlock){(Statement*[]){statement_auto_assign("bar", expression_float(1.5))}, 1}
            ),
            statement_for(
                    (Statement*){statement_auto_assign("i", expression_int(0))},
                    expression_binary('<', expression_name("i"), expression_int(10)),
                    (Statement*){statement_expr(expression_unary(TOKEN_INC, expression_name("i")))},
                    (StatementBlock){(Statement*[]){statement_expr(expression_unary(TOKEN_ADD_ASSIGN, expression_name("sum")))}, 1}
            ),
            statement_while(
                    expression_binary('>', expression_name("j"), expression_name("foo")),
                    (StatementBlock){(Statement*[]){statement_break()}, 1}
            ),
            statement_do_while(
                    expression_binary('<', expression_name("j"), expression_name("foo")),
                    (StatementBlock){(Statement*[]){statement_break()}, 1}
            ),
            statement_switch(
                    expression_name("kind"),
                    (SwitchCase[]) {
                            (SwitchCase){
                                    (Expression*[]) {
                                            expression_str("foo"),
                                    },
                                    1,
                                    (StatementBlock){(Statement*[]){statement_break()}, 1},
                                    false
                            },
                            (SwitchCase) {
                                    0,
                                    0,
                                    (StatementBlock){(Statement*[]){statement_continue()}, 1},
                                    true
                            }
                    },
                    2
            ),
            statement_assign(TOKEN_ADD_ASSIGN, expression_name("foo"), expression_int(56))
    };

    for (Statement **it = stmts; it != stmts + sizeof(stmts)/ sizeof(*stmts); it++) {
        print_statement(*it);
        printf("\n");
    }
}

void ast_test() {
    expr_test();
    statement_test();
}

void parser_test() {
    const char *code[] = {
            "func fib(n:int):int {if (n == 0) { return 0; } else if (n == 1) { return 1; } else { return fib(n - 1) + fib(n - 2); } }",
            "var i = sizeof(10+20);",
            "var p :Vec3={1,2,3};",
            "const foo = sizeof(:float*[50])"
            "typedef t = func(int):double[1000]",
            "struct Vec3 {x,y,z:float;}",
            "union test {one:Vec3; two:Vec2;}",
            "const pi = 3.14",
            "enum meme {lol = 1, kek = 2, foo}",
            "func foo() {while(i == 10) {i++;}}",
            "func bar() {switch (kind) { case ONE: i++; break; case two: case three: j++; break; default: k++; }}",
            "var isFoo = i == 0? b+2+3/5:9+63+5-10;",
            "func foo() {var i : int = 10; i++; return i;}",
            "func bar() {if (n == 5) {return;}}"
    };

    for (const char **it = code; it != code + sizeof(code) / sizeof(*code); it++) {
        init_stream(*it);
        Declaration *d = parse_declaration();
        print_declaration(d);
        printf("\n");
    }
}

