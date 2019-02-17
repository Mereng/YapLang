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
            print_type(expr->compound.type);
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
            printf("(%c ", expr->unary.op);
            print_expression(expr->unary.operand);
            printf(")");
            break;
        case EXPR_BINARY:
            printf("(%c ", expr->binary.op);
            print_expression(expr->binary.left);
            printf(" ");
            print_expression(expr->binary.right);
            printf(")");
            break;
        case EXPR_TERNARY:
            printf("( ");
            print_expression(expr->ternary.cond);
            printf(" ? ");
            print_expression(expr->ternary.then_ex);
            printf(" : ");
            print_expression(expr->ternary.else_ex);
            printf(")");
        case EXPR_SIZEOF:
            printf("(sizeof ");
            if (expr->size_of.kind == SIZEOF_TYPE) {
                print_type(expr->size_of.type);
            } else {
                print_expression(expr->size_of.expr);
            }
        default:
            break;
    }
}

void print_statement(Statement *stmt);

void print_statement_block(StatementBlock block, bool single_line) {
    for (Statement **it = block.statements; it != block.statements + block.num_statements; it++) {
        if (!single_line)
            printf("\t");
        print_statement(*it);
        if (single_line)
            printf(" ");
        else
            printf("\n");
    }
}

const char *token_kind_names[] = {
        [TOKEN_EOF] = "EOF",
        [TOKEN_INT] = "int",
        [TOKEN_FLOAT] = "float",
        [TOKEN_STR] = "string",
        [TOKEN_NAME] = "name",
        [TOKEN_LSHIFT] = "<<",
        [TOKEN_RSHIFT] = ">>",
        [TOKEN_EQ] = "==",
        [TOKEN_NOTEQ] = "!=",
        [TOKEN_LTEQ] = "<=",
        [TOKEN_GTEQ] = ">=",
        [TOKEN_AND] = "&&",
        [TOKEN_OR] = "||",
        [TOKEN_INC] = "++",
        [TOKEN_DEC] = "--",
        [TOKEN_AUTO_ASSIGN] = ":=",
        [TOKEN_ADD_ASSIGN] = "+=",
        [TOKEN_SUB_ASSIGN] = "-=",
        [TOKEN_OR_ASSIGN] = "|=",
        [TOKEN_LSHIFT_ASSIGN] = "<<=",
        [TOKEN_RSHIFT_ASSIGN] = ">>=",
        [TOKEN_AND_ASSIGN] = "&=",
        [TOKEN_XOR_ASSIGN] = "^=",
        [TOKEN_DIV_ASSIGN] = "/=",
        [TOKEN_MOD_ASSIGN] = "%=",
};

void print_statement(Statement *stmt) {
    switch (stmt->kind) {
        case STMT_IF:
            printf("(if ");
            print_expression(stmt->if_stmt.cond);
            printf("\n");
            print_statement_block(stmt->if_stmt.then, false);
            for (ElseIf *it = stmt->if_stmt.else_ifs; it != stmt->if_stmt.else_ifs + stmt->if_stmt.num_else_ifs; it++) {
                printf("elseif ");
                print_expression(it->cond);
                printf("\n");
                print_statement_block(it->body, false);
            }

            if (stmt->if_stmt.else_body.num_statements > 0) {
                printf("else\n");
                print_statement_block(stmt->if_stmt.else_body, false);
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
            printf("\n");
            print_statement_block(stmt->for_stmt.body, false);
            printf(")");
            break;
        case STMT_WHILE:
            printf("(while ");
            print_expression(stmt->while_stmt.cond);
            printf("\n");
            print_statement_block(stmt->while_stmt.body, false);
            printf(")");
            break;
        case STMT_DO_WHILE:
            printf("(do while ");
            print_expression(stmt->while_stmt.cond);
            printf("\n");
            print_statement_block(stmt->while_stmt.body, false);
            printf(")");
            break;
        case STMT_SWITCH:
            printf("(switch ");
            print_expression(stmt->switch_stmt.expr);
            for (SwitchCase *it = stmt->switch_stmt.cases; it != stmt->switch_stmt.cases + stmt->switch_stmt.num_cases; it++) {
                printf("\n");
                printf("(case ");
                if (it->is_default) {
                    printf("default ");
                }

                for (Expression **jt = it->expressions; jt != it->expressions + it->num_expressions; jt++) {
                    printf(" ");
                    print_expression(*jt);
                }
                printf("\n");
                print_statement_block(it->body, false);
            }
            printf(")");
            break;
        case STMT_ASSIGN:
            printf("(%s ", token_kind_names[stmt->assign.op]);
            print_expression(stmt->assign.left);
            printf(" ");
            print_expression(stmt->assign.right);
            printf(")");
            break;
        case STMT_AUTO_ASSIGN:
            printf("(:= %s ", stmt->auto_assign.name);
            print_expression(stmt->auto_assign.init);
            printf(")");
            break;
        case STMT_BLOCK:
            print_statement_block(stmt->block, false);
            break;
        case STMT_EXPR:
            print_expression(stmt->expr);
            break;
        case STMT_RETURN:
            printf("(return ");
            print_expression(stmt->return_stmt.expr);
            printf(")");
            break;
        case STMT_BREAK:
            printf("(break)");
            break;
        case STMT_CONTINUE:
            printf("(continue)");
            break;
        default:
            break;
    }
}