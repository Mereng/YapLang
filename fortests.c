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
        default:
            break;
    }
}

void print_statement(Statement *stmt);

void print_statement_block(StatementBlock block) {
    for (Statement **it = block.statements; it != block.statements + block.num_statements; it++) {
        printf("\t");
        print_statement(*it);
        printf("\n");
    }
}

void print_statement(Statement *stmt) {
    switch (stmt->kind) {
        case STMT_IF:
            printf("(if ");
            print_expression(stmt->if_stmt.cond);
            printf("\n");
            print_statement_block(stmt->if_stmt.then);
            for (ElseIf *it = stmt->if_stmt.else_ifs + stmt->if_stmt.num_else_ifs; it++) {
                printf("(elseif ");
                print_expression(it->cond);
                printf("\n");
                print_statement_block(it->body);
                printf(")");
            }
    }
}