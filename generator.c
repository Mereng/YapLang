#include <ast.h>

char *gen_buf = NULL;
int gen_indent = 0;
SrcLocation gen_location;

const char *gen_init = "#include <stdio.h>\n";

#define genf(...) buf_printf(gen_buf, __VA_ARGS__)
void genln() {
    genf("\n%.*s", gen_indent * 4, "                                                                 ");
    gen_location.line++;
}
#define genlnf(...) (genln(), genf(__VA_ARGS__))

const char* cdecl_name(Type *type) {
    switch (type->kind) {
        case TYPE_VOID:
            return "void";
        case TYPE_CHAR:
            return "char";
        case TYPE_INT:
            return "int";
        case TYPE_FLOAT:
            return "float";
        case TYPE_STRUCT:
        case TYPE_UNION:
            return type->entity->name;
        default:
            assert(0);
            return NULL;
    }
}

static inline const char *cdecl_paren(const char *str, bool is_paren) {
    return is_paren ? stringf("(%s)", str) : str;
}

void generate_expression(Expression *expr);

char* type_to_cdecl(Type *type, const char *str) {
    switch (type->kind) {
        case TYPE_VOID:
        case TYPE_CHAR:
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_STRUCT:
        case TYPE_UNION:
            return stringf("%s%s%s", cdecl_name(type), *str ? " ": "", str);
        case TYPE_ARRAY:
            return type_to_cdecl(type->array.base, cdecl_paren(stringf("%s[%"PRIu64"]", str, type->array.size), *str));
        case TYPE_POINTER:
            return type_to_cdecl(type->pointer.base, cdecl_paren(stringf("*%s", str), *str));
        case TYPE_FUNC: {
            char *buf = NULL;
            buf_printf(buf, "%s(", cdecl_paren(stringf("*%s", str), *str));
            if (type->func.num_args == 0) {
                buf_printf(buf, "void");
            } else {
                for (size_t i = 0; i < type->func.num_args; i++) {
                    buf_printf(buf, "%s%s", i == 0 ? "": ",", type_to_cdecl(type->func.args[i], ""));
                }
            }
            buf_printf(buf, ")");
            return type_to_cdecl(type->func.ret, buf);
        }
        default:
            assert(0);
            return NULL;
    }
}

char* typespec_to_cdecl(Typespec *typespec, const char *str) {
    switch (typespec->kind) {
        case TYPESPEC_NAME:
            return stringf("%s%s%s", typespec->name, *str ? " ": "", str);
        case TYPESPEC_ARRAY: {
            char *tmp_buf = gen_buf;
            gen_buf = NULL;
            generate_expression(typespec->array.size);
            char *r = typespec_to_cdecl(typespec->array.base, cdecl_paren(stringf("%s[%s]", str, gen_buf), *str));
            gen_buf = tmp_buf;
            return r;
        }
        case TYPESPEC_POINTER:
            return typespec_to_cdecl(typespec->pointer.base, cdecl_paren(stringf("*%s", str), *str));
        case TYPESPEC_FUNC: {
            char *buf = NULL;
            buf_printf(buf, "%s(", cdecl_paren(stringf("*%s", str), *str));
            if (typespec->func.num_args == 0) {
                buf_printf(buf, "void");
            } else {
                for (size_t i = 0; i < typespec->func.num_args; i++) {
                    buf_printf(buf, "%s%s", i == 0 ? "": ",", typespec_to_cdecl(typespec->func.args[i], ""));
                }
            }
            buf_printf(buf, ")");
            if (typespec->func.ret) {
                return typespec_to_cdecl(typespec->func.ret, buf);
            } else {
                return type_to_cdecl(type_void, buf);
            }
        }
        default:
            assert(0);
            return NULL;
    }
}


char char_to_escape[256] = {
    ['\n'] = 'n',
    ['\\'] = '\\',
    ['"'] = '"',
    ['\''] = '\''
};

void generate_string(const char *str) {
    genf("\"");
    while (*str) {
        const char *start = str;
        while (*str && !char_to_escape[*(unsigned char*)str]) {
            str++;
        }
        if (start != str) {
            genf("%.*s", str - start, start);
        }
        if (*str && char_to_escape[*(unsigned char*)str]) {
            genf("\\%c", char_to_escape[*(unsigned char*)str]);
            str++;
        }
    }
    genf("\"");
}

void generate_sync_location(SrcLocation location) {
    if (gen_location.line != location.line || gen_location.name != location.name) {
        genlnf("#line %d", location.line);
        generate_string(location.name);
        gen_location = location;
    }
}

void generate_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT:
            genf("%"PRId64, expr->int_val);
            break;
        case EXPR_FLOAT:
            genf("%f", expr->float_val);
            break;
        case EXPR_STR:
            generate_string(expr->str_val);
            break;
        case EXPR_NAME:
            genf("%s", expr->name);
            break;
        case EXPR_CAST:
            genf("(%s)(", type_to_cdecl(expr->cast.typespec->type, ""));
            generate_expression(expr->cast.expr);
            genf(")");
            break;
        case EXPR_CALL:
            generate_expression(expr->call.operand);
            genf("(");
            for (size_t i = 0; i < expr->call.num_args; i++) {
                if (i != 0) {
                    genf(", ");
                }
                generate_expression(expr->call.args[i]);
            }
            genf(")");
            break;
        case EXPR_INDEX:
            generate_expression(expr->index.operand);
            genf("[");
            generate_expression(expr->index.index);
            genf("]");
            break;
        case EXPR_FIELD:
            generate_expression(expr->field.operand);
            genf(".%s", expr->field.name);
            break;
        case EXPR_COMPOUND:
            if (expr->compound.type) {
                genf("(%s){", typespec_to_cdecl(expr->compound.type, ""));
            } else {
                genf("(%s){", type_to_cdecl(expr->type, ""));
            }

            for (size_t i = 0; i < expr->compound.num_fields; i++) {
                if (i != 0) {
                    genf(", ");
                }
                CompoundField field = expr->compound.fields[i];
                if (field.kind == COMPOUNDFIELD_NAME) {
                    genf(".%s = ", field.name);
                } else if (field.kind == COMPOUNDFIELD_INDEX) {
                    genf("[");
                    generate_expression(field.index);
                    genf("] = ");
                }
                generate_expression(field.init);
            }
            genf("}");
            break;
        case EXPR_UNARY:
            genf("%s(", token_kind_names[expr->unary.op]);
            generate_expression(expr->unary.operand);
            genf(")");
            break;
        case EXPR_BINARY:
            genf("(");
            generate_expression(expr->binary.left);
            genf(") %s (", token_kind_names[expr->binary.op]);
            generate_expression(expr->binary.right);
            genf(")");
            break;
        case EXPR_TERNARY:
            genf("(");
            generate_expression(expr->ternary.cond);
            genf(" ? ");
            generate_expression(expr->ternary.then_ex);
            genf(" : ");
            generate_expression(expr->ternary.else_ex);
            genf(")");
            break;
        case EXPR_SIZEOF_EXPR:
            genf("sizeof(");
            generate_expression(expr->size_of_expr);
            genf(")");
            break;
        case EXPR_SIZEOF_TYPE:
            genf("sizeof(%s)", type_to_cdecl(expr->size_of_type->type, ""));
            break;
        default:
            assert(0);
            break;
    }
}

void generate_statement(Statement *stmt);

void generate_statement_block(StatementBlock block) {
    genf("{");
    gen_indent++;
    for (size_t i = 0; i < block.num_statements; i++) {
        generate_statement(block.statements[i]);
    }
    gen_indent--;
    genlnf("}");
}

void generate_simple_statement(Statement *stmt) {
    switch (stmt->kind) {
        case STMT_EXPR:
            generate_expression(stmt->expr);
            break;
        case STMT_AUTO_ASSIGN:
            genf("%s = ", type_to_cdecl(stmt->auto_assign.init->type, stmt->auto_assign.name));
            generate_expression(stmt->auto_assign.init);
            break;
        case STMT_ASSIGN:
            generate_expression(stmt->assign.left);
            if (stmt->assign.right) {
                genf(" %s ", token_kind_names[stmt->assign.op]);
                generate_expression(stmt->assign.right);
            } else {
                genf("%s", token_kind_names[stmt->assign.op]);
            }
            break;
        default:
            assert(0);
            break;
    }
}

void generate_statement(Statement *stmt) {
    generate_sync_location(stmt->location);
    switch (stmt->kind) {
        case STMT_IF:
            genlnf("if (");
            generate_expression(stmt->if_stmt.cond);
            genf(") ");
            generate_statement_block(stmt->if_stmt.then);
            for (size_t i = 0; i < stmt->if_stmt.num_else_ifs; i++) {
                ElseIf elif = stmt->if_stmt.else_ifs[i];
                genf(" else if (");
                generate_expression(elif.cond);
                genf(")");
                generate_statement_block(elif.body);
            }

            if (stmt->if_stmt.else_body.statements) {
                genf(" else ");
                generate_statement_block(stmt->if_stmt.else_body);
            }
            break;
        case STMT_WHILE:
            genlnf("while (");
            generate_expression(stmt->while_stmt.cond);
            genf(") ");
            generate_statement_block(stmt->while_stmt.body);
            break;
        case STMT_DO_WHILE:
            genlnf("do ");
            generate_statement_block(stmt->while_stmt.body);
            genf(" while (");
            generate_expression(stmt->while_stmt.cond);
            genf(");");
            break;
        case STMT_FOR:
            genlnf("for (");
            if (stmt->for_stmt.init) {
                generate_simple_statement(stmt->for_stmt.init);
            }
            genf(";");
            if (stmt->for_stmt.cond) {
                genf(" ");
                generate_expression(stmt->for_stmt.cond);
            }
            genf(";");
            if (stmt->for_stmt.next) {
                genf(" ");
                generate_simple_statement(stmt->for_stmt.next);
            }
            genf(") ");
            generate_statement_block(stmt->for_stmt.body);
            break;
        case STMT_SWITCH:
            genlnf("switch (");
            generate_expression(stmt->switch_stmt.expr);
            genf(") {");
            for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
                SwitchCase _case = stmt->switch_stmt.cases[i];
                for (size_t j = 0; j < _case.num_expressions; j++) {
                    genlnf("case ");
                    generate_expression(_case.expressions[j]);
                    genf(":");
                }
                if (_case.is_default) {
                    genlnf("default:");
                }
                genf(" {");
                gen_indent++;
                for (size_t j = 0; j < _case.body.num_statements; j++) {
                    generate_statement(_case.body.statements[j]);
                }
                genlnf("break;");
                gen_indent--;
                genlnf("}");
            }
            genlnf("}");
            break;
        case STMT_RETURN:
            genlnf("return");
            if (stmt->expr) {
                genf(" ");
                generate_expression(stmt->expr);
            }
            genf(";");
            break;
        case STMT_BREAK:
            genlnf("break;");
            break;
        case STMT_CONTINUE:
            genlnf("continue;");
            break;
        case STMT_BLOCK:
            genln();
            generate_statement_block(stmt->block);
            break;
        default:
            genln();
            generate_simple_statement(stmt);
            genf(";");
            break;
    }
}

void generate_func_declaration(Declaration *decl) {
    generate_sync_location(decl->location);
    if (decl->func.return_type) {
        genlnf("%s(", typespec_to_cdecl(decl->func.return_type, decl->name));
    } else {
        genlnf("void %s(", decl->name);
    }

    if (decl->func.num_params == 0) {
        genf("void");
    } else {
        for (size_t i = 0; i < decl->func.num_params; i++) {
            FuncParam param = decl->func.params[i];
            if (i != 0) {
                genf(", ");
            }
            genf("%s", typespec_to_cdecl(param.type, param.name));
        }
    }
    genf(")");
}

void generate_forward_declarations() {
    for (Entity **it = global_entities_buf; it != buf_end(global_entities_buf); it++) {
        Entity *entity = *it;
        Declaration *decl = entity->decl;
        if (!decl) {
            continue;
        }

        switch (decl->kind) {
            case DECL_STRUCT:
                genlnf("typedef struct %s %s;", entity->name, entity->name);
                break;
            case DECL_UNION:
                genlnf("typedef union %s %s;", entity->name, entity->name);
                break;
            case DECL_FUNC:
                generate_func_declaration(decl);
                genf(";");
                break;
            default:
                // nothing
                break;
        }
    }
}

void generate_aggregate(Declaration *decl) {
    genlnf("%s %s {", decl->kind == DECL_STRUCT ? "struct": "union", decl->name);
    gen_indent++;
    for (size_t i = 0; i < decl->aggregate.num_items; i++) {
        AggregateItem item = decl->aggregate.items[i];
        for (size_t j = 0; j < item.num_names; j++) {
            generate_sync_location(item.location);
            genlnf("%s;", typespec_to_cdecl(item.type, item.names[j]));
        }
    }
    gen_indent--;
    genlnf("};");
}

void generate_entity(Entity *entity) {
    Declaration *decl = entity->decl;
    if (!decl) {
        return;
    }
    generate_sync_location(decl->location);
    switch (decl->kind) {
        case DECL_CONST:
            genlnf("enum { %s = ", entity->name);
            generate_expression(decl->const_decl.expr);
            genf(" };");
            break;
        case DECL_VAR:
            if (decl->var.type) {
                genlnf("%s", typespec_to_cdecl(decl->var.type, entity->name));
            } else {
                genlnf("%s", type_to_cdecl(entity->type, entity->name));
            }
            if (decl->var.expr) {
                genf(" = ");
                generate_expression(decl->var.expr);
            }
            genf(";");
            break;
        case DECL_FUNC:
            generate_func_declaration(decl);
            genf(" ");
            generate_statement_block(decl->func.body);
            break;
        case DECL_STRUCT:
        case DECL_UNION:
            generate_aggregate(decl);
            break;
        case DECL_TYPEDEF:
            genlnf("typedef %s;", typespec_to_cdecl(decl->typedef_decl.type, entity->name));
            break;
        default:
            assert(0);
            break;
    }
}

void generate_ordered_entities() {
    for (Entity **it = entities_ordered; it != buf_end(entities_ordered); it++) {
        generate_entity(*it);
    }
}

void generate_c_code() {
    gen_buf = NULL;
    genf("%s", gen_init);
    generate_forward_declarations();
    genln();
    generate_ordered_entities();
}