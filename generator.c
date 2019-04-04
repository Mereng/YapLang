#include <ast.h>

char *gen_buf = NULL;
int gen_indent = 0;
SrcLocation gen_location;

const char *gen_init = "#include <stdio.h>\n"
                       "#include <stdbool.h>\n"
                       "#include <math.h>\n"
                       "typedef signed char schar;\n"
                       "typedef unsigned char uchar;\n"
                       "typedef unsigned short ushort;\n"
                       "typedef unsigned int uint;\n"
                       "typedef unsigned long ulong;\n"
                       "typedef long long llong;\n"
                       "typedef unsigned long long ullong;\n"
                       "typedef char int8;\n"
                       "typedef uchar uint8;\n"
                       "typedef short int16;\n"
                       "typedef ushort uint16;\n"
                       "typedef int int32;\n"
                       "typedef uint uint32;\n"
                       "typedef llong int64;\n"
                       "typedef ullong uint64;\n";

#define genf(...) buf_printf(gen_buf, __VA_ARGS__)
void genln() {
    genf("\n%.*s", gen_indent * 4, "                                                                 ");
    gen_location.line++;
}
#define genlnf(...) (genln(), genf(__VA_ARGS__))

const char* cdecl_name(Type *type) {
    const char *name = type_names[type->kind];
    if (name) {
        return name;
    } else {
        return type->entity->name;
    }
}

static inline const char *cdecl_paren(const char *str, bool is_paren) {
    return is_paren ? stringf("(%s)", str) : str;
}

void generate_expression(Expression *expr);

char* type_to_cdecl(Type *type, const char *str) {
    switch (type->kind) {
        case TYPE_ARRAY:
            if (type->num_elements == 0) {
                return type_to_cdecl(type->base, cdecl_paren(stringf("%s[]", str), *str));
            } else {
                return type_to_cdecl(type->base, cdecl_paren(stringf("%s[%zu]", str, type->num_elements), *str));
            }
        case TYPE_POINTER:
            return type_to_cdecl(type->base, cdecl_paren(stringf("*%s", str), *str));
        case TYPE_CONST:
            return type_to_cdecl(type->base, stringf("const %s", cdecl_paren(str, *str)));
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

            if (type->func.is_variadic) {
                buf_printf(buf, ", ...");
            }

            buf_printf(buf, ")");
            return type_to_cdecl(type->func.ret, buf);
        }
        default:
            return stringf("%s%s%s", cdecl_name(type), *str ? " ": "", str);;
    }
}

char* typespec_to_cdecl(Typespec *typespec, const char *str) {
    switch (typespec->kind) {
        case TYPESPEC_NAME:
            return stringf("%s%s%s", typespec->name, *str ? " ": "", str);
        case TYPESPEC_ARRAY: {
            if (typespec->size == 0) {
                return typespec_to_cdecl(typespec->base, cdecl_paren(stringf("%s[]", str), *str));
            } else {
                char *tmp_buf = gen_buf;
                gen_buf = NULL;
                generate_expression(typespec->size);
                char *r = typespec_to_cdecl(typespec->base, cdecl_paren(stringf("%s[%s]", str, gen_buf), *str));
                gen_buf = tmp_buf;
                return r;
            }
        }
        case TYPESPEC_POINTER:
            return typespec_to_cdecl(typespec->base, cdecl_paren(stringf("*%s", str), *str));
        case TYPESPEC_CONST:
            return typespec_to_cdecl(typespec->base, stringf("const %s", cdecl_paren(str, *str)));
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

            if (typespec->func.is_variadic) {
                buf_printf(buf, ", ...");
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
        ['\0'] = '0',
        ['\n'] = 'n',
        ['\r'] = 'r',
        ['\t'] = 't',
        ['\v'] = 'v',
        ['\b'] = 'b',
        ['\a'] = 'a',
        ['\\'] = '\\',
        ['"'] = '"',
        ['\''] = '\''
};

void generate_char(char c) {
    if (char_to_escape[(unsigned char)c]) {
        genf("'\\%c'", char_to_escape[(unsigned char)c]);
    } else if (isprint(c)) {
        genf("'%c'", c);
    } else {
        genf("'\\x%x'", c);
    }
}

void generate_string(const char *str, bool multiline) {
    if (multiline) {
        gen_indent++;
        genln();
    }
    genf("\"");
    while (*str) {
        const char *start = str;
        while (*str && isprint(*str) && !char_to_escape[*(unsigned char*)str]) {
            str++;
        }
        if (start != str) {
            genf("%.*s", str - start, start);
        }
        if (*str) {
            if (char_to_escape[*(unsigned char*)str]) {
                genf("\\%c", char_to_escape[*(unsigned char *) str]);
                if (str[0] == '\n' && str[1]) {
                    genf("\"");
                    genlnf("\"");
                }
            } else {
                genf("\\x%x", *str);
            }
            str++;
        }
    }
    genf("\"");
    if (multiline) {
        gen_indent--;
    }
}

void generate_sync_location(SrcLocation location) {
    if (gen_location.line != location.line || gen_location.name != location.name) {
        genlnf("#line %d", location.line);
        if (gen_location.name != location.name) {
            genf(" ");
            generate_string(location.name, false);
        }
        gen_location = location;
    }
}

void generate_expression_compound(Expression *expr, bool is_auto_assign) {
    if (is_auto_assign) {
        genf("{");
    } else if (expr->compound.type) {
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

    if (expr->compound.num_fields == 0) {
        genf("0");
    }

    genf("}");
}

void generate_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT: {
            const char *suffix = token_suffix_name[expr->int_lit.suffix];
            switch (expr->int_lit.mod) {
                case TOKENMOD_BIN:
                case TOKENMOD_HEX:
                    genf("0x%llx%s", expr->int_lit.val, suffix);
                    break;
                case TOKENMOD_OCT:
                    genf("0%llo%s", expr->int_lit.val, suffix);
                    break;
                case TOKENMOD_CHAR:
                    generate_char((char)expr->int_lit.val);
                    break;
                default:
                    genf("%llu%s", expr->int_lit.val, suffix);
                    break;
            }
            break;
        }
        case EXPR_FLOAT:
            genf("%f%s", expr->float_lit.val, expr->float_lit.suffix == TOKENSUFFIX_D ? "" : "f");
            break;
        case EXPR_STR:
            generate_string(expr->str_lit.val, expr->str_lit.mod == TOKENMOD_MULTILINE);
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
            genf("(");
            generate_expression(expr->call.operand);
            genf(")");
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
            genf("%s%s", expr->field.operand->type->kind == TYPE_POINTER ? "->" : ".", expr->field.name);
            break;
        case EXPR_COMPOUND:
            generate_expression_compound(expr, false);
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

void generate_init_expression(Expression *expr) {
    if (expr->kind == EXPR_COMPOUND) {
        generate_expression_compound(expr, true);
    } else {
        generate_expression(expr);
    }
}

void generate_simple_statement(Statement *stmt) {
    switch (stmt->kind) {
        case STMT_EXPR:
            generate_expression(stmt->expr);
            break;
        case STMT_AUTO_ASSIGN:
            if (stmt->auto_assign.type) {
                if (stmt->auto_assign.type->kind == TYPESPEC_ARRAY && !stmt->auto_assign.type->size) {
                    genf("%s", type_to_cdecl(stmt->auto_assign.init->type, stmt->auto_assign.name));
                } else {
                    genf("%s", typespec_to_cdecl(stmt->auto_assign.type, stmt->auto_assign.name));
                }
                if (stmt->auto_assign.init) {
                    genf(" = ");
                    generate_init_expression(stmt->auto_assign.init);
                }
            } else {
                genf("%s = ", type_to_cdecl(base_type(stmt->auto_assign.init->type), stmt->auto_assign.name));
                generate_init_expression(stmt->auto_assign.init);
            }
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

    if (decl->func.is_variadic) {
        genf(", ...");
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
                if (get_declaration_attribute(decl, keywords.foreign) == NULL) {
                    generate_func_declaration(decl);
                    genf(";");
                }
                break;
            default:
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

bool is_array_typespec_incomplete(Typespec *typespec) {
    return typespec->kind == TYPESPEC_ARRAY && !typespec->size;
}

void generate_declaration(Entity *entity) {
    Declaration *decl = entity->decl;
    if (!decl || (get_declaration_attribute(decl, keywords.foreign))) {
        return;
    }
    generate_sync_location(decl->location);
    switch (decl->kind) {
        case DECL_CONST:
            genlnf("#define %s (", entity->name);
            generate_expression(decl->const_decl.expr);
            genf(")");
            break;
        case DECL_VAR:
            if (decl->var.type && !is_array_typespec_incomplete(decl->var.type)) {
                genlnf("%s", typespec_to_cdecl(decl->var.type, entity->name));
            } else {
                genlnf("%s", type_to_cdecl(entity->type, entity->name));
            }
            if (decl->var.expr) {
                genf(" = ");
                generate_init_expression(decl->var.expr);
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
        generate_declaration(*it);
    }
}

void generate_c_code() {
    gen_buf = NULL;
    genf("%s", gen_init);
    generate_forward_declarations();
    genln();
    generate_ordered_entities();
}