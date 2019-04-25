#include <ast.h>

char *gen_buf = NULL;
int gen_indent = 0;
SrcLocation gen_location;

const char **gen_header_buf;

const char *gen_init = "#define _CRT_SECURE_NO_WARNINGS\n"
                       "#include <stdbool.h>\n"
                       "#include <stdint.h>\n"
                       "#include <stddef.h>\n"
                       "#include <assert.h>\n"
                       "\n"
                       "typedef signed char schar;\n"
                       "typedef unsigned char uchar;\n"
                       "typedef unsigned short ushort;\n"
                       "typedef unsigned int uint;\n"
                       "typedef unsigned long ulong;\n"
                       "typedef long long llong;\n"
                       "typedef unsigned long long ullong;\n"
                       "\n"
                       "typedef int8_t int8;\n"
                       "typedef uint8_t uint8;\n"
                       "typedef int16_t int16;\n"
                       "typedef uint16_t uint16;\n"
                       "typedef int32_t int32;\n"
                       "typedef uint32_t uint32;\n"
                       "typedef int64_t int64;\n"
                       "typedef uint64_t uint64;\n"
                       "\n"
                       "typedef uintptr_t uintptr;\n"
                       "typedef size_t usize;\n"
                       "typedef ptrdiff_t ssize;\n"
                       "#ifdef _MSC_VER\n"
                       "#define alignof(x) __alignof(x)\n"
                       "#else\n"
                       "#define alignof(x) __alignof__(x)\n"
                       "#endif\n"
                       "\n";

#define genf(...) buf_printf(gen_buf, __VA_ARGS__)
void genln() {
    genf("\n%.*s", gen_indent * 4, "                                                                 ");
    gen_location.line++;
}
#define genlnf(...) (genln(), genf(__VA_ARGS__))

Map names_map;

const char* get_name_default(const void *key, const char *default_name) {
    const char *name = map_get(&names_map, key);
    if (!name) {
        Entity *entity = get_resolved_entity(key);
        if (entity) {
            if (entity->external_name) {
                name = entity->external_name;
            } else if (entity->package->external_name) {
                name = stringf("%s%s", entity->package->external_name, entity->name);
            } else {
                name = entity->name;
            }
        } else {
            name = default_name;
        }
        map_put(&names_map, key, (void*)name);
    }
    return name;
}

const char* get_name(const void *key) {
    return get_name_default(key, "err!");
}

const char* cdecl_name(Type *type) {
    const char *name = type_names[type->kind];
    if (name) {
        return name;
    } else {
        return get_name(type->entity);
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
            buf_printf(buf, "(*%s)(", str);
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
    if (!typespec) {
        return stringf("void%s%s", *str ? " " : "", str);
    }
    switch (typespec->kind) {
        case TYPESPEC_NAME:
            return stringf("%s%s%s", get_name_default(typespec, typespec->name), *str ? " ": "", str);
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
            buf_printf(buf, "(*%s)(", str);
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
        genf("'\\x%X'", (unsigned char)c);
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
                if (str[0] == '\n' && str[1] && multiline) {
                    genf("\"");
                    genlnf("\"");
                }
            } else {
                genf("\\x%X", (unsigned char)*str);
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

void generate_expression_compound(Expression *expr) {
    Type *expected = get_resolved_expected_type(expr);
    if (expected && expected->kind != TYPE_POINTER) {
        genf("{");
    } else if (expr->compound.type) {
        genf("(%s){", typespec_to_cdecl(expr->compound.type, ""));
    } else {
        genf("(%s){", type_to_cdecl(get_resolved_type(expr), ""));
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
            genf("%s", get_name_default(expr, expr->name));
            break;
        case EXPR_CAST:
            genf("(%s)(", type_to_cdecl(get_resolved_type(expr->cast.typespec), ""));
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
            genf("%s%s", get_resolved_type(expr->field.operand)->kind == TYPE_POINTER ? "->" : ".", expr->field.name);
            break;
        case EXPR_COMPOUND:
            generate_expression_compound(expr);
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
        case EXPR_MODIFY:
            if (!expr->modify.is_post) {
                genf("%s", token_kind_names[expr->modify.op]);
            }
            genf("(");
            generate_expression(expr->modify.operand);
            genf(")");
            if (expr->modify.is_post) {
                genf("%s", token_kind_names[expr->modify.op]);
            }
            break;
        case EXPR_SIZEOF_EXPR:
            genf("sizeof(");
            generate_expression(expr->size_of_expr);
            genf(")");
            break;
        case EXPR_SIZEOF_TYPE:
            genf("sizeof(%s)", type_to_cdecl(get_resolved_type(expr->size_of_type), ""));
            break;
        case EXPR_ALIGNOF_EXPR:
            genf("alignof(%s)", type_to_cdecl(get_resolved_type(expr->align_of_expr), ""));
            break;
        case EXPR_ALIGNOF_TYPE:
            genf("alignof(%s)", type_to_cdecl(get_resolved_type(expr->align_of_type), ""));
            break;
        case EXPR_OFFSETOF:
            genf("offsetof(%s, %s)", typespec_to_cdecl(expr->offset_of_field.type, ""), expr->offset_of_field.name);
            break;
        case EXPR_PAREN:
            genf("(");
            generate_expression(expr->paren.expr);
            genf(")");
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
            if (stmt->auto_assign.type) {
                if (stmt->auto_assign.type->kind == TYPESPEC_ARRAY && !stmt->auto_assign.type->size) {
                    genf("%s", type_to_cdecl(get_resolved_type(stmt->auto_assign.init), stmt->auto_assign.name));
                } else {
                    genf("%s", typespec_to_cdecl(stmt->auto_assign.type, stmt->auto_assign.name));
                }
                genf(" = ");
                if (stmt->auto_assign.init) {
                    generate_expression(stmt->auto_assign.init);
                } else {
                    genf("{0}");
                }
            } else {
                genf("%s = ", type_to_cdecl(unqualify_type(get_resolved_type(stmt->auto_assign.init)), stmt->auto_assign.name));
                generate_expression(stmt->auto_assign.init);
            }
            break;
        case STMT_ASSIGN:
            genf("(");
            generate_expression(stmt->assign.left);
            genf(")");
            genf("%s", token_kind_names[stmt->assign.op]);
            generate_expression(stmt->assign.right);
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
            if (stmt->if_stmt.init) {
                genlnf("{");
                gen_indent++;
                generate_statement(stmt->if_stmt.init);
            }
            generate_sync_location(stmt->location);
            genlnf("if (");
            if (stmt->if_stmt.cond) {
                generate_expression(stmt->if_stmt.cond);
            } else {
                genf("%s", stmt->if_stmt.init->auto_assign.name);
            }
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
            if (stmt->if_stmt.init) {
                gen_indent--;
                genlnf("}");
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
        case STMT_ATTR:
            if (stmt->attribute.name == keywords.assert) {
                genlnf("assert(");
                assert(stmt->attribute.num_args == 1);
                generate_expression(stmt->attribute.args[0].expr);
                genf(");");
            }
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
    char *result = NULL;
    buf_printf(result, "%s(", get_name(decl));

    if (decl->func.num_params == 0) {
        buf_printf(result, "void");
    } else {
        for (size_t i = 0; i < decl->func.num_params; i++) {
            FuncParam param = decl->func.params[i];
            if (i != 0) {
                buf_printf(result, ", ");
            }
            buf_printf(result, "%s", typespec_to_cdecl(param.type, param.name));
        }
    }

    if (decl->func.is_variadic) {
        buf_printf(result, ", ...");
    }

    buf_printf(result, ")");
    generate_sync_location(decl->location);
    if (decl->func.return_type) {
        genlnf("%s", typespec_to_cdecl(decl->func.return_type, result));
    } else {
        genlnf("void %s", result);
    }
}

void generate_forward_declarations() {
    for (Entity **it = entities_ordered; it != buf_end(entities_ordered); it++) {
        Entity *entity = *it;
        Declaration *decl = entity->decl;
        if (!decl) {
            continue;
        }
        if (get_declaration_attribute(decl, keywords.foreign)) {
            continue;
        }
        switch (decl->kind) {
            case DECL_STRUCT:
            case DECL_UNION: {
                const char *name = get_name(entity);
                genlnf("typedef %s %s %s;", decl->kind == DECL_STRUCT ? "struct" : "union", name, name);
                break;
            }
            default:
                break;
        }
    }
}

void generate_aggregate(Declaration *decl) {
    if (decl->is_incomplete) {
        return;;
    }
    genlnf("%s %s {", decl->kind == DECL_STRUCT ? "struct": "union", get_name(decl));
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
            genlnf("#define %s (", get_name(entity));
            if (decl->const_decl.type) {
                genf("(%s)(", typespec_to_cdecl(decl->const_decl.type, ""));
            }
            generate_expression(decl->const_decl.expr);
            if (decl->const_decl.type) {
                genf(")");
            }
            genf(")");
            break;
        case DECL_VAR:
            genlnf("extern ");
            if (decl->var.type && !is_array_typespec_incomplete(decl->var.type)) {
                genlnf("%s", typespec_to_cdecl(decl->var.type, get_name(entity)));
            } else {
                genlnf("%s", type_to_cdecl(entity->type, get_name(entity)));
            }
            genf(";");
            break;
        case DECL_FUNC:
            generate_func_declaration(decl);
            genf(";");
            break;
        case DECL_STRUCT:
        case DECL_UNION:
            generate_aggregate(decl);
            break;
        case DECL_TYPEDEF:
            genlnf("typedef %s;", typespec_to_cdecl(decl->typedef_decl.type, get_name(entity)));
            break;
        case DECL_ENUM:
            genlnf("typedef int %s;", get_name(decl));
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

void generate_definitions() {
    for (Entity **it = entities_ordered; it != buf_end(entities_ordered); it++) {
        Entity *entity = *it;
        Declaration *decl = entity->decl;
        if (entity->state != ENTITY_RESOLVED || !decl || get_declaration_attribute(decl, keywords.foreign) || decl->is_incomplete) {
            continue;
        }
        if (decl->kind == DECL_FUNC) {
            generate_func_declaration(decl);
            genf(" ");
            generate_statement_block(decl->func.body);
            genln();
        } else if (decl->kind == DECL_VAR) {
            if (decl->var.type && !is_array_typespec_incomplete(decl->var.type)){
                genlnf("%s", typespec_to_cdecl(decl->var.type, get_name(entity)));
            } else {
                genlnf("%s", type_to_cdecl(entity->type, get_name(entity)));
            }
            if (decl->var.expr) {
                genf(" = ");
                generate_expression(decl->var.expr);
            }
            genf(";");
        }
    }
}
void generate_package_headers(Package *package) {
    for (size_t i = 0; i < package->num_declarations; i++) {
        Declaration *decl = package->declarations[i];
        if (decl->kind != DECL_ATTRIBUTE) {
            continue;
        }
        Attribute attr = decl->attribute;
        if (attr.name == keywords.foreign) {
            for (size_t j = 0; j < attr.num_args; j++) {
                if (attr.args[j].name != keywords.header) {
                    continue;
                }
                Expression *expr = attr.args[j].expr;
                if (expr->kind != EXPR_STR) {
                    fatal_error(decl->location, "#foreign import' arguments must be string");
                }
                const char *header = expr->str_lit.val;
                bool found = false;
                for (const char **it = gen_header_buf; it != buf_end(gen_header_buf); it++) {
                    if (*it == header) {
                        found = true;
                    }
                }
                if (!found) {
                    buf_push(gen_header_buf, header);
                    genlnf("#include ");
                    if (*header == '<') {
                        genf("%s", header);
                    } else {
                        generate_string(header, false);
                    }
                }
            }
        }
    }
}

void generate_foreign_headers() {
    for (size_t i = 0; i < buf_len(package_list); i++) {
        generate_package_headers(package_list[i]);
    }
}

void generate_package_external_names() {
    for (size_t i = 0; i < buf_len(package_list); i++) {
        Package *p = package_list[i];
        if (!p->external_name) {
            char *external = NULL;
            for (const char *j = p->path; *j; j++) {
                buf_printf(external, "%c", *j == '/' ? '_' : *j);
            }
            buf_printf(external, "_");
            p->external_name = str_intern(external);
        }
    }
}

void generate_c_code() {
    gen_buf = NULL;
    genf("%s", gen_init);
    genln();
    generate_package_external_names();
    generate_foreign_headers();
    generate_forward_declarations();
    genln();
    generate_ordered_entities();
    generate_definitions();
}