#include <ast.h>


Type *type_void = &(Type){TYPE_VOID, 0};
Type *type_int = &(Type){TYPE_INT, 4, 4};
Type *type_float = &(Type){TYPE_FLOAT, 4};
Type *type_char = &(Type){TYPE_CHAR, 1, 1};

typedef struct ResolvedExpression {
    Type *type;
    bool is_lvalue;
    bool is_const;
    int64_t val;
} ResolvedExpression;

ResolvedExpression resolved_rvalue(Type *type) {
    return (ResolvedExpression) {
        .type = type,
    };
}

ResolvedExpression resolved_lvalue(Type *type) {
    return (ResolvedExpression) {
        .type = type,
        .is_lvalue = true
    };
}

ResolvedExpression resolved_const(int64_t val) {
    return (ResolvedExpression) {
        .type = type_int,
        .is_const = true,
        .val = val
    };
}

typedef struct PointerTypeCached {
    Type *base;
    Type *pointer;
} PointerTypeCached;

PointerTypeCached *pointer_types_cache;

const size_t POINTER_SIZE = 8;
const size_t POINTER_ALIGN = 8;

Type *type_pointer(Type *base) {
    for (PointerTypeCached *it = pointer_types_cache; it != buf_end(pointer_types_cache); it++) {
        if (it->base == base) {
            return it->pointer;
        }
    }

    Type *type = type_new(TYPE_POINTER);
    type->size = POINTER_SIZE;
    type->align = POINTER_ALIGN;
    type->pointer.base = base;
    buf_push(pointer_types_cache, ((PointerTypeCached){base, type}));
    return type;
}

typedef struct ArrayTypeCached {
    Type *base;
    size_t size;
    Type *array;
} ArrayTypeCached;

ArrayTypeCached *array_type_cache;

Type* type_array(Type *base, size_t size) {
    for (ArrayTypeCached *it = array_type_cache; it != buf_end(array_type_cache); it++) {
        if (it->base == base && it->size == size) {
            return it->array;
        }
    }

    Type *type = type_new(TYPE_ARRAY);
    type->size = size * base->size;
    type->align = base->align;
    type->array.base = base;
    type->array.size = size;
    buf_push(array_type_cache, ((ArrayTypeCached){base, size, type}));
    return type;
}


typedef struct FuncTypeCached {
    Type **params;
    size_t num_params;
    Type *ret;
    Type *func;
} FuncTypeCached;

FuncTypeCached *func_type_cache;

Type* type_func(Type **params, size_t num_params, Type *ret) {
    for (FuncTypeCached *it = func_type_cache; it != buf_end(func_type_cache); it++) {
        if (it->num_params == num_params && it->ret == ret) {
            bool isMatch = true;
            for (size_t i = 0; i < num_params; i++) {
                if (it->params[i] != params[i]) {
                    isMatch = false;
                    break;
                }
            }

            if (isMatch) {
                return it->func;
            }
        }
    }

    Type *type = type_new(TYPE_FUNC);
    type->size = POINTER_SIZE;
    type->align = POINTER_ALIGN;
    type->func.args = calloc(num_params, sizeof(Type *));
    memcpy(type->func.args, params, num_params * sizeof(Type*));
    type->func.num_args = num_params;
    type->func.ret = ret;
    buf_push(func_type_cache, ((FuncTypeCached){params, num_params, ret, type}));
    return type;
}

Type* type_incomplete(Entity *entity) {
    Type *type = type_new(TYPE_INCOMPLETE);
    type->entity = entity;
    return type;
}

#define MAX_LOCAL_ENTITIES 1024

Entity **global_entities;
Entity local_entities[MAX_LOCAL_ENTITIES];
Entity *local_entities_end = local_entities;
Entity **entities_ordered;

Type* resolve_typespec(Typespec *typespec);

void type_complete_struct(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        type->size = it->type->size + ALIGN_UP(type->size, it->type->align);
        type->align = MAX(type->align, it->type->align);
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
}

void type_complete_union(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_UNION;
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        type->size = MAX(it->type->size, type->size);
        type->align = MAX(it->type->align, type->align);
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
}

bool check_duplicate_fields(TypeField *fields, size_t num_fields) {
    for (size_t i = 0; i < num_fields; i++) {
        for (size_t j = i + 1; j < num_fields; ++j) {
            if (fields[i].name == fields[j].name) {
                return true;
            }
        }
    }
    return false;
}

void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal_syntax("Type dependence cycle");
    }

    if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    type->kind = TYPE_COMPLETING;
    Declaration *decl = type->entity->decl;
    TypeField *fields = NULL;

    for (AggregateItem *it = decl->aggregate.items; it != decl->aggregate.items + decl->aggregate.num_items; it++) {
        Type *item_type = resolve_typespec(it->type);
        complete_type(item_type);
        for (const char **name = it->names; name != it->names + it->num_names; name++) {
            buf_push(fields, ((TypeField){*name, item_type}));
        }
    }
    if (buf_len(fields) == 0) {
        fatal_syntax("There no fields in struct");
    }
    if (check_duplicate_fields(fields, buf_len(fields))) {
        fatal_syntax("Duplicate fields in union and struct not allowed");
    }
    if (decl->kind == DECL_STRUCT) {
        type_complete_struct(type, fields, buf_len(fields));
    } else {
        type_complete_union(type, fields, buf_len(fields));
    }

    buf_push(entities_ordered, type->entity);
}

Entity* entity_new(EntityKind kind, const char *name, Declaration *declaration) {
    Entity *entity = calloc(1, sizeof(Entity));
    entity->kind = kind;
    entity->name = name;
    entity->decl = declaration;
    return entity;
}

Entity* entity_declaration(Declaration *declaration) {
    EntityKind kind = ENTITY_NONE;
    switch (declaration->kind) {
        case DECL_ENUM:
        case DECL_TYPEDEF:
        case DECL_STRUCT:
        case DECL_UNION:
            kind = ENTITY_TYPE;
            break;
        case DECL_VAR:
            kind = ENTITY_VAR;
            break;
        case DECL_CONST:
            kind = ENTITY_CONST;
            break;
        case DECL_FUNC:
            kind = ENTITY_FUNC;
            break;
        default:
            break;
    }

    Entity *entity = entity_new(kind, declaration->name, declaration);
    if (declaration->kind == DECL_STRUCT || declaration->kind == DECL_UNION) {
        entity->state = ENTITY_RESOLVED;
        entity->type = type_incomplete(entity);
    }
    return entity;
}

Entity *entity_enum_const(const char *name, Declaration *declaration) {
    return entity_new(ENTITY_ENUM_CONST, name, declaration);
}

Entity* entity_get(const char *name) {
    for (Entity *it = local_entities_end; it != local_entities; it--) {
        Entity *entity = it - 1;
        if (entity->name == name) {
            return entity;
        }
    }
    for (Entity **it = global_entities; it != buf_end(global_entities); it++) {
        if ((*it)->name == name) {
            return *it;
        }
    }
    return NULL;
}

void local_entities_push_var(const char *name, Type *type) {
    if (local_entities_end == local_entities + MAX_LOCAL_ENTITIES) {
        fatal_syntax("Too many local entities");
    }
    *local_entities_end++ = (Entity){
        .name = name,
        .type = type,
        .kind = ENTITY_VAR,
        .state = ENTITY_RESOLVED
    };
}

Entity *local_scope_enter() {
    return local_entities_end;
}

void local_scope_leave(Entity *ptr_end) {
    local_entities_end = ptr_end;
}

Entity* entity_append_declaration(Declaration *declaration) {
    Entity *entity = entity_declaration(declaration);
    declaration->entity = entity;
    buf_push(global_entities, entity);
    if (declaration->kind == DECL_ENUM) {
        for (EnumItem *it = declaration->enum_delc.items; it != declaration->enum_delc.items + declaration->enum_delc.num_items; it++) {
            buf_push(global_entities, entity_enum_const(it->name, declaration));
        }
    }
    return entity;
}

Entity* entity_append_type(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_TYPE, name, NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    buf_push(global_entities, entity);
    return entity;
}


int64_t eval_unary_int(TokenKind op, int64_t val) {
    switch (op) {
        case TOKEN_ADD:
            return +val;
        case TOKEN_SUB:
            return -val;
        case TOKEN_BIN_NOT:
            return ~val;
        case TOKEN_NOT:
            return !val;
        default:
            assert(0);
            return 0;
    }
}

int64_t evan_binary_int (TokenKind op, int64_t left, int64_t right) {
    switch (op) {
        case TOKEN_MUL:
            return left * right;
        case TOKEN_DIV:
            return right != 0 ? left / right : 0;
        case TOKEN_MOD:
            return right != 0 ? left % right : 0;
        case TOKEN_BIN_AND:
            return left & right;
        case TOKEN_LSHIFT:
            return left << right;
        case TOKEN_RSHIFT:
            return left >> right;
        case TOKEN_ADD:
            return left + right;
        case TOKEN_SUB:
            return left - right;
        case TOKEN_BIN_OR:
            return left | right;
        case TOKEN_XOR:
            return left ^ right;
        case TOKEN_EQ:
            return left && right;
        case TOKEN_NOTEQ:
            return left != right;
        case TOKEN_LT:
            return left < right;
        case TOKEN_LTEQ:
            return left <= right;
        case TOKEN_GT:
            return left > right;
        case TOKEN_GTEQ:
            return left >= right;
        case TOKEN_AND:
            return left && right;
        case TOKEN_OR:
            return left || right;
        default:
            assert(0);
            return 0;
    }
}

Entity* resolve_entity_name(const char *name);
ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type);
ResolvedExpression resolve_expression(Expression *expr);
int64_t resolve_const_expression(Expression *expr);
void resolve_statement(Statement *stmt, Type *ret_type);

ResolvedExpression pointer_decay(ResolvedExpression expr) {
    if (expr.type->kind == TYPE_ARRAY) {
        return resolved_rvalue(type_pointer(expr.type->array.base));
    } else {
        return expr;
    }
}

ResolvedExpression resolve_expression_name(Expression *expr) {
    Entity *entity = resolve_entity_name(expr->name);
    if (entity->kind == ENTITY_VAR) {
        return resolved_lvalue(entity->type);
    } else if (entity->kind == ENTITY_CONST) {
        return resolved_const(entity->val);
    } else if (entity->kind == ENTITY_FUNC) {
        return resolved_rvalue(entity->type);
    } else {
        fatal_syntax("%s must be var or const");
    }
}

ResolvedExpression resolve_expression_unary(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->unary.operand);
    switch (expr->unary.op) {
        case TOKEN_MUL:
            operand = pointer_decay(operand);
            if (operand.type->kind != TYPE_POINTER) {
                fatal_syntax("Can't dereference non pointer typespec");
            }
            return resolved_lvalue(operand.type->pointer.base);
        case TOKEN_BIN_AND:
            if (!operand.is_lvalue) {
                fatal_syntax("Can't take address of non-lvalue");
            }
            return (ResolvedExpression){type_pointer(operand.type)};
        default:
            if (operand.type->kind != TYPE_INT) {
                fatal_syntax("%s is working yet for ints", token_kind_names[expr->unary.op]);
            }

            if (operand.is_const) {
                return resolved_const(eval_unary_int(expr->unary.op, operand.val));
            } else {
                resolved_rvalue(operand.type);
            }
    }

    return (ResolvedExpression){operand.type->pointer.base};
}

ResolvedExpression resolve_expression_binary(Expression *expr) {
    ResolvedExpression left = resolve_expression(expr->binary.left);
    ResolvedExpression right = resolve_expression(expr->binary.right);
    if (left.type != type_int || right.type != type_int) {
        fatal_syntax("%s is working yet for ints", token_kind_names[expr->binary.op]);
    }
    if (left.is_const && right.is_const) {
        return resolved_const(evan_binary_int(expr->binary.op, left.val, right.val));
    } else {
        return resolved_rvalue(left.type);
    }
}

ResolvedExpression resolve_expression_field(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->field.operand);
    complete_type(operand.type);
    if (operand.type->kind != TYPE_STRUCT && operand.type->kind != TYPE_UNION) {
        fatal_syntax("Can only access fields on aggregate types");
    }

    for (TypeField *it = operand.type->aggregate.fields; it != operand.type->aggregate.fields + operand.type->aggregate.num_fields; it++) {
        if (it->name == expr->field.name) {
            return operand.is_lvalue ? resolved_lvalue(it->type) : resolved_rvalue(it->type);
        }
    }

    fatal_syntax("No field named %s on typespec", expr->field.name);
}

size_t aggregate_field_index(Type *type, const char *name) {
    for (size_t i =0; i < type->aggregate.num_fields; i++) {
        if (type->aggregate.fields[i].name == name) {
            return i;
        }
    }
    fatal_syntax("Field %s in compound literal not found in struct or union", name);
    return 0;
}

ResolvedExpression resolve_expression_compound(Expression *expr, Type *expected_type) {
    if (!expected_type && !expr->compound.type) {
        fatal_syntax("Impossible to determine typespec of compound literal");
    }

    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
    } else {
        type = expected_type;
    }
    complete_type(type);
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_ARRAY) {
        fatal_syntax("Compound literals can only be user with struct, union or array types");
    }

    if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
        size_t index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == COMPOUNDFIELD_INDEX) {
                fatal_syntax("Index in compound literal for struct or union not allowed");
            } else if (field.kind == COMPOUNDFIELD_NAME) {
                index = aggregate_field_index(type, field.name);
            }
            if (index >= type->aggregate.num_fields) {
                fatal_syntax("Field initializer in struct or union compound out of range");
            }

            ResolvedExpression init = resolve_expression_expected_type(expr->compound.fields[i].init, type->aggregate.fields[index].type);
            if (init.type != type->aggregate.fields[index].type) {
                fatal_syntax("Compound literal field typespec mismatch");
            }
            index++;
        }
    } else {
        size_t index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == COMPOUNDFIELD_NAME) {
                fatal_syntax("Named field not allowed for array");
            } else if (field.kind == COMPOUNDFIELD_INDEX) {
                int64_t tmp_indx= (size_t)resolve_const_expression(field.index);
                 if (tmp_indx < 0) {
                     fatal_syntax("Field initializer index can't be negative");
                 }
                 index = (size_t)tmp_indx;
            }

            if (index >= type->aggregate.num_fields) {
                fatal_syntax("Field initialzer in array out of range");
            }
            index++;
        }
    }
    return resolved_rvalue(type);
}

ResolvedExpression resolve_expression_call(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->call.operand);
    complete_type(operand.type);
    if (operand.type->kind != TYPE_FUNC) {
        fatal_syntax("Calling non-function value");
    }

    if (expr->call.num_args != operand.type->func.num_args) {
        fatal_syntax("Calling function with wrong number of arguments ");
    }

    for (size_t i = 0; i < expr->call.num_args; i++) {
        Type *param = operand.type->func.args[i];
        ResolvedExpression arg = resolve_expression_expected_type(expr->call.args[i], param);
        if (arg.type != param) {
            fatal_syntax("Call argument expression typespec doesn't match expected typespec");
        }
    }

    return resolved_rvalue(operand.type->func.ret);
}

ResolvedExpression resolve_expression_ternary(Expression *expr, Type *expected_type) {
    ResolvedExpression cond = pointer_decay(resolve_expression(expr->ternary.cond));
    if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_POINTER) {
        fatal_syntax("Condition expression of ternary operator ? must have typespec int or ptr");
    }

    ResolvedExpression then_ex = resolve_expression_expected_type(expr->ternary.then_ex, expected_type);
    ResolvedExpression else_ex = resolve_expression_expected_type(expr->ternary.else_ex, expected_type);

    if (then_ex.type != else_ex.type) {
        fatal_syntax("Then and else expression of ternary operator ? must have matching types");
    }

    if (cond.is_const && then_ex.is_const && else_ex.is_const) {
        return resolved_const(cond.val ? then_ex.val : else_ex.val);
    } else {
        return resolved_rvalue(then_ex.type);
    }
}

ResolvedExpression resolve_expression_index(Expression *expr) {
    ResolvedExpression operand = pointer_decay(resolve_expression(expr->index.operand));
    if (operand.type->kind != TYPE_POINTER) {
        fatal_syntax("Can only index arrays or pointers");
    }
    ResolvedExpression index = resolve_expression(expr->index.index);

    if (index.type->kind != TYPE_INT) {
        fatal_syntax("Index must have typespec int");
    }
    return resolved_lvalue(operand.type->pointer.base);
}

ResolvedExpression resolve_expression_cast(Expression *expr) {
    Type *type = resolve_typespec(expr->cast.typespec);
    ResolvedExpression result = pointer_decay(resolve_expression(expr->cast.expr));
    if (type->kind == TYPE_POINTER) {
        if (result.type->kind != TYPE_POINTER && result.type->kind != TYPE_INT) {
            fatal_syntax("Invalid cast to pointer typespec");
        }
    } else if (type->kind == TYPE_INT) {
        if (result.type->kind != TYPE_POINTER && result.type->kind != TYPE_INT) {
            fatal_syntax("Invalid cast to int typespec");
        }
    } else {
        fatal_syntax("Invalid target cast typespec");
    }

    return resolved_rvalue(type);
}

ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type) {
    ResolvedExpression resolved;
    switch (expr->kind) {
        case EXPR_INT:
            resolved = resolved_const(expr->int_val);
            break;
        case EXPR_FLOAT:
            resolved = resolved_rvalue(type_float);
            break;
        case EXPR_STR:
            resolved = resolved_rvalue(type_pointer(type_char));
            break;
        case EXPR_NAME:
            resolved = resolve_expression_name(expr);
            break;
        case EXPR_UNARY:
            resolved = resolve_expression_unary(expr);
            break;
        case EXPR_BINARY:
            resolved = resolve_expression_binary(expr);
            break;
        case EXPR_SIZEOF_EXPR: {
            ResolvedExpression result = resolve_expression(expr->size_of_expr);
            Type *type = result.type;
            complete_type(type);
            resolved = resolved_const(type->size);
            break;
        }
        case EXPR_SIZEOF_TYPE: {
            Type *type = resolve_typespec(expr->size_of_type);
            complete_type(type);
            resolved = resolved_const(type->size);
            break;
        }
        case EXPR_FIELD:
            resolved = resolve_expression_field(expr);
            break;
        case EXPR_COMPOUND:
            resolved = resolve_expression_compound(expr, expected_type);
            break;
        case EXPR_CALL:
            resolved = resolve_expression_call(expr);
            break;
        case EXPR_TERNARY:
            resolved = resolve_expression_ternary(expr, expected_type);
            break;
        case EXPR_INDEX:
            resolved = resolve_expression_index(expr);
            break;
        case EXPR_CAST:
            resolved = resolve_expression_cast(expr);
            break;
        default:
            assert(0);
            break;
    }

    if (resolved.type) {
        expr->type = resolved.type;
    }
    return resolved;
}

ResolvedExpression resolve_expression(Expression *expr) {
    return resolve_expression_expected_type(expr, NULL);
}

int64_t resolve_const_expression(Expression *expr) {
    ResolvedExpression result = resolve_expression(expr);
    if (!result.is_const) {
        fatal_syntax("Expected constant expression");
    }
    return result.val;
}

void resolve_conditional_expression(Expression *expr) {
    ResolvedExpression cond = resolve_expression(expr);
    if (cond.type != type_int) {
        fatal_syntax("Conditional expression must have typespec int");
    }
}

void resolve_statement_block(StatementBlock block, Type *ret_type) {
    Entity *entities = local_scope_enter();
    for (size_t i = 0; i < block.num_statements; i++) {
        resolve_statement(block.statements[i], ret_type);
    }
    local_scope_leave(entities);
}

void resolve_statement(Statement *stmt, Type *ret_type) {
    switch (stmt->kind) {
        case STMT_IF:
            resolve_conditional_expression(stmt->if_stmt.cond);
            resolve_statement_block(stmt->if_stmt.then, ret_type);
            for (size_t i = 0; i < stmt->if_stmt.num_else_ifs; i++) {
                ElseIf else_if = stmt->if_stmt.else_ifs[i];
                resolve_conditional_expression(else_if.cond);
                resolve_statement_block(else_if.body, ret_type);
            }
            if (stmt->if_stmt.else_body.statements) {
                resolve_statement_block(stmt->if_stmt.else_body, ret_type);
            }
            break;
        case STMT_FOR: {
            Entity *entities = local_scope_enter();
            resolve_statement(stmt->for_stmt.init, ret_type);
            resolve_conditional_expression(stmt->for_stmt.cond);
            resolve_statement(stmt->for_stmt.next, ret_type);
            resolve_statement_block(stmt->for_stmt.body, ret_type);
            local_scope_leave(entities);
            break;
        }
        case STMT_WHILE:
        case STMT_DO_WHILE:
            resolve_conditional_expression(stmt->while_stmt.cond);
            resolve_statement_block(stmt->while_stmt.body, ret_type);
            break;
        case STMT_SWITCH: {
            ResolvedExpression expr = resolve_expression(stmt->switch_stmt.expr);
            for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
                SwitchCase _case = stmt->switch_stmt.cases[i];
                for (size_t j = 0; j < _case.num_expressions; j++) {
                    ResolvedExpression case_expr = resolve_expression(_case.expressions[j]);
                    if (case_expr.type != expr.type) {
                        fatal_syntax("Case expression in switch typespec mismatch");
                    }
                }
                resolve_statement_block(_case.body, ret_type);
            }
            break;
        }
        case STMT_ASSIGN: {
            ResolvedExpression left = resolve_expression(stmt->assign.left);
            if (stmt->assign.right) {
                ResolvedExpression right = resolve_expression_expected_type(stmt->assign.right, left.type);
                if (left.type != right.type) {
                    fatal_syntax("Left operand typespec of assignment doesn't match right operand");
                }
            }
            if (!left.is_lvalue) {
                fatal_syntax("Can't assign to non-lvalue");
            }

            if (stmt->assign.op != TOKEN_ASSIGN && left.type != type_int) {
                fatal_syntax("Assigment operator works only for ints");
            }
            break;
        }
        case STMT_AUTO_ASSIGN:
            local_entities_push_var(stmt->auto_assign.name, resolve_expression(stmt->auto_assign.init).type);
            break;
        case STMT_RETURN:
            if (stmt->expr) {
                ResolvedExpression ret = resolve_expression_expected_type(stmt->expr, ret_type);
                if (ret.type != ret_type) {
                    fatal_syntax("Return typespec mismatch");
                }
            } else {
                if (ret_type != type_void) {
                    fatal_syntax("Empty return expression for function with non-void return typespec");
                }
            }
            break;
        case STMT_BREAK:
        case STMT_CONTINUE:
            break;
        default:
            assert(0);
            break;
    }
}

Type* resolve_typespec(Typespec *typespec) {
    if (!typespec) {
        return type_void;
    }
    Type *type = NULL;
    switch (typespec->kind) {
        case TYPESPEC_NAME: {
            Entity *entity = resolve_entity_name(typespec->name);
            if (entity->kind != ENTITY_TYPE) {
                fatal_error(typespec->location, "%s must be typespec", typespec->name);
            }
            type = entity->type;
            break;
        }
        case TYPESPEC_ARRAY:
            type = type_array(resolve_typespec(typespec->array.base), (size_t)resolve_const_expression(typespec->array.size));
            break;
        case TYPESPEC_POINTER:
            type = type_pointer(resolve_typespec(typespec->pointer.base));
            break;
        case TYPESPEC_FUNC: {
            Type **args = NULL;
            for (Typespec **it = typespec->func.args; it != typespec->func.args + typespec->func.num_args; it++) {
                buf_push(args, resolve_typespec(*it));
            }
            Type *ret = type_void;
            if (typespec->func.ret) {
                ret = resolve_typespec(typespec->func.ret);
            }
            type = type_func(args, buf_len(args), ret);
            break;
        }
        default:
            assert(0);
            break;
    }

    typespec->type = type;
    return type;
}

Type* resolve_declaration_type(Declaration *decl) {
    return resolve_typespec(decl->typedef_decl.type);
}

Type* resolve_declaration_var(Declaration *decl) {
    Type *type = NULL;

    if (decl->var.type) {
        type = resolve_typespec(decl->var.type);
    }

    if (decl->var.expr) {
        ResolvedExpression result = resolve_expression_expected_type(decl->var.expr, type);
        if (type && result.type != type) {
            fatal_syntax("Declared var types doesn't not match inferred typespec");
        }

        type = result.type;
    }

    complete_type(type);
    return type;
}

Type* resolve_declaration_const(Declaration *decl, int64_t *val) {
    ResolvedExpression result = resolve_expression(decl->const_decl.expr);
    if (!result.is_const) {
        fatal_syntax("Initial value for const is not a constant");
    }
    *val = result.val;
    return result.type;
}

Type* resolve_declaration_func(Declaration *decl) {
    Type **params = NULL;
    for (FuncParam *it = decl->func.params; it != decl->func.params + decl->func.num_params; it++) {
        buf_push(params, resolve_typespec(it->type));
    }
    Type *ret = type_void;
    if (decl->func.return_type) {
        ret = resolve_typespec(decl->func.return_type);
    }
    return type_func(params, buf_len(params), ret);
}

void resolve_func(Entity *entity) {
    Entity *entities = local_scope_enter();
    for (size_t i = 0; i < entity->decl->func.num_params; i++) {
        FuncParam param = entity->decl->func.params[i];
        local_entities_push_var(param.name, resolve_typespec(param.type));
    }
    resolve_statement_block(entity->decl->func.body, resolve_typespec(entity->decl->func.return_type));
    local_scope_leave(entities);
}

void resolve_entity(Entity *entity) {
    if (entity->state == ENTITY_RESOLVED) {
        return;;
    }

    if (entity->state == ENTITY_RESOLVING) {
        fatal_syntax("Cyclic dependency of declaration");
    }
    entity->state = ENTITY_RESOLVING;

    switch (entity->kind) {
        case ENTITY_TYPE:
            entity->type = resolve_declaration_type(entity->decl);
            break;
        case ENTITY_VAR:
            entity->type = resolve_declaration_var(entity->decl);
            break;
        case ENTITY_CONST:
            entity->type = resolve_declaration_const(entity->decl, &entity->val);
            break;
        case ENTITY_FUNC:
            entity->type = resolve_declaration_func(entity->decl);
            break;
        default:
            break;
    }

    entity->state = ENTITY_RESOLVED;
    buf_push(entities_ordered, entity);

}

Entity* resolve_entity_name(const char *name) {
    Entity *entity = entity_get(name);
    if (!entity) {
        fatal_syntax("Unknown name declaration");
    }

    resolve_entity(entity);
    return entity;
}

void complete_entity(Entity *entity) {
    resolve_entity(entity);
    if (entity->kind == ENTITY_TYPE) {
        complete_type(entity->type);
    } else if (entity->kind == ENTITY_FUNC) {
        resolve_func(entity);
    }
}

void complete_entities() {
    for (Entity **it = global_entities; it != buf_end(global_entities); it++) {
        complete_entity(*it);
    }
}

void init_entities() {
    entity_append_type(str_intern("void"), type_void);
    entity_append_type(str_intern("char"), type_char);
    entity_append_type(str_intern("int"), type_int);
    entity_append_type(str_intern("float"), type_float);
}

void entities_append_declaration_list(DeclarationList *decl_list) {
    for (size_t i = 0; i < decl_list->num_declarations; i++) {
        entity_append_declaration(decl_list->declarations[i]);
    }
}