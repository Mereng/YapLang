#include <ast.h>

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_CHAR,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ARRAY,
    TYPE_POINTER,
    TYPE_FUNC,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING
} TypeKind;

typedef struct Type Type;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;


typedef enum EntityState {
    ENTITY_UNRESOLVED,
    ENTITY_RESOLVING,
    ENTITY_RESOLVED
} EntityState;

typedef enum EntityKind {
    ENTITY_NONE,
    ENTITY_VAR,
    ENTITY_CONST,
    ENTITY_TYPE,
    ENTITY_ENUM_CONST,
    ENTITY_FUNC
} EntityKind;

typedef struct Entity {
    EntityKind kind;
    EntityState state;
    const char *name;
    Declaration *decl;
    Type *type;
    int64_t val;

} Entity;

struct Type {
    TypeKind kind;
    size_t size;
    Entity *entity;
    union {
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            Type *base;
            size_t size;
        } array;
        struct {
            Type *base;
        } pointer;
        struct {
            Type **params;
            size_t num_params;
            Type *ret;
        } func;
    };
};


Type *type_void = &(Type){TYPE_VOID, 0};
Type *type_int = &(Type){TYPE_INT, 4};
Type *type_float = &(Type){TYPE_FLOAT, 4};
Type *type_char = &(Type){TYPE_CHAR, 1};

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


Type *type_new(TypeKind kind) {
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}


typedef struct PointerTypeCached {
    Type *base;
    Type *pointer;
} PointerTypeCached;

PointerTypeCached *pointer_types_cache;

const size_t POINTER_SIZE = 8;

Type *type_pointer(Type *base) {
    for (PointerTypeCached *it = pointer_types_cache; it != buf_end(pointer_types_cache); it++) {
        if (it->base == base) {
            return it->pointer;
        }
    }

    Type *type = type_new(TYPE_POINTER);
    type->size = POINTER_SIZE;
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
    type->func.params = calloc(num_params, sizeof(Type *));
    memcpy(type->func.params, params, num_params * sizeof(Type*));
    type->func.num_params = num_params;
    type->func.ret = ret;
    buf_push(func_type_cache, ((FuncTypeCached){params, num_params, ret, type}));
    return type;
}

Type* type_incomplete(Entity *entity) {
    Type *type = type_new(TYPE_INCOMPLETE);
    type->entity = entity;
    return type;
}
Entity **entities;
Entity **entities_ordered;

Type* resolve_typespec(Typespec *typespec);

void type_complete_struct(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_STRUCT;
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        type->size += it->type->size;
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
        fatal("Type dependence cycle");
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
    if (check_duplicate_fields(fields, buf_len(fields))) {
        fatal("Duplicate fields in union and struct not allowed");
    }
    if (decl->kind == DECL_STRUCT) {
        type_complete_struct(type, fields, buf_len(fields));
    } else {
        type_complete_union(type, fields, buf_len(fields));
    }

    buf_push(entities_ordered, type->entity);
}

Entity* entity_new(EntityKind kind, const char *name, Declaration *declaration) {
    Entity *entity = calloc(1, sizeof(Declaration));
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
    for (Entity **it = entities; it != buf_end(entities); it++) {
        if ((*it)->name == name) {
            return *it;
        }
    }
    return NULL;
}

Entity* entity_append_declaration(Declaration *declaration) {
    Entity *entity = entity_declaration(declaration);
    buf_push(entities, entity);
    if (declaration->kind == DECL_ENUM) {
        for (EnumItem *it = declaration->enum_delc.items; it != declaration->enum_delc.items + declaration->enum_delc.num_items; it++) {
            buf_push(entities, entity_enum_const(it->name, declaration));
        }
    }
    return entity;
}

Entity* entity_append_type(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_TYPE, name, NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    buf_push(entities, entity);
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
        fatal("%s must be var or const");
    }
}

ResolvedExpression resolve_expression_unary(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->unary.operand);
    switch (expr->unary.op) {
        case TOKEN_MUL:
            operand = pointer_decay(operand);
            if (operand.type->kind != TYPE_POINTER) {
                fatal("Can't dereference non pointer type");
            }
            return resolved_lvalue(operand.type->pointer.base);
        case TOKEN_BIN_AND:
            if (!operand.is_lvalue) {
                fatal("Can't take address of non-lvalue");
            }
            return (ResolvedExpression){type_pointer(operand.type)};
        default:
            if (operand.type->kind != TYPE_INT) {
                fatal("%s is working yet for ints", token_kind_names[expr->unary.op]);
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
        fatal("%s is working yet for ints", token_kind_names[expr->binary.op]);
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
        fatal("Can only access fields on aggregate types");
    }

    for (TypeField *it = operand.type->aggregate.fields; it != operand.type->aggregate.fields + operand.type->aggregate.num_fields; it++) {
        if (it->name == expr->field.name) {
            return operand.is_lvalue ? resolved_lvalue(it->type) : resolved_rvalue(it->type);
        }
    }

    fatal("No field named %s on type", expr->field.name);
}

ResolvedExpression resolve_expression_compound(Expression *expr, Type *expected_type) {
    if (!expected_type && !expr->compound.type) {
        fatal("Impossible to determine type of compound literal");
    }

    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
        if (expected_type && expected_type != type) {
            fatal("Explicit compound literal doesn't match expected type");
        }
    } else {
        type = expected_type;
    }
    complete_type(type);
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_ARRAY) {
        fatal("Compound literals can only be user with struct, union or array types");
    }

    if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
        if (expr->compound.num_args > type->aggregate.num_fields) {
            fatal("Compound literal has to many fields");
        }

        for (size_t i = 0; i < expr->compound.num_args; i++) {
            ResolvedExpression field = resolve_expression(expr->compound.args[i]);
            if (field.type != type->aggregate.fields[i].type) {
                fatal("Compound literal field type mismatch");
            }
        }
    } else {
        if (expr->compound.num_args > type->array.size) {
            fatal("Compound literal has too many elements");
        }
        for (size_t i = 0; i < expr->compound.num_args; i++) {
            ResolvedExpression elem = resolve_expression(expr->compound.args[i]);
            if (elem.type != type->array.base) {
                fatal("Compound literal element type mismatch");
            }
        }
    }
    return resolved_rvalue(type);
}

ResolvedExpression resolve_expression_call(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->call.operand);
    complete_type(operand.type);
    if (operand.type->kind != TYPE_FUNC) {
        fatal("Calling non-function value");
    }

    if (expr->call.num_args != operand.type->func.num_params) {
        fatal("Calling function with wrong number of arguments ");
    }

    for (size_t i = 0; i < expr->call.num_args; i++) {
        Type *param = operand.type->func.params[i];
        ResolvedExpression arg = resolve_expression_expected_type(expr->call.args[i], param);
        if (arg.type != param) {
            fatal("Call argument expression type doesn't match expected type");
        }
    }

    return resolved_rvalue(operand.type->func.ret);
}

ResolvedExpression resolve_expression_ternary(Expression *expr, Type *expected_type) {
    ResolvedExpression cond = pointer_decay(resolve_expression(expr->ternary.cond));
    if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_POINTER) {
        fatal("Condition expression of ternary operator ? must have type int or ptr");
    }

    ResolvedExpression then_ex = resolve_expression_expected_type(expr->ternary.then_ex, expected_type);
    ResolvedExpression else_ex = resolve_expression_expected_type(expr->ternary.else_ex, expected_type);

    if (then_ex.type != else_ex.type) {
        fatal("Then and else expression of ternary operator ? must have matching types");
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
        fatal("Can only index arrays or pointers");
    }
    ResolvedExpression index = resolve_expression(expr->index.index);

    if (index.type->kind != TYPE_INT) {
        fatal("Index must have type int");
    }
    return resolved_lvalue(operand.type->pointer.base);
}

ResolvedExpression resolve_expression_cast(Expression *expr) {
    Type *type = resolve_typespec(expr->cast.type);
    ResolvedExpression result = pointer_decay(resolve_expression(expr->cast.expr));
    if (type->kind == TYPE_POINTER) {
        if (result.type->kind != TYPE_POINTER && result.type->kind != TYPE_INT) {
            fatal("Invalid cast to pointer type");
        }
    } else if (type->kind == TYPE_INT) {
        if (result.type->kind != TYPE_POINTER && result.type->kind != TYPE_INT) {
            fatal("Invalid cast to int type");
        }
    } else {
        fatal("Invalid target cast type");
    }

    return resolved_rvalue(type);
}

ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type) {
    switch (expr->kind) {
        case EXPR_INT:
            return resolved_const(expr->int_val);
        case EXPR_FLOAT:
            return resolved_rvalue(type_float);
        case EXPR_STR:
            return resolved_rvalue(type_pointer(type_char));
        case EXPR_NAME:
            return resolve_expression_name(expr);
        case EXPR_UNARY:
            return resolve_expression_unary(expr);
        case EXPR_BINARY:
            return resolve_expression_binary(expr);
        case EXPR_SIZEOF_EXPR: {
            ResolvedExpression result = resolve_expression(expr->size_of_expr);
            Type *type = result.type;
            complete_type(type);
            return resolved_const(type->size);
        }
        case EXPR_SIZEOF_TYPE: {
            Type *type = resolve_typespec(expr->size_of_type);
            complete_type(type);
            return resolved_const(type->size);
        }
        case EXPR_FIELD:
            return resolve_expression_field(expr);
        case EXPR_COMPOUND:
            return resolve_expression_compound(expr, expected_type);
        case EXPR_CALL:
            return resolve_expression_call(expr);
        case EXPR_TERNARY:
            return resolve_expression_ternary(expr, expected_type);
        case EXPR_INDEX:
            return resolve_expression_index(expr);
        case EXPR_CAST:
            return resolve_expression_cast(expr);
        default:
            assert(0);
            break;
    }
}

ResolvedExpression resolve_expression(Expression *expr) {
    return resolve_expression_expected_type(expr, NULL);
}

ResolvedExpression resolve_const_expression(Expression *expr) {
    ResolvedExpression result = resolve_expression(expr);
    if (!result.is_const) {
        fatal("Expected constant expression");
    }
    return result;
}


Type* resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
        case TYPESPEC_NAME: {
            Entity *entity = resolve_entity_name(typespec->name);
            if (entity->kind != ENTITY_TYPE) {
                fatal("%s must be type", typespec->name);
            }
            return entity->type;
        }
        case TYPESPEC_ARRAY:
            return type_array(resolve_typespec(typespec->array.base), (size_t)resolve_const_expression(typespec->array.size).val);
        case TYPESPEC_POINTER:
            return type_pointer(resolve_typespec(typespec->pointer.base));
        case TYPESPEC_FUNC: {
            Type **args = NULL;
            for (Typespec **it = typespec->func.args_types; it != typespec->func.args_types + typespec->func.num_args_types; it++) {
                buf_push(args, resolve_typespec(*it));
            }
            Type *ret = type_void;
            if (typespec->func.return_type) {
                ret = resolve_typespec(typespec->func.return_type);
            }
            return type_func(args, buf_len(args), ret);
        }
        default:
            break;
    }
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
            fatal("Declared var types doesn't not match inferred type");
        }

        type = result.type;
    }

    complete_type(type);
    return type;
}

Type* resolve_declaration_const(Declaration *decl, int64_t *val) {
    ResolvedExpression result = resolve_expression(decl->const_decl.expr);
    if (!result.is_const) {
        fatal("Initial value for const is not a constant");
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

void resolve_entity(Entity *entity) {
    if (entity->state == ENTITY_RESOLVED) {
        return;;
    }

    if (entity->state == ENTITY_RESOLVING) {
        fatal("Cyclic dependency of declaration");
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
        fatal("Unknown name declaration");
    }

    resolve_entity(entity);
    return entity;
}