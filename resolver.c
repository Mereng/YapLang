#include <ast.h>

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INT,
    TYPE_FLOAT,
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

Type type_int_val = {TYPE_INT, 4};
Type type_float_val = {TYPE_FLOAT, 4};

Type *type_int_link = &type_int_val;
Type *type_float_link = &type_float_val;

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
        .type = type_int_link,
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

Type *type_ponter(Type *base) {
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


Entity* resolve_entity_name(const char *name);
ResolvedExpression resolve_expression(Expression *expr);

ResolvedExpression resolve_expression_name(Expression *expr) {
    Entity *entity = resolve_entity_name(expr->name);
    if (entity->kind == ENTITY_VAR) {
        return resolved_lvalue(entity->type);
    } else if (entity->kind == ENTITY_CONST) {
        return resolved_const(entity->val);
    } else {
        fatal("%s must be var or const");
    }
}

ResolvedExpression resolve_expression_unary(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->unary.operand);
    switch (expr->unary.op) {
        case TOKEN_MUL:
            if (operand.type->kind != TYPE_POINTER) {
                fatal("Can't dereference non pointer type");
            }
            return resolved_lvalue(operand.type->pointer.base);
        case TOKEN_BIN_AND:
            if (!operand.is_lvalue) {
                fatal("Can't take address of non-lvalue");
            }
            return (ResolvedExpression){type_ponter(type_ponter(operand.type))};
        default:
            assert(0);
            break;
    }

    return (ResolvedExpression){operand.type->pointer.base};
}

ResolvedExpression resolve_expression_binary(Expression *expr) {
    assert(expr->binary.op == TOKEN_ADD);
    ResolvedExpression left = resolve_expression(expr->binary.left);
    ResolvedExpression right = resolve_expression(expr->binary.right);
    if (left.is_const && right.is_const) {
        return resolved_const(left.val + right.val);
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

ResolvedExpression resolve_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT:
            return resolved_const(expr->int_val);
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
        default:
            assert(0);
            break;
    }
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
            return type_ponter(resolve_typespec(typespec->pointer.base));
        case TYPESPEC_FUNC: {
            Type **args = NULL;
            for (Typespec **it = typespec->func.args_types; it != typespec->func.args_types + typespec->func.num_args_types; it++) {
                buf_push(args, resolve_typespec(*it));
            }
            return type_func(args, buf_len(args), resolve_typespec(typespec->func.return_type));
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
        ResolvedExpression result = resolve_expression(decl->var.expr);
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
    *val = result.val;
    return result.type;
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