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
    TYPE_INCOPLETE,
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

typedef struct ResolvedExpression {
    Type *type;
    bool is_const;
    int64_t val;
} ResolvedExpression;

Type *type_new(TypeKind kind) {
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}

Type type_int_val = {TYPE_INT, 4};
Type type_float_val = {TYPE_FLOAT, 4};

Type *type_int_link = &type_int_val;
Type *type_float_link = &type_float_val;

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

Type* type_struct(TypeField *fields, size_t num_fields) {
    Type *type = type_new(TYPE_STRUCT);
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        type->size += it->type->size;
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    return type;
}
Type* type_union(TypeField *fields, size_t num_fields) {
    Type *type = type_new(TYPE_UNION);
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        type->size += it->type->size;
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    return type;
}

Type* type_incomplete(Entity *entity) {
    Type *type = type_new(TYPE_INCOPLETE);
    type->entity = entity;
    return type;
}


Entity **entities;

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

Entity **entities_ordered;

ResolvedExpression resolve_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT:
            //..
        case EXPR_NAME:
            //..
        case EXPR_UNARY:
            //..
        case EXPR_BINARY:
            //..
    }
}

Entity* resolve_entity_name(const char *name);

Type* resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
        case TYPESPEC_NAME: {
            Entity *entity = resolve_entity_name(typespec->name);
            return entity->type;
        }
        case TYPESPEC_ARRAY:
            //...
            return type_array(resolve_typespec(typespec->array.base));
        case TYPESPEC_POINTER:
            return type_ponter(resolve_typespec(typespec->pointer.base));
        case TYPESPEC_FUNC:
            break;
        default:
            break;
    }
}

Type* resolve_declaration_type(Declaration *declaration) {

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
            break;
        case ENTITY_VAR:
            break;
        case ENTITY_CONST:
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

void resolve_entities() {
    for (Entity **it = entities; it != buf_end(entities); it++) {
        resolve_entity(*it);
    }
}