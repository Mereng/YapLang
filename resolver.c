typedef enum TypeKind {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ARRAY,
    TYPE_POINTER,
    TYPE_FUNC
} TypeKind;

typedef struct Type Type;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

struct Type {
    TypeKind kind;
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

Type *type_new(TypeKind kind) {
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}

Type type_int_val = {TYPE_INT};
Type type_float_val = {TYPE_FLOAT};

Type *type_int_link = &type_int_val;
Type *type_float_link = &type_float_val;

typedef struct PointerTypeCached {
    Type *base;
    Type *pointer;
} PointerTypeCached;

PointerTypeCached *pointer_types_cache;

Type *type_ponter(Type *base) {
    for (PointerTypeCached *it = pointer_types_cache; it != buf_end(pointer_types_cache); it++) {
        if (it->base == base) {
            return it->pointer;
        }
    }

    Type *type = type_new(TYPE_POINTER);
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

Type *type_array(Type *base, size_t size) {
    for (ArrayTypeCached *it = array_type_cache; it != buf_end(array_type_cache); it++) {
        if (it->base == base && it->size == size) {
            return it->array;
        }
    }

    Type *type = type_new(TYPE_ARRAY);
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
    type->func.params = calloc(num_params, sizeof(Type *));
    memcpy(type->func.params, params, num_params * sizeof(Type*));
    type->func.num_params = num_params;
    type->func.ret = ret;
    buf_push(func_type_cache, ((FuncTypeCached){params, num_params, ret, type}));
    return type;
}

Type* type_struct(TypeField *fields, size_t num_fields) {
    Type *type = type_new(TYPE_STRUCT);
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    return type;
}
Type* type_union(TypeField *fields, size_t num_fields) {
    Type *type = type_new(TYPE_UNION);
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    return type;
}

typedef enum SymDeclState {
    SYMDECL_UNRESOLVED,
    SYMDECL_RESOLVING,
    SYMDECL_RESOLVED
} SymDeclState;

typedef struct SymDecl {
    const char *name;
    Declaration *decl;
    SymDeclState state;
} SymDecl;

SymDecl *syms;

SymDecl *symdecl_get(const char *name) {
    for (SymDecl *it = syms; it != buf_end(syms); it++) {
        if (it->name == name) {
            return it;
        }
    }
    return NULL;
}

void symdecl_add(Declaration *decl) {
    buf_push(syms, ((SymDecl){decl->name, decl, SYMDECL_UNRESOLVED}));
}

void resolve_symdecl(SymDecl *sym) {
    if (sym->state == SYMDECL_RESOLVED) {
        return;;
    }

    if (sym->state == SYMDECL_RESOLVING) {
        fatal("Cyclic dependency of declaration");
    }

}

SymDecl* resolve_symdecl_name(const char *name) {
    SymDecl *sym = symdecl_get(name);
    if (!sym) {
        fatal("Unknown name declaration");
    }

    resolve_symdecl(sym);
    return sym;
}

void resolve_syms() {
    for (SymDecl *it = syms; it != buf_end(syms); it++) {
        resolve_symdecl(it);
    }
}