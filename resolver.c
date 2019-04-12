DeclarationList *global_declaration_list;

Type *type_void = &(Type){TYPE_VOID, 0};
Type *type_char = &(Type){TYPE_CHAR, 1, 1};
Type *type_schar = &(Type){TYPE_SCHAR, 1, 1};
Type *type_uchar = &(Type){TYPE_UCHAR, 1, 1};
Type *type_bool = &(Type){TYPE_BOOL, 1, 1};
Type *type_short = &(Type){TYPE_SHORT, 2, 2};
Type *type_ushort = &(Type){TYPE_USHORT, 2, 2};
Type *type_int = &(Type){TYPE_INT, 4, 4};
Type *type_uint = &(Type){TYPE_UINT, 4, 4};
Type *type_long = &(Type){TYPE_LONG, 4, 4};
Type *type_ulong = &(Type){TYPE_ULONG, 4, 4};
Type *type_llong = &(Type){TYPE_LLONG, 8, 8};
Type *type_ullong = &(Type){TYPE_ULONG, 8, 8};
Type *type_float = &(Type){TYPE_FLOAT, 4, 4};
Type *type_double = &(Type){TYPE_DOUBLE, 8, 8};

#define type_usize type_ullong
#define type_size type_llong
#define type_uintptr type_ullong

Type *type_pointer(Type *base);
void complete_type(Type *type);
Type* unqualify_type(Type *type);

typedef struct ResolvedExpression {
    Type *type;
    bool is_lvalue;
    bool is_const;
    Value val;
} ResolvedExpression;

ResolvedExpression resolved_rvalue(Type *type) {
    return (ResolvedExpression) {
        .type = unqualify_type(type),
    };
}

ResolvedExpression resolved_lvalue(Type *type) {
    return (ResolvedExpression) {
        .type = type,
        .is_lvalue = true
    };
}

ResolvedExpression resolved_const(Type *type, Value val) {
    return (ResolvedExpression) {
        .type = unqualify_type(type),
        .is_const = true,
        .val = val
    };
}

ResolvedExpression resolved_decay(ResolvedExpression operand) {
    operand.type = unqualify_type(operand.type);
    if (operand.type->kind == TYPE_ARRAY) {
        operand.type = type_pointer(operand.type->base);
    }
    operand.is_lvalue = false;
    return operand;
}

bool is_integer_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_ENUM;
}

bool is_floating_type(Type *type) {
    return TYPE_FLOAT <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_math_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_scalar_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_FUNC;
}

bool is_null_pointer(ResolvedExpression operand);

bool is_convertible(ResolvedExpression *operand, Type *dest) {
    dest = unqualify_type(dest);
    Type *src = unqualify_type(operand->type);
    if (dest == src) {
        return true;
    } else if (is_math_type(dest) && is_math_type(src)) {
        return true;
    } else if (dest->kind == TYPE_POINTER && src->kind == TYPE_POINTER) {
        if (dest->base->kind == TYPE_CONST && src->base->kind == TYPE_CONST) {
            return dest->base->base == src->base->base || dest->base->base == type_void || src->base->base == type_void;
        } else {
            Type *unqual_dest_base = unqualify_type(dest->base);
            if (unqual_dest_base == src->base) {
                return true;
            } else if (unqual_dest_base == type_void) {
                return dest->base->kind == TYPE_CONST || src->base->kind != TYPE_CONST;
            } else {
                return src->base == type_void;
            }
        }
    } else if (is_null_pointer(*operand) && dest->kind == TYPE_POINTER) {
        return true;
    }

    return false;
}

bool is_castable(ResolvedExpression *operand, Type *dest) {
    Type *src = operand->type;
    if (is_convertible(operand, dest)) {
        return true;
    } else if (is_integer_type(dest)) {
        return src->kind == TYPE_POINTER;
    } else if (is_integer_type(src)) {
        return dest->kind == TYPE_POINTER;
    } else if (dest->kind == TYPE_POINTER && src->kind == TYPE_POINTER) {
        return true;
    }

    return false;
}

#define CASE(k, t) \
    case k: \
        switch (type->kind) { \
            case TYPE_CHAR: \
                operand->val.c = (char)operand->val.t; \
                break; \
            case TYPE_SCHAR: \
                operand->val.sc = (signed char)operand->val.t; \
                break; \
            case TYPE_UCHAR: \
                operand->val.uc = (unsigned char)operand->val.t; \
                break; \
            case TYPE_BOOL: \
                operand->val.b = (bool)operand->val.t; \
                break; \
            case TYPE_SHORT:\
                operand->val.s = (short)operand->val.t; \
                break; \
            case TYPE_USHORT: \
                operand->val.us = (unsigned short)operand->val.t; \
                break; \
            case TYPE_INT: \
                operand->val.i = (int)operand->val.t; \
                break; \
            case TYPE_UINT: \
                operand->val.ui = (unsigned int)operand->val.t; \
                break; \
            case TYPE_LONG: \
                operand->val.l = (long)operand->val.t; \
                break; \
            case TYPE_ULONG: \
                operand->val.ul = (unsigned long)operand->val.t; \
                break; \
            case TYPE_LLONG: \
                operand->val.ll = (long long)operand->val.t; \
                break; \
            case TYPE_ULLONG: \
                operand->val.ull = (unsigned long long)operand->val.t; \
                break; \
            case TYPE_POINTER: \
                operand->val.p = (uintptr_t)operand->val.t; \
                break; \
            case TYPE_FLOAT: \
            case TYPE_DOUBLE: \
                break; \
            default: \
                operand->is_const = false; \
                break; \
        } \
        break;

bool cast_expression(ResolvedExpression *operand, Type *type) {
    Type *limited_type = type;
    type = unqualify_type(type);
    operand->type = unqualify_type(operand->type);
    if (operand->type != type) {
        if (!is_castable(operand, operand->type)) {
            return false;
        }

        if (operand->is_const) {
            if (is_floating_type(operand->type)) {
                operand->is_const = !is_integer_type(type);
            } else {
                switch (operand->type->kind) {
                    CASE(TYPE_BOOL, b)
                    CASE(TYPE_CHAR, c)
                    CASE(TYPE_SCHAR, sc)
                    CASE(TYPE_UCHAR, uc)
                    CASE(TYPE_SHORT, s)
                    CASE(TYPE_USHORT, us)
                    CASE(TYPE_INT, i)
                    CASE(TYPE_UINT, ui)
                    CASE(TYPE_LONG, l)
                    CASE(TYPE_ULONG, ul)
                    CASE(TYPE_LLONG, ll)
                    CASE(TYPE_ULLONG, ull)
                    CASE(TYPE_POINTER, p)
                    default:
                        operand->is_const = false;
                        break;
                }
            }
        }
    }
    operand->type = limited_type;
    return true;
}

#undef CASE

bool convert_expression(ResolvedExpression *operand, Type *type) {
    if (is_convertible(operand, type)) {
        cast_expression(operand, type);
        *operand = resolved_rvalue(operand->type);
        return true;
    }
    return false;
}

Value convert_const_expression(Type *src, Type *dest, Value val) {
    ResolvedExpression operand = resolved_const(src, val);
    cast_expression(&operand, dest);
    return operand.val;
}

bool is_null_pointer(ResolvedExpression operand) {
    if (operand.is_const && (operand.type->kind == TYPE_POINTER || is_integer_type(operand.type))) {
        cast_expression(&operand, type_ullong);
        return operand.val.ull == 0;
    } else {
        return false;
    }
}

void promote_expression(ResolvedExpression *operand) {
    switch (operand->type->kind) {
        case TYPE_CHAR:
        case TYPE_SCHAR:
        case TYPE_UCHAR:
        case TYPE_SHORT:
        case TYPE_USHORT:
            cast_expression(operand, type_int);
            break;
        default:
            break;
    }
}

Map pointer_types_cache;

const size_t POINTER_SIZE = 8;
const size_t POINTER_ALIGN = 8;

Type *type_pointer(Type *base) {
    Type *type = map_get(&pointer_types_cache, base);
    if (!type) {
        type = type_new(TYPE_POINTER);
        type->size = POINTER_SIZE;
        type->align = POINTER_ALIGN;
        type->base = base;
        map_put(&pointer_types_cache, base, type);
    }
    return type;
}

typedef struct ArrayTypeCached {
    Type *type;
    struct ArrayTypeCached *next;
} ArrayTypeCached;

Map array_type_cache;

Type* type_array(Type *base, size_t size) {
    uint64_t hash = hash_mix(hash_pointer(base), hash_uint64(size));
    void *key = (void*)(hash ? hash : 1);
    ArrayTypeCached *cached = map_get(&array_type_cache, key);
    for (ArrayTypeCached *it = cached; it; it = it->next) {
        Type *type = it->type;
        if (type->base == base && type->num_elements == size) {
            return type;
        }
    }

    complete_type(base);

    Type *type = type_new(TYPE_ARRAY);
    type->size = size * base->size;
    type->align = base->align;
    type->base = base;
    type->num_elements = size;
    type->is_nonmodify = base->is_nonmodify;
    ArrayTypeCached *new = malloc(sizeof(ArrayTypeCached));
    new->type = type;
    new->next = cached;
    map_put(&array_type_cache, key, new);
    return type;
}

Map const_types_cache;
Type* type_const(Type *base) {
    if (base->kind == TYPE_CONST) {
        return base;
    }

    Type *type = map_get(&const_types_cache, base);
    if (!type) {
        complete_type(base);
        type = type_new(TYPE_CONST);
        type->is_nonmodify = true;
        type->size = base->size;
        type->align = base->align;
        type->base = base;
        map_put(&const_types_cache, base, type);
    }
    return type;
}

typedef struct FuncTypeCached {
    Type *type;
    struct FuncTypeCached *next;
} FuncTypeCached;

Map func_type_cache;

Type* type_func(Type **args, size_t num_args, Type *ret, bool variadic) {
    size_t args_size = num_args * sizeof(*args);
    uint64_t hash = hash_mix(hash_bytes(args, args_size), hash_pointer(ret));
    void *key = (void*)(hash ? hash : 1);
    FuncTypeCached *cached = map_get(&func_type_cache, key);
    for (FuncTypeCached *it = cached; it; it = it->next) {
        Type *type = it->type;
        if (type->func.num_args == num_args && type->func.ret == ret && type->func.is_variadic == variadic) {
            if (memcmp(type->func.args, args, args_size) == 0) {
                return type;
            }
        }
    }

    Type *type = type_new(TYPE_FUNC);
    type->size = POINTER_SIZE;
    type->align = POINTER_ALIGN;
    type->func.args = calloc(num_args, sizeof(Type *));
    memcpy(type->func.args, args, num_args * sizeof(Type*));
    type->func.num_args = num_args;
    type->func.ret = ret;
    type->func.is_variadic = variadic;
    FuncTypeCached *new = malloc(sizeof(FuncTypeCached));
    new->type = type;
    new->next = cached;
    map_put(&func_type_cache, key, new);
    return type;
}

Type* type_incomplete(Entity *entity) {
    Type *type = type_new(TYPE_INCOMPLETE);
    type->entity = entity;
    return type;
}

Type* type_enum(Entity *entity) {
    Type *type = type_new(TYPE_ENUM);
    type->entity = entity;
    type->size = type_int->size;
    type->align = type_int->align;
    return type;
}

Map resolved_type_map;

Type* get_resolved_type(void *key) {
    return map_get(&resolved_type_map, key);
}
void set_resolved_type(void *key, Type *type) {
    map_put(&resolved_type_map, key, type);
}

Type* unqualify_type(Type *type) {
    if (type->kind == TYPE_CONST) {
        return type->base;
    } else {
        return type;
    }
}

bool issigned(Type *type) {
    switch (type->kind) {
        case TYPE_SCHAR:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
        case TYPE_LLONG:
            return true;
        default:
            return false;
    }
}

const char *type_names[TYPE_MAX] = {
    [TYPE_VOID] = "void",
    [TYPE_CHAR] = "char",
    [TYPE_SCHAR] = "schar",
    [TYPE_UCHAR] = "uchar",
    [TYPE_BOOL] = "bool",
    [TYPE_SHORT] = "short",
    [TYPE_USHORT] = "ushort",
    [TYPE_INT] = "int",
    [TYPE_UINT] = "uint",
    [TYPE_LONG] = "long",
    [TYPE_ULONG] = "ulong",
    [TYPE_LLONG] = "llong",
    [TYPE_ULLONG] = "ullong",
    [TYPE_FLOAT] = "float",
    [TYPE_DOUBLE] = "double",
};

int type_ranks[TYPE_MAX] = {
    [TYPE_BOOL] = 1,
    [TYPE_CHAR] = 2,
    [TYPE_SCHAR] = 2,
    [TYPE_UCHAR] = 2,
    [TYPE_SHORT] = 3,
    [TYPE_USHORT] = 3,
    [TYPE_INT] = 4,
    [TYPE_UINT] = 4,
    [TYPE_LONG] = 5,
    [TYPE_ULONG] = 5,
    [TYPE_LLONG] = 6,
    [TYPE_ULLONG] = 6
};

int type_rank(Type *type) {
    return type_ranks[type->kind];
}

Type* unsigned_type(Type *type) {
    switch (type->kind) {
        case TYPE_CHAR:
        case TYPE_SCHAR:
        case TYPE_UCHAR:
            return type_uchar;
        case TYPE_BOOL:
            return type_bool;
        case TYPE_SHORT:
        case TYPE_USHORT:
            return type_ushort;
        case TYPE_INT:
        case TYPE_UINT:
            return type_uint;
        case TYPE_LONG:
        case TYPE_ULONG:
            return type_ulong;
        case TYPE_LLONG:
        case TYPE_ULLONG:
            return type_ullong;
        default:
            assert(0);
            return NULL;
    }
}

Type* promote_type(Type *type) {
    ResolvedExpression operand = resolved_rvalue(type);
    promote_expression(&operand);
    return operand.type;
}

#define MAX_LOCAL_ENTITIES 1024

Map global_entities;
Entity **global_entities_buf;
Entity local_entities[MAX_LOCAL_ENTITIES];
Entity *local_entities_end = local_entities;
Entity **entities_ordered;

void global_entities_put(Entity *entity) {
    if (map_get(&global_entities, (void*)entity->name)) {
        SrcLocation location = entity->decl ? entity->decl->location : location_builtin;
        fatal_error(location, "Duplicate definition");
    }
    map_put(&global_entities, (void*)entity->name, entity);
    buf_push(global_entities_buf, entity);
}

Type* resolve_typespec(Typespec *typespec);

void type_complete_struct(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;
    bool is_nonmodify = false;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        it->offset = type->size;
        type->size = it->type->size + ALIGN_UP(type->size, it->type->align);
        type->align = MAX(type->align, it->type->align);
        is_nonmodify = is_nonmodify || it->type->is_nonmodify;
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    type->is_nonmodify = is_nonmodify;
}

void type_complete_union(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_UNION;
    type->size = 0;
    type->align = 0;
    bool is_nonmodify = false;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        it->offset = 0;
        type->size = MAX(it->type->size, type->size);
        type->align = MAX(it->type->align, type->align);
        is_nonmodify = is_nonmodify || it->type->is_nonmodify;
    }
    type->aggregate.fields = calloc(num_fields, sizeof(TypeField));
    memcpy(type->aggregate.fields, fields, num_fields * sizeof(TypeField));
    type->aggregate.num_fields = num_fields;
    type->is_nonmodify = is_nonmodify;
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
        fatal("Type dependence cycle %s", type->entity->name);
    }

    if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    Declaration *decl = type->entity->decl;
    if (decl->is_incomplete) {
        fatal_error(decl->location, "Can't use incomplete type");
    }
    type->kind = TYPE_COMPLETING;
    TypeField *fields = NULL;

    for (AggregateItem *it = decl->aggregate.items; it != decl->aggregate.items + decl->aggregate.num_items; it++) {
        Type *item_type = resolve_typespec(it->type);
        complete_type(item_type);
        if (item_type->size == 0) {
            fatal_error(it->location, "Field type of size 0 is not allowed");
        }
        for (const char **name = it->names; name != it->names + it->num_names; name++) {
            buf_push(fields, ((TypeField){*name, item_type}));
        }
    }
    if (buf_len(fields) == 0) {
        fatal("There no fields in struct %s", type->entity->name);
    }
    if (check_duplicate_fields(fields, buf_len(fields))) {
        fatal("%s: duplicate fields in union and struct not allowed", type->entity->name);
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

Entity* entity_get_local(const char *name) {
    for (Entity *it = local_entities_end; it != local_entities; it--) {
        Entity *entity = it - 1;
        if (entity->name == name) {
            return entity;
        }
    }
    return NULL;
}

Entity* entity_get(const char *name) {
    Entity *entity = entity_get_local(name);
    return entity ? entity : map_get(&global_entities, (void*)name);
}

bool local_entities_push_var(const char *name, Type *type) {
    if (entity_get_local(name)) {
        return false;
    }
    if (local_entities_end == local_entities + MAX_LOCAL_ENTITIES) {
        fatal("Too many local entities in %s", type->entity->name);
    }
    *local_entities_end++ = (Entity){
        .name = name,
        .type = type,
        .kind = ENTITY_VAR,
        .state = ENTITY_RESOLVED
    };
    return true;
}

Entity *local_scope_enter() {
    return local_entities_end;
}

void local_scope_leave(Entity *ptr_end) {
    local_entities_end = ptr_end;
}

void entity_append_const(const char *name, Type *type, Value val) {
    Entity *entity = entity_new(ENTITY_CONST, str_intern(name), NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    entity->val = val;
    global_entities_put(entity);
}

Entity* entity_append_declaration(Declaration *declaration) {
    Entity *entity = entity_declaration(declaration);
    global_entities_put(entity);
    if (declaration->kind == DECL_ENUM) {
        entity->state = ENTITY_RESOLVED;
        entity->type = type_enum(entity);
        buf_push(entities_ordered, entity);
        for (size_t i = 0; i < declaration->enum_delc.num_items; i++) {
            EnumItem item = declaration->enum_delc.items[i];
            if (item.init) {
                fatal_error(item.location, "Enum constant initializers are not currently supported");
            }
            entity_append_const(item.name, entity->type, (Value) {.i = i});
        }
    }
    return entity;
}

void entity_append_type(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_TYPE, str_intern(name), NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    global_entities_put(entity);
}


void entity_append_func(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_FUNC, str_intern(name), NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    global_entities_put(entity);
}

void entity_append_typedef(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_TYPE, str_intern(name), declaration_typedef(name, typespec_name(name,
            location_builtin), location_builtin));
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    global_entities_put(entity);
}

void unify_math_expressions(ResolvedExpression *left, ResolvedExpression *right) {
    if (left->type == type_double) {
        cast_expression(right, type_double);
    } else if (right->type == type_double) {
        cast_expression(left, type_double);
    } else if (left->type == type_float) {
        cast_expression(right, type_float);
    } else if (right->type == type_float) {
        cast_expression(left, type_float);
    } else {
        promote_expression(left);
        promote_expression(right);
        if (left->type != right->type) {
            if (issigned(left->type) && issigned(right->type)) {
                if (type_rank(left->type) <= type_rank(right->type)) {
                    cast_expression(left, right->type);
                } else {
                    cast_expression(right, left->type);
                }
            } else if (issigned(left->type) && type_rank(right->type) >= type_rank(left->type)) {
                cast_expression(left, right->type);
            } else if (issigned(right->type) && type_rank(left->type) >= type_rank(right->type)) {
                cast_expression(right, left->type);
            } else if (issigned(left->type) && left->type->size > right->type->size) {
                cast_expression(right, left->type);
            } else if (issigned(right->type) && right->type->size > left->type->size) {
                cast_expression(left, right->type);
            } else {
                Type *type = unsigned_type(issigned(left->type) ? left->type : right->type);
                cast_expression(left, type);
                cast_expression(right, type);
            }
        }
    }

    assert(left->type == right->type);
}

Type* unify_math_type(Type *left, Type *right) {
    ResolvedExpression left_op = resolved_rvalue(left);
    ResolvedExpression right_op = resolved_rvalue(right);
    unify_math_expressions(&left_op, &right_op);
    assert(left_op.type == right_op.type);
    return left_op.type;
}

Value eval_unary(TokenKind op, Type *type, Value value) {
    if (is_integer_type(type)) {
        ResolvedExpression operand = resolved_const(type, value);
        if (issigned(type)) {
            cast_expression(&operand, type_llong);
            long long x = operand.val.ll;
            long long res;
            switch (op) {
                case TOKEN_ADD:
                    res = +x;
                    break;
                case TOKEN_SUB:
                    res = -x;
                    break;
                case TOKEN_BIN_NOT:
                    res = ~x;
                    break;
                case TOKEN_NOT:
                    res = !x;
                    break;
                default:
                    assert(0);
                    break;
            }
            operand.val.ll = res;
        } else {
            cast_expression(&operand, type_ullong);
            unsigned long long x = operand.val.ull;
            unsigned long long res;
            switch (op) {
                case TOKEN_ADD:
                    res = +x;
                    break;
                case TOKEN_SUB:
                    res = x;
                    break;
                case TOKEN_BIN_NOT:
                    res = ~x;
                    break;
                case TOKEN_NOT:
                    res = (unsigned) (!x);
                    break;
                default:
                    assert(0);
                    break;
            }
            operand.val.ull = res;
        }
        cast_expression(&operand, type);
        return operand.val;
    } else {
        return (Value){0};
    }
}

Value eval_binary(TokenKind op, Type *type, Value left, Value right) {
    if (is_integer_type(type)) {
        ResolvedExpression left_op = resolved_const(type, left);
        ResolvedExpression right_op = resolved_const(type, right);
        ResolvedExpression result;
        if (issigned(type)) {
            cast_expression(&left_op, type_llong);
            cast_expression(&right_op, type_llong);
            long long x = left_op.val.ll;
            long long y = right_op.val.ll;
            long long res;
            switch (op) {
                case TOKEN_MUL:
                    res = x * y;
                    break;
                case TOKEN_DIV:
                    res = y != 0 ? x / y : 0;
                    break;
                case TOKEN_MOD:
                    res = y != 0 ? x % y : 0;
                    break;
                case TOKEN_BIN_AND:
                    res = x & y;
                    break;
                case TOKEN_LSHIFT:
                    res = x << y;
                    break;
                case TOKEN_RSHIFT:
                    res = x >> y;
                    break;
                case TOKEN_ADD:
                    res = x + y;
                    break;
                case TOKEN_SUB:
                    res = x - y;
                    break;
                case TOKEN_BIN_OR:
                    res = x | y;
                    break;
                case TOKEN_XOR:
                    res = x ^ y;
                    break;
                case TOKEN_EQ:
                    res = x && y;
                    break;
                case TOKEN_NOTEQ:
                    res = x != y;
                    break;
                case TOKEN_LT:
                    res = x < y;
                    break;
                case TOKEN_LTEQ:
                    res = x <= y;
                    break;
                case TOKEN_GT:
                    res = x > y;
                    break;
                case TOKEN_GTEQ:
                    res = x >= y;
                    break;
                case TOKEN_AND:
                    res = x && y;
                    break;
                case TOKEN_OR:
                    res = x || y;
                    break;
                default:
                    assert(0);
                    break;
            }
            result = resolved_const(type_llong, (Value) {.ll = res});
        } else {
            cast_expression(&left_op, type_ullong);
            cast_expression(&right_op, type_ullong);
            unsigned long long x = left_op.val.ull;
            unsigned long long y = right_op.val.ull;
            unsigned long long res;
            switch (op) {
                case TOKEN_MUL:
                    res = x * y;
                    break;
                case TOKEN_DIV:
                    res = y != 0 ? x / y : 0;
                    break;
                case TOKEN_MOD:
                    res = y != 0 ? x % y : 0;
                    break;
                case TOKEN_BIN_AND:
                    res = x & y;
                    break;
                case TOKEN_LSHIFT:
                    res = x << y;
                    break;
                case TOKEN_RSHIFT:
                    res = x >> y;
                    break;
                case TOKEN_ADD:
                    res = x + y;
                    break;
                case TOKEN_SUB:
                    res = x - y;
                    break;
                case TOKEN_BIN_OR:
                    res = x | y;
                    break;
                case TOKEN_XOR:
                    res = x ^ y;
                    break;
                case TOKEN_EQ:
                    res = (unsigned) (x && y);
                    break;
                case TOKEN_NOTEQ:
                    res = (unsigned) (x != y);
                    break;
                case TOKEN_LT:
                    res = (unsigned) (x < y);
                    break;
                case TOKEN_LTEQ:
                    res = (unsigned) (x <= y);
                    break;
                case TOKEN_GT:
                    res = (unsigned) (x > y);
                    break;
                case TOKEN_GTEQ:
                    res = (unsigned) (x >= y);
                    break;
                case TOKEN_AND:
                    res = (unsigned) (x && y);
                    break;
                case TOKEN_OR:
                    res = (unsigned) (x || y);
                    break;
                default:
                    assert(0);
                    break;
            }
            result = resolved_const(type_llong, (Value) {.ull = res});
        }
        cast_expression(&result, type);
        return result.val;
    } else {
        return (Value){0};
    }
}

Entity* resolve_entity_name(const char *name);
ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type);
ResolvedExpression resolve_expression(Expression *expr);
ResolvedExpression resolve_const_expression(Expression *expr);
ResolvedExpression resolve_expression_decayed(Expression *expr);
ResolvedExpression resolve_expression_decayed_expected_type(Expression *expr, Type *expected_type);

typedef struct StatementContext {
    bool break_legal;
    bool continue_legal;
} StatementContext;

bool resolve_statement(Statement *stmt, Type *ret_type, StatementContext ctx);

ResolvedExpression resolve_expression_name(Expression *expr) {
    Entity *entity = resolve_entity_name(expr->name);
    if (!entity) {
        fatal_error(expr->location, "Name does %s not exists", expr->name);
    }
    if (entity->kind == ENTITY_VAR) {
        return resolved_lvalue(entity->type);
    } else if (entity->kind == ENTITY_CONST) {
        return resolved_const(entity->type, entity->val);
    } else if (entity->kind == ENTITY_FUNC) {
        return resolved_rvalue(entity->type);
    } else {
        fatal_error(expr->location, "%s must be var or const", expr->name);
    }
}

ResolvedExpression resolve_unary(TokenKind op, ResolvedExpression operand) {
    promote_expression(&operand);
    if (operand.is_const) {
        return resolved_const(operand.type, eval_unary(op, operand.type, operand.val));
    } else {
        return operand;
    }
}

ResolvedExpression resolve_expression_unary(Expression *expr) {
    if (expr->unary.op == TOKEN_BIN_AND) {
        ResolvedExpression operand = resolve_expression(expr->unary.operand);
        if (!operand.is_lvalue) {
            fatal_error(expr->location, "Can't take address of non-lvalue");
        }
        return resolved_rvalue(type_pointer(operand.type));
    } else {
        ResolvedExpression operand = resolve_expression_decayed(expr->unary.operand);
        switch (expr->unary.op) {
            case TOKEN_MUL:
                if (operand.type->kind != TYPE_POINTER) {
                    fatal_error(expr->location, "Can't dereference non pointer type");
                }
                return resolved_lvalue(operand.type->base);
            case TOKEN_ADD:
            case TOKEN_SUB:
                if (!is_math_type(operand.type)) {
                    fatal_error(expr->location, "Can only use unary %s with mathematics type",
                                token_kind_names[expr->unary.op]);
                }
                return resolve_unary(expr->unary.op, operand);
            case TOKEN_BIN_NOT:
                if (!is_integer_type(operand.type)) {
                    fatal_error(expr->location, "Can only use ~ with integer type");
                }
                return resolve_unary(expr->unary.op, operand);
            case TOKEN_NOT:
                if (!is_scalar_type(operand.type)) {
                    fatal_error(expr->location, "Can only use ! with scalar type");
                }
                return resolve_unary(expr->unary.op, operand);
            default:
                assert(0);
                break;
        }

        return (ResolvedExpression) {0};
    }
}

ResolvedExpression resolve_binary(TokenKind op, ResolvedExpression left, ResolvedExpression right) {
    if (left.is_const && right.is_const) {
        return resolved_const(left.type, eval_binary(op, left.type, left.val, right.val));
    } else {
        return resolved_rvalue(left.type);
    }
}

ResolvedExpression resolve_binary_math(TokenKind op, ResolvedExpression left, ResolvedExpression right) {
    unify_math_expressions(&left, &right);
    return resolve_binary(op, left, right);
}

ResolvedExpression resolve_expression_binary_op(TokenKind op, const char *name,
        ResolvedExpression left, ResolvedExpression right, SrcLocation loc) {
    switch (op) {
        case TOKEN_MUL:
        case TOKEN_DIV:
            if (!is_math_type(left.type)) {
                fatal_error(loc, "Left operand of %s must have mathematics type", name);
            }
            if (!is_math_type(right.type)) {
                fatal_error(loc, "Right operand of %s must have mathematics type", name);
            }
            return resolve_binary_math(op, left, right);
        case TOKEN_MOD:
            if (!is_integer_type(left.type)) {
                fatal_error(loc, "Left operand of %% must have integer type");
            }
            if (!is_integer_type(right.type)) {
                fatal_error(loc, "Right operand of %% must have integer type");
            }
            return resolve_binary_math(op, left, right);
        case TOKEN_ADD:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                return resolve_binary_math(op, left, right);
            } else if (left.type->kind == TYPE_POINTER && is_integer_type(right.type)) {
                return resolved_rvalue(left.type);
            } else if (right.type->kind == TYPE_POINTER && is_integer_type(left.type)) {
                return resolved_rvalue(right.type);
            } else {
                fatal_error(loc, "Invalid types of operands of +");
            }
        case TOKEN_SUB:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                return resolve_binary_math(op, left, right);
            } else if (left.type->kind == TYPE_POINTER && is_integer_type(right.type)) {
                return resolved_rvalue(left.type);
            } else if (left.type->kind == TYPE_POINTER && right.type->kind == TYPE_POINTER) {
                if (left.type->base != right.type->base) {
                    fatal_error(loc, "Can't subtract different pointers");
                }
                return resolved_rvalue(type_size);
            } else {
                fatal_error(loc, "Invalid types of operands of -");
            }
        case TOKEN_LSHIFT:
        case TOKEN_RSHIFT:
            if (is_integer_type(left.type) && is_integer_type(right.type)) {
                promote_expression(&left);
                promote_expression(&right);
                if (issigned(left.type)) {
                    cast_expression(&left, type_llong);
                    cast_expression(&right, type_llong);
                } else {
                    cast_expression(&left, type_ullong);
                    cast_expression(&right, type_ullong);
                }
                ResolvedExpression result = resolve_binary(op, left, right);
                cast_expression(&result, left.type);
                return result;
            } else  {
                fatal_error(loc, "Invalid types of operands of %s", name);
            }
        case TOKEN_LT:
        case TOKEN_LTEQ:
        case TOKEN_GT:
        case TOKEN_GTEQ:
        case TOKEN_EQ:
        case TOKEN_NOTEQ:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                ResolvedExpression result = resolve_binary_math(op, left, right);
                cast_expression(&result, type_int);
                return result;
            } else if (left.type->kind == TYPE_POINTER && right.type->kind == TYPE_POINTER) {
                if (left.type->base != right.type->base) {
                    fatal_error(loc, "Can't compare different pointer")
                }
                return resolved_rvalue(type_int);
            } else if ((is_null_pointer(left) && right.type->kind == TYPE_POINTER) ||
                       (is_null_pointer(right) && left.type->kind == TYPE_POINTER)) {
                return resolved_rvalue(type_int);
            } else {
                fatal_error(loc, "Invalid types of operands of %s", name);
            }
        case TOKEN_AND:
        case TOKEN_OR:
            if (is_scalar_type(left.type) && is_scalar_type(right.type)) {
                if (left.is_const && right.is_const) {
                    cast_expression(&left, type_bool);
                    cast_expression(&right, type_bool);
                    int r;
                    if (op == TOKEN_AND) {
                        r = left.val.b && right.val.b;
                    } else {
                        r = left.val.b || right.val.b;
                    }
                    return resolved_const(type_int, (Value) {.i = r});
                } else {
                    return resolved_rvalue(type_int);
                }
            } else {
                fatal_error(loc, "Invalid types of operands of %s", name);
            }
        case TOKEN_BIN_AND:
        case TOKEN_BIN_OR:
        case TOKEN_XOR:
            if (is_integer_type(left.type) && is_integer_type(right.type)) {
                return resolve_binary_math(op, left, right);
            } else {
                fatal_error(loc, "Invalid types of operands of %s", name);
            }
        default:
            assert(0);
            break;
    }

    return (ResolvedExpression){0};
}

ResolvedExpression resolve_expression_binary(Expression *expr) {
    ResolvedExpression left = resolve_expression_decayed(expr->binary.left);
    ResolvedExpression right = resolve_expression_decayed(expr->binary.right);
    return resolve_expression_binary_op(expr->binary.op, token_kind_names[expr->binary.op], left, right, expr->location);
}

ResolvedExpression resolve_expression_field(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->field.operand);
    bool is_const = operand.type->kind == TYPE_CONST;
    Type *type = unqualify_type(operand.type);
    complete_type(type);
    if (operand.type->kind == TYPE_POINTER) {
        operand = resolved_lvalue(type->base);
        is_const = operand.type->kind == TYPE_CONST;
        type = unqualify_type(operand.type);
        complete_type(type);
    }
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
        fatal_error(expr->location, "Can only access fields on aggregate types");
    }

    for (TypeField *it = type->aggregate.fields; it != type->aggregate.fields + type->aggregate.num_fields; it++) {
        if (it->name == expr->field.name) {
            ResolvedExpression field = operand.is_lvalue ? resolved_lvalue(it->type) : resolved_rvalue(it->type);
            if (is_const) {
                field.type = type_const(field.type);
            }
            return field;
        }
    }

    fatal_error(expr->location, "No field named %s on typespec", expr->field.name);
}

int aggregate_field_index(Type *type, const char *name) {
    for (size_t i =0; i < type->aggregate.num_fields; i++) {
        if (type->aggregate.fields[i].name == name) {
            return i;
        }
    }
    return -1;
}

ResolvedExpression resolve_expression_compound(Expression *expr, Type *expected_type) {
    if (!expected_type && !expr->compound.type) {
        fatal_error(expr->location, "Impossible to determine type of compound literal");
    }

    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
    } else {
        type = expected_type;
    }
    complete_type(type);
    bool is_const = type->kind == TYPE_CONST;
    type = unqualify_type(type);
    if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
        int index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == COMPOUNDFIELD_INDEX) {
                fatal_error(field.location, "Index in compound literal for struct or union not allowed");
            } else if (field.kind == COMPOUNDFIELD_NAME) {
                index = aggregate_field_index(type, field.name);
                if (index == -1) {
                    fatal_error(field.location, "Named field %s does not exist", field.name);
                }
            }
            if (index >= type->aggregate.num_fields) {
                fatal_error(field.location, "Field initializer in struct or union compound out of range");
            }
            Type *field_type = type->aggregate.fields[index].type;
            ResolvedExpression init = resolve_expression_decayed_expected_type(field.init, field_type);
            if (!convert_expression(&init, field_type)) {
                fatal_error(field.location, "Illegal conversion");
            }
            index++;
        }
    } else if (type->kind == TYPE_ARRAY) {
        int index = 0, index_max = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == COMPOUNDFIELD_NAME) {
                fatal_error(field.location, "Named field not allowed for array");
            } else if (field.kind == COMPOUNDFIELD_INDEX) {
                ResolvedExpression tmp_indx= resolve_const_expression(field.index);
                if (!is_integer_type(tmp_indx.type)) {
                    fatal_error(field.location, "Field initializer index must have integer type");
                }
                if (!cast_expression(&tmp_indx, type_int)) {
                    fatal_error(field.location, "Illegal conversion in initializer index");
                }
                if (tmp_indx.val.i < 0) {
                    fatal_error(field.location, "Field initializer index can't be negative");
                }
                index = tmp_indx.val.i;
            }

            if (type->num_elements && index >= type->num_elements) {
                fatal_error(field.location, "Field initializer in array out of range");
            }
            ResolvedExpression init_val = resolve_expression_decayed_expected_type(expr->compound.fields[i].init,
                    type->base);
            if (!convert_expression(&init_val, type->base)) {
                fatal_error(field.location, "Illegal conversion in initializer");
            }
            index_max = MAX(index_max, index);
            index++;
        }
        if (type->num_elements == 0) {
            type = type_array(type->base, index_max + 1);
        }
    } else {
        if (expr->compound.num_fields > 1) {
            fatal_error(expr->location, "Compound literal is invalid");
        }
        if (expr->compound.num_fields == 1) {
            CompoundField field = expr->compound.fields[0];
            ResolvedExpression init_val = resolve_expression_decayed_expected_type(field.init, type);
            if (!convert_expression(&init_val, type)) {
                fatal_error(field.location, "Illegal conversion in compound literal");
            }
        }
    }
    return resolved_lvalue(is_const ? type_const(type) : type);
}

ResolvedExpression resolve_expression_call(Expression *expr) {

    if (expr->call.operand->kind == EXPR_NAME) {
        Entity *entity = resolve_entity_name(expr->call.operand->name);
        if (entity && entity->kind == ENTITY_TYPE) {
            if (expr->call.num_args != 1) {
                fatal_error(expr->location, "Type conversion is invalid, operator takes 1 argument");
            }
            ResolvedExpression operand = resolve_expression_decayed(expr->call.args[0]);
            if (!cast_expression(&operand, entity->type)) {
                fatal_error(expr->location, "Invalid type conversion");
            }
            return operand;
        }
    }

    ResolvedExpression func = resolve_expression_decayed(expr->call.operand);
    complete_type(func.type);
    if (func.type->kind != TYPE_FUNC) {
        fatal_error(expr->location, "Calling non-function value");
    }

    size_t num_args = func.type->func.num_args;
    if (expr->call.num_args < num_args) {
        fatal_error(expr->location, "Calling function with too few arguments");
    }

    if (expr->call.num_args > num_args && !func.type->func.is_variadic) {
        fatal_error(expr->location, "Calling function with too many arguments");
    }

    for (size_t i = 0; i < num_args; i++) {
        Type *param = func.type->func.args[i];
        if (param->kind == TYPE_ARRAY) {
            param = type_pointer(param->base);
        }
        ResolvedExpression arg = resolve_expression_decayed_expected_type(expr->call.args[i], param);
        if (!convert_expression(&arg, param)) {
            fatal_error(expr->call.args[i]->location, "Call argument expression type doesn't match expected type");
        }
    }

    for (size_t i = num_args; i < expr->call.num_args; i++) {
        resolve_expression_decayed(expr->call.args[i]);
    }

    return resolved_rvalue(func.type->func.ret);
}

ResolvedExpression resolve_expression_ternary(Expression *expr, Type *expected_type) {
    ResolvedExpression cond = resolve_expression_decayed(expr->ternary.cond);
    if (!is_scalar_type(cond.type)) {
        fatal_error(expr->location, "Condition expression of ternary operator ? must have scalar type");
    }

    ResolvedExpression then_ex = resolve_expression_decayed_expected_type(expr->ternary.then_ex, expected_type);
    ResolvedExpression else_ex = resolve_expression_decayed_expected_type(expr->ternary.else_ex, expected_type);
    if (is_math_type(then_ex.type) && is_math_type(else_ex.type)) {
        unify_math_expressions(&then_ex, &else_ex);
        if (cond.is_const && then_ex.is_const && else_ex.is_const) {
            return resolved_const(then_ex.type, cond.val.i ? then_ex.val : else_ex.val);
        } else {
            return resolved_rvalue(then_ex.type);
        }
    } else if (then_ex.type == else_ex.type) {
        return resolved_rvalue(then_ex.type);
    } else {
        fatal_error(expr->location, "Ivalid types of operands of ?");
    }
}

ResolvedExpression resolve_expression_modify(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->modify.operand);
    Type *type = operand.type;
    complete_type(type);
    if (!operand.is_lvalue) {
        fatal_error(expr->location, "Can't modify non-lvalue");
    }
    if (type->is_nonmodify) {
        fatal_error(expr->location, "Can't modify const type");
    }

    if (!is_integer_type(type) && type->kind != TYPE_POINTER) {
        fatal_error(expr->location, "Type of operand of %s is not valid, must be integer or pointer", expr->modify.op);
    }
    return resolved_rvalue(type);
}

ResolvedExpression resolve_expression_index(Expression *expr) {
    ResolvedExpression operand = resolve_expression_decayed(expr->index.operand);
    if (operand.type->kind != TYPE_POINTER) {
        fatal_error(expr->location, "Can only index arrays or pointers");
    }
    ResolvedExpression index = resolve_expression(expr->index.index);

    if (!is_integer_type(index.type)) {
        fatal_error(expr->location, "Index must have typespec int");
    }
    return resolved_lvalue(operand.type->base);
}

ResolvedExpression resolve_expression_cast(Expression *expr) {
    Type *type = resolve_typespec(expr->cast.typespec);
    ResolvedExpression result = resolve_expression_decayed(expr->cast.expr);
    if (!cast_expression(&result, type)) {
        fatal_error(expr->location, "Illegal conversion");
    }
    return result;
}

ResolvedExpression resolve_expression_int(Expression *expr) {
    unsigned long long val = expr->int_lit.val;
    ResolvedExpression operand = resolved_const(type_ullong, (Value) {.ull = val});
    Type *type = type_ullong;

    if (expr->int_lit.mod == TOKENMOD_NONE) {
        bool overflow = false;
        switch (expr->int_lit.suffix) {
            case TOKENSUFFIX_NONE:
                type = type_int;
                if (val > INT_MAX) {
                    type = type_long;
                    if (val > LONG_MAX) {
                        type = type_llong;
                        overflow = val > LLONG_MAX;
                    }
                }
                break;
            case TOKENSUFFIX_U:
                type = type_uint;
                if (val > UINT_MAX) {
                    type = type_ulong;
                    if (val > ULONG_MAX) {
                        type = type_ullong;
                    }
                }
                break;
            case TOKENSUFFIX_L:
                type = type_long;
                if (val > LONG_MAX) {
                    type = type_llong;
                    overflow = val > LLONG_MAX;
                }
                break;
            case TOKENSUFFIX_UL:
                type = type_ulong;
                if (val > ULONG_MAX) {
                    type = type_ullong;
                }
                break;
            case TOKENSUFFIX_LL:
                type = type_llong;
                overflow = val > LLONG_MAX;
                break;
            case TOKENSUFFIX_ULL:
                type = type_ullong;
                break;
            default:
                assert(0);
                break;
        }
        if (overflow) {
            fatal_error(expr->location, "Integer literal is overflow");
        }
    } else {
        switch (expr->int_lit.suffix) {
            case TOKENSUFFIX_NONE:
                type = type_int;
                if (val > INT_MAX) {
                    type = type_uint;
                    if (val > UINT_MAX) {
                        type = type_long;
                        if (val > LONG_MAX) {
                            type = type_ulong;
                            if (val > ULONG_MAX) {
                                type = type_llong;
                                if (val > LLONG_MAX) {
                                    type = type_ullong;
                                }
                            }
                        }
                    }
                }
                break;
            case TOKENSUFFIX_U:
                type = type_uint;
                if (val > UINT_MAX) {
                    type = type_ulong;
                    if (val > ULONG_MAX) {
                        type = type_ullong;
                    }
                }
                break;
            case TOKENSUFFIX_L:
                type = type_long;
                if (val > LONG_MAX) {
                    type = type_ulong;
                    if (val > ULONG_MAX) {
                        type = type_llong;
                        if (val > LLONG_MAX) {
                            type = type_ullong;
                        }
                    }
                }
                break;
            case TOKENSUFFIX_UL:
                type = type_ulong;
                if (val > ULONG_MAX) {
                    type = type_ullong;
                }
                break;
            case TOKENSUFFIX_LL:
                type = type_llong;
                if (val > ULLONG_MAX) {
                    type = type_ullong;
                }
                break;
            case TOKENSUFFIX_ULL:
                type = type_ullong;
                break;
            default:
                assert(0);
                break;
        }
    }
    cast_expression(&operand, type);
    return operand;
}

ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type) {
    ResolvedExpression resolved;
    switch (expr->kind) {
        case EXPR_INT:
            resolved = resolve_expression_int(expr);
            break;
        case EXPR_FLOAT:
            resolved = resolved_const(expr->float_lit.suffix == TOKENSUFFIX_D ? type_double : type_float, (Value) {0});
            break;
        case EXPR_STR:
            resolved = resolved_rvalue(type_pointer(type_const(type_char)));
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
            if (expr->size_of_expr->kind == EXPR_NAME) {
                Entity *entity = resolve_entity_name(expr->size_of_expr->name);
                if (entity && entity->kind == ENTITY_TYPE) {
                    complete_type(entity->type);
                    resolved = resolved_const(type_usize, (Value) {.ull = entity->type->size});
                    set_resolved_type(expr->size_of_expr, entity->type);
                }
            }
            ResolvedExpression result = resolve_expression(expr->size_of_expr);
            Type *type = result.type;
            complete_type(type);
            resolved = resolved_const(type_usize, (Value) {.ull = type->size});
            break;
        }
        case EXPR_SIZEOF_TYPE: {
            Type *type = resolve_typespec(expr->size_of_type);
            complete_type(type);
            resolved = resolved_const(type_usize, (Value){.ull = type->size});
            break;
        }
        case EXPR_ALIGNOF_EXPR: {
            if (expr->align_of_expr->kind == EXPR_NAME) {
                Entity *entity = resolve_entity_name(expr->align_of_expr->name);
                if (entity && entity->kind == ENTITY_TYPE) {
                    complete_type(entity->type);
                    resolved = resolved_const(type_usize, (Value) {.ull = entity->type->align});
                    set_resolved_type(expr->align_of_expr, entity->type);
                    break;
                }
            }
            Type *type = resolve_expression(expr->align_of_expr).type;
            complete_type(type);
            resolved = resolved_const(type_usize, (Value) {.ull = type->align});
            break;
        }
        case EXPR_ALIGNOF_TYPE: {
            Type *type = resolve_typespec(expr->align_of_type);
            complete_type(type);
            resolved = resolved_const(type_usize, (Value) {.ull = type->align});
            break;
        }
        case EXPR_OFFSETOF: {
            Type *type = resolve_typespec(expr->offset_of_field.type);
            if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
                fatal_error(expr->location, "offsetof can only be used with struct or union");
            }
            int findex = aggregate_field_index(type, expr->offset_of_field.name);
            if (findex < 0) {
                fatal_error(expr->location, "No fiield %s in  type", expr->offset_of_field.name);
            }
            resolved = resolved_const(type_usize, (Value) {.ull = type->aggregate.fields[findex].offset});
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
        case EXPR_MODIFY:
            resolved = resolve_expression_modify(expr);
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
    set_resolved_type(expr, resolved.type);
    return resolved;
}

ResolvedExpression resolve_expression(Expression *expr) {
    return resolve_expression_expected_type(expr, NULL);
}

ResolvedExpression resolve_const_expression(Expression *expr) {
    ResolvedExpression result = resolve_expression(expr);
    if (!result.is_const) {
        fatal_error(expr->location, "Expected constant expression");
    }
    return result;
}

ResolvedExpression resolve_expression_decayed(Expression *expr) {
    return resolved_decay(resolve_expression(expr));
}

ResolvedExpression resolve_expression_decayed_expected_type(Expression *expr, Type *expected_type) {
    return resolved_decay(resolve_expression_expected_type(expr, expected_type));
}

void resolve_conditional_expression(Expression *expr) {
    ResolvedExpression cond = resolve_expression_decayed(expr);
    if (!is_scalar_type(cond.type)) {
        fatal_error(expr->location, "Conditional expression must have mathematics or pointer type");
    }
}

bool resolve_statement_block(StatementBlock block, Type *ret_type, StatementContext ctx) {
    Entity *entities = local_scope_enter();
    bool returns = false;
    for (size_t i = 0; i < block.num_statements; i++) {
        returns = resolve_statement(block.statements[i], ret_type,  ctx);
    }
    local_scope_leave(entities);
    return returns;
}

void resolve_statement_auto_assign(Statement *stmt) {
    Type *type = NULL;
    if (stmt->auto_assign.type) {
        type = resolve_typespec(stmt->auto_assign.type);
        if (stmt->auto_assign.init) {
            Type *expected = unqualify_type(type);
            ResolvedExpression expr = resolve_expression_expected_type(stmt->auto_assign.init, expected);
            if (type->kind == TYPE_ARRAY && expr.type->kind == TYPE_ARRAY && type->base == expr.type->base &&
                !type->num_elements) {
                type = expr.type;
            } else if (!convert_expression(&expr, expected)) {
                fatal_error(stmt->location, "Initialization expression not expected type");
            }
        }
    } else {
        type = unqualify_type(resolve_expression(stmt->auto_assign.init).type);
    }
    if (type->size == 0) {
        fatal_error(stmt->location, "Declaration can't have size 0");
    }
    if (!local_entities_push_var(stmt->auto_assign.name, type)) {
        fatal_error(stmt->location, "Duplicate definition");
    }
}

void resolve_statement_assign(Statement *stmt) {
    ResolvedExpression left = resolve_expression(stmt->assign.left);
    if (!left.is_lvalue) {
        fatal_error(stmt->location, "Can't assign to non-lvalue");
    }
    if (left.type->kind == TYPE_ARRAY) {
        fatal_error(stmt->location, "Can't assign to array");
    }
    if (left.type->is_nonmodify) {
        fatal_error(stmt->location, "lvalue has non-modifiable type");
    }
    const char *op_name = token_kind_names[stmt->assign.op];
    TokenKind op_binary = map_assign_token_binary_token[stmt->assign.op];
    ResolvedExpression right = resolve_expression_decayed_expected_type(stmt->assign.right, left.type);
    ResolvedExpression result;
    if (stmt->assign.op == TOKEN_ASSIGN) {
        result = right;
    } else if (stmt->assign.op == TOKEN_ADD_ASSIGN || stmt->assign.op == TOKEN_SUB_ASSIGN){
        if (left.type->kind == TYPE_POINTER && is_integer_type(right.type)) {
            result = resolved_rvalue(left.type);
        } else if (is_math_type(left.type) && is_math_type(right.type)) {
            result = resolve_expression_binary_op(op_binary, op_name, left, right, stmt->location);
        } else {
            fatal_error(stmt->location, "Invalid operand types for %s", op_name);
        }
    } else {
        result = resolve_expression_binary_op(op_binary, op_name, left, right, stmt->location);
    }
    if (!convert_expression(&result, left.type)) {
        fatal_error(stmt->location, "Illegal conversion right to left operand");
    }
}

bool resolve_statement(Statement *stmt, Type *ret_type, StatementContext ctx) {
    switch (stmt->kind) {
        case STMT_IF: {
            resolve_conditional_expression(stmt->if_stmt.cond);
            bool returns = resolve_statement_block(stmt->if_stmt.then, ret_type, ctx);
            for (size_t i = 0; i < stmt->if_stmt.num_else_ifs; i++) {
                ElseIf else_if = stmt->if_stmt.else_ifs[i];
                resolve_conditional_expression(else_if.cond);
                returns = resolve_statement_block(else_if.body, ret_type, ctx);
            }
            if (stmt->if_stmt.else_body.statements) {
                returns = resolve_statement_block(stmt->if_stmt.else_body, ret_type, ctx);
            } else {
                returns = false;
            }
            return returns;
        }
        case STMT_FOR: {
            Entity *entities = local_scope_enter();
            if (stmt->for_stmt.init) {
                resolve_statement(stmt->for_stmt.init, ret_type, ctx);
            }
            if (stmt->for_stmt.cond) {
                resolve_conditional_expression(stmt->for_stmt.cond);
            }
            if (stmt->for_stmt.next) {
                resolve_statement(stmt->for_stmt.next, ret_type, ctx);
            }
            ctx.continue_legal = true;
            ctx.break_legal = true;
            resolve_statement_block(stmt->for_stmt.body, ret_type, ctx);
            local_scope_leave(entities);
            return false;
        }
        case STMT_WHILE:
        case STMT_DO_WHILE:
            resolve_conditional_expression(stmt->while_stmt.cond);
            ctx.break_legal = true;
            ctx.continue_legal = true;
            resolve_statement_block(stmt->while_stmt.body, ret_type, ctx);
            return false;
        case STMT_SWITCH: {
            ResolvedExpression expr = resolve_expression_decayed(stmt->switch_stmt.expr);
            if (!is_integer_type(expr.type)) {
                fatal_error(stmt->location, "Switch expression not an integer");
            }
            ctx.break_legal = true;
            bool returns = false;
            bool has_default = false;
            for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
                SwitchCase _case = stmt->switch_stmt.cases[i];
                for (size_t j = 0; j < _case.num_expressions; j++) {
                    ResolvedExpression case_expr = resolve_expression(_case.expressions[j]);
                    if (!convert_expression(&case_expr, expr.type)) {
                        fatal_error(stmt->location, "Case expression in switch typespec mismatch");
                    }
                }
                if (_case.is_default) {
                    if (has_default) {
                        fatal_error(stmt->location, "Switch statement has nultiple default")
                    }
                    has_default = true;
                }
                if (_case.body.num_statements > 0) {
                    Statement *last_stmt = _case.body.statements[_case.body.num_statements - 1];
                    if (last_stmt->kind == STMT_BREAK) {
                        warning(last_stmt->location, "Case blocks already end with an impliicit break");
                    }
                }
                returns = resolve_statement_block(_case.body, ret_type, ctx) && returns;
            }

            return returns && has_default;
        }
        case STMT_ASSIGN: {
            resolve_statement_assign(stmt);
            return false;
        }
        case STMT_AUTO_ASSIGN:
            resolve_statement_auto_assign(stmt);
            return false;
        case STMT_RETURN:
            if (stmt->expr) {
                ResolvedExpression ret = resolve_expression_expected_type(stmt->expr, ret_type);
                if (!convert_expression(&ret, ret_type)) {
                    fatal_error(stmt->location, "Return typespec mismatch");
                }
            } else {
                if (ret_type != type_void) {
                    fatal_error(stmt->location, "Empty return expression for function with non-void return typespec");
                }
            }
            return true;
        case STMT_ATTR:
            if (stmt->attribute.name == keywords.assert) {
                if (stmt->attribute.num_args != 1) {
                    fatal_error(stmt->location, "#assert takes 1 argument");
                }
                resolve_conditional_expression(stmt->attribute.args[0].expr);
            } else {
                warning(stmt->location, "Unknown attribute '%s'", stmt->location.name);
            }
            return false;
        case STMT_BREAK:
            if (!ctx.break_legal) {
                fatal_error(stmt->location, "Illegal break");
            }
            return false;
        case STMT_CONTINUE:
            if (!ctx.continue_legal) {
                fatal_error(stmt->location, "Illegal continue");
            }
            return false;
        case STMT_EXPR:
            resolve_expression(stmt->expr);
            return false;
        case STMT_BLOCK:
            return resolve_statement_block(stmt->block, ret_type, ctx);
        default:
            assert(0);
            return false;
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
            if (!entity) {
                fatal_error(typespec->location, "unknown type name '%s'", typespec->name);
            }
            if (entity->kind != ENTITY_TYPE) {
                fatal_error(typespec->location, "%s must be type", typespec->name);
            }
            type = entity->type;
            break;
        }
        case TYPESPEC_ARRAY: {
            int size = 0;
            if (typespec->size) {
                ResolvedExpression size_expr = resolve_const_expression(typespec->size);
                if (!is_integer_type(size_expr.type)) {
                    fatal_error(typespec->location, "Array size must definition by integer");
                }
                cast_expression(&size_expr, type_int);
                size = size_expr.val.i;
                if (size < 0) {
                    fatal_error(typespec->location, "Negative array size");
                }
            }
            type = type_array(resolve_typespec(typespec->base), size);
            break;
        }
        case TYPESPEC_CONST:
            type = type_const(resolve_typespec(typespec->base));
            break;
        case TYPESPEC_POINTER:
            type = type_pointer(resolve_typespec(typespec->base));
            break;
        case TYPESPEC_FUNC: {
            Type **args = NULL;
            for (Typespec **it = typespec->func.args; it != typespec->func.args + typespec->func.num_args; it++) {
                Type *arg = resolve_typespec(*it);
                if (arg == type_void) {
                    fatal_error(typespec->location, "Function parameter can't have type void");
                }
                buf_push(args, resolve_typespec(*it));
            }
            Type *ret = type_void;
            if (typespec->func.ret) {
                ret = resolve_typespec(typespec->func.ret);
            }
            if (ret->kind == TYPE_ARRAY) {
                fatal_error(typespec->location, "Function return type can't be array");
            }
            type = type_func(args, buf_len(args), ret, typespec->func.is_variadic);
            break;
        }
        default:
            assert(0);
            break;
    }

    set_resolved_type(typespec, type);
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
            if (!(type->kind == TYPE_ARRAY && result.type->kind == TYPE_ARRAY && type->base == result.type->base
                && !type->num_elements)) {
                if (!convert_expression(&result, type)) {
                    fatal_error(decl->location, "Illegal conversion in variable initializer");
                }
            }
        }

        type = result.type;
    }

    complete_type(type);
    if (type->size == 0) {
        fatal_error(decl->location, "Declaration can't have size 0");
    }
    return type;
}

Type* resolve_declaration_const(Declaration *decl, Value *val) {
    ResolvedExpression result = resolve_const_expression(decl->const_decl.expr);
    if (!is_scalar_type(result.type)) {
        fatal_error(decl->location, "Const must have scalar type");
    }
    if (decl->const_decl.type) {
        Type *type = resolve_typespec(decl->const_decl.type);
        if (!convert_expression(&result, type)) {
            fatal_error(decl->location, "Invalid type in constant");
        }
    }
    *val = result.val;
    return result.type;
}

Type* resolve_declaration_func(Declaration *decl) {
    Type **params = NULL;
    for (FuncParam *it = decl->func.params; it != decl->func.params + decl->func.num_params; it++) {
        Type *param = resolve_typespec(it->type);
        complete_type(param);
        if (param == type_void) {
            fatal_error(decl->location, "Function parameter can't be void");
        }
        buf_push(params, param);
    }
    Type *ret = type_void;
    if (decl->func.return_type) {
        ret = resolve_typespec(decl->func.return_type);
        complete_type(ret);
    }

    if (ret->kind == TYPE_ARRAY) {
        fatal_error(decl->location, "Function return type can't be array ");
    }

    return type_func(params, buf_len(params), ret, decl->func.is_variadic);
}

void resolve_func(Entity *entity) {
    if (entity->decl->is_incomplete) {
        return;
    }
    Entity *entities = local_scope_enter();
    for (size_t i = 0; i < entity->decl->func.num_params; i++) {
        FuncParam param = entity->decl->func.params[i];
        Type *param_type = resolve_typespec(param.type);
        if (param_type->kind == TYPE_ARRAY) {
            param_type = type_pointer(param_type->base);
        }
        local_entities_push_var(param.name, param_type);
    }
    Type *ret = resolve_typespec(entity->decl->func.return_type);
    bool returns = resolve_statement_block(entity->decl->func.body, ret, (StatementContext) {0});
    local_scope_leave(entities);
    if (ret != type_void && !returns) {
        fatal_error(entity->decl->location, "Not all control path return value");
    }
}

void resolve_entity(Entity *entity) {
    if (entity->state == ENTITY_RESOLVED) {
        return;;
    }

    if (entity->state == ENTITY_RESOLVING) {
        fatal("Cyclic dependency of declaration %s", entity->name);
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
        return NULL;
    }

    resolve_entity(entity);
    return entity;
}

void complete_entity(Entity *entity) {
    resolve_entity(entity);
    if (entity->kind == ENTITY_TYPE) {
        if (entity->decl && entity->decl->is_incomplete) {
            return;
        }
        complete_type(entity->type);
    } else if (entity->kind == ENTITY_FUNC) {
        resolve_func(entity);
    }
}

void complete_entities() {
    for (Entity **it = global_entities_buf; it != buf_end(global_entities_buf); it++) {
        Entity *entity = *it;
        if (entity->decl) {
            complete_entity(entity);
        }
    }
}

void init_entities() {
    entity_append_type("void", type_void);
    entity_append_type("bool", type_bool);
    entity_append_type("char", type_char);
    entity_append_type("schar", type_schar);
    entity_append_type("uchar", type_uchar);
    entity_append_type("int", type_int);
    entity_append_type("uint", type_uint);
    entity_append_type("short", type_short);
    entity_append_type("ushort", type_ushort);
    entity_append_type("long", type_long);
    entity_append_type("ulong", type_ulong);
    entity_append_type("llong", type_llong);
    entity_append_type("ullong", type_ullong);
    entity_append_type("float", type_float);
    entity_append_type("double", type_double);

    entity_append_typedef("int8", type_schar);
    entity_append_typedef("uint8", type_uchar);
    entity_append_typedef("int16", type_short);
    entity_append_typedef("uint16", type_ushort);
    entity_append_typedef("int32", type_int);
    entity_append_typedef("uint32", type_uint);
    entity_append_typedef("int64", type_llong);
    entity_append_typedef("uint64", type_ullong);

    entity_append_typedef("usize", type_usize);
    entity_append_typedef("size", type_size);
    entity_append_typedef("uintptr", type_uintptr);

    entity_append_const("true", type_bool, (Value) {.b = true});
    entity_append_const("false", type_bool, (Value) {.b = false});
    entity_append_const("NULL", type_pointer(type_void), (Value) {.p = 0});
}

void entities_append_declaration_list() {
    for (size_t i = 0; i < global_declaration_list->num_declarations; i++) {
        Declaration *d = global_declaration_list->declarations[i];
        if (d->kind != DECL_ATTRIBUTE) {
            entity_append_declaration(d);
        }
    }
}