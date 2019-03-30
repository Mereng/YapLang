Type *type_void = &(Type){TYPE_VOID, 0};
Type *type_char = &(Type){TYPE_CHAR, 1, 1};
Type *type_schar = &(Type){TYPE_SCHAR, 1, 1};
Type *type_uchar = &(Type){TYPE_UCHAR, 1, 1};
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

typedef struct ResolvedExpression {
    Type *type;
    bool is_lvalue;
    bool is_const;
    Value val;
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

ResolvedExpression resolved_const(Type *type, Value val) {
    return (ResolvedExpression) {
        .type = type,
        .is_const = true,
        .val = val
    };
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
            case TYPE_FLOAT: \
                operand->val.f = (float)operand->val.t; \
                break; \
            case TYPE_DOUBLE: \
                operand->val.d = (double)operand->val.t; \
                break; \
            default: \
                operand->is_const = false; \
                break; \
        } \
        break;

bool is_math_type(Type *type);

bool convert_expression(ResolvedExpression *operand, Type *type) {
    if (operand->type == type) {
        return true;
    }
    if (!(is_math_type(operand->type) && is_math_type(type)) &&
            !(type->kind == TYPE_POINTER && type->pointer.base == type_void) &&
            !(operand->type->kind == TYPE_POINTER && operand->type->pointer.base == type_void && type->kind == TYPE_POINTER)) {
        return false;
    }
    if (operand->is_const) {
        switch (operand->type->kind) {
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
            CASE(TYPE_FLOAT, f)
            CASE(TYPE_DOUBLE, d)
            default:
                operand->is_const = false;
                break;
        }
    }
    operand->type = type;
    return true;
}

#undef CASE

Value convert_const_expression(Type *src, Type *dest, Value val) {
    ResolvedExpression operand = resolved_const(src, val);
    convert_expression(&operand, dest);
    return operand.val;
}

void promote_expression(ResolvedExpression *operand) {
    switch (operand->type->kind) {
        case TYPE_CHAR:
        case TYPE_SCHAR:
        case TYPE_UCHAR:
        case TYPE_SHORT:
        case TYPE_USHORT:
            convert_expression(operand, type_int);
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
        type->pointer.base = base;
        map_put(&pointer_types_cache, base, type);
    }
    return type;
}

typedef struct ArrayTypeCached {
    Type *base;
    int size;
    Type *array;
} ArrayTypeCached;

ArrayTypeCached *array_type_cache;

Type* type_array(Type *base, int size) {
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
    bool variadic;
} FuncTypeCached;

FuncTypeCached *func_type_cache;

Type* type_func(Type **params, size_t num_params, Type *ret, bool variadic) {
    for (FuncTypeCached *it = func_type_cache; it != buf_end(func_type_cache); it++) {
        if (it->num_params == num_params && it->ret == ret && it->variadic == variadic) {
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
    type->func.is_variadic = variadic;
    buf_push(func_type_cache, ((FuncTypeCached){params, num_params, ret, type}));
    return type;
}

Type* type_incomplete(Entity *entity) {
    Type *type = type_new(TYPE_INCOMPLETE);
    type->entity = entity;
    return type;
}

bool is_integer_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_ULLONG;
}

bool is_math_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_scalar_type(Type *type) {
    return TYPE_CHAR <= type->kind && type->kind <= TYPE_FUNC;
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
    [TYPE_CHAR] = 1,
    [TYPE_SCHAR] = 1,
    [TYPE_UCHAR] = 1,
    [TYPE_SHORT] = 2,
    [TYPE_USHORT] = 2,
    [TYPE_INT] = 3,
    [TYPE_UINT] = 3,
    [TYPE_LONG] = 4,
    [TYPE_ULONG] = 4,
    [TYPE_LLONG] = 5,
    [TYPE_ULLONG] = 5
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
    map_put(&global_entities, (void*)entity->name, entity);
    buf_push(global_entities_buf, entity);
}

Type* resolve_typespec(Typespec *typespec);

void type_complete_struct(Type* type, TypeField *fields, size_t num_fields) {
    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        it->offset = type->size;
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
        it->offset = 0;
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
        fatal("Type dependence cycle %s", type->entity->name);
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

Entity* entity_get(const char *name) {
    for (Entity *it = local_entities_end; it != local_entities; it--) {
        Entity *entity = it - 1;
        if (entity->name == name) {
            return entity;
        }
    }
    return map_get(&global_entities, (void*)name);
}

void local_entities_push_var(const char *name, Type *type) {
    if (local_entities_end == local_entities + MAX_LOCAL_ENTITIES) {
        fatal("Too many local entities in %s", type->entity->name);
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
    global_entities_put(entity);

    declaration->entity = entity;
    if (declaration->kind == DECL_ENUM) {
        for (size_t i = 0; i < declaration->enum_delc.num_items; i++) {
            Entity *entity_const = entity_enum_const(declaration->enum_delc.items[i].name, declaration);
            global_entities_put(entity_const);
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

void unify_math_expressions(ResolvedExpression *left, ResolvedExpression *right) {
    if (left->type == type_double) {
        convert_expression(right, type_double);
    } else if (right->type == type_double) {
        convert_expression(left, type_double);
    } else if (left->type == type_float) {
        convert_expression(right, type_float);
    } else if (right->type == type_float) {
        convert_expression(left, type_float);
    } else {
        promote_expression(left);
        promote_expression(right);
        if (left->type != right->type) {
            if (issigned(left->type) && issigned(right->type)) {
                if (type_rank(left->type) <= type_rank(right->type)) {
                    convert_expression(left, right->type);
                } else {
                    convert_expression(right, left->type);
                }
            } else if (issigned(left->type) && type_rank(right->type) >= type_rank(left->type)) {
                convert_expression(left, right->type);
            } else if (issigned(right->type) && type_rank(left->type) >= type_rank(right->type)) {
                convert_expression(right, left->type);
            } else if (issigned(left->type) && left->type->size > right->type->size) {
                convert_expression(right, left->type);
            } else if (issigned(right->type) && right->type->size > left->type->size) {
                convert_expression(left, right->type);
            } else {
                Type *type = unsigned_type(issigned(left->type) ? left->type : right->type);
                convert_expression(left, type);
                convert_expression(right, type);
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
    ResolvedExpression operand = resolved_const(type, value);
    if (is_integer_type(type)) {
        if (issigned(type)) {
            convert_expression(&operand, type_llong);
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
            convert_expression(&operand, type_ullong);
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
    } else {
        convert_expression(&operand, type_double);
        switch (op) {
            case TOKEN_ADD:
                operand.val.d = +operand.val.d;
                break;
            case TOKEN_SUB:
                operand.val.d = -operand.val.d;
                break;
            default:
                assert(0);
                break;
        }
    }

    convert_expression(&operand, type);
    return operand.val;
}

Value eval_binary(TokenKind op, Type *type, Value left, Value right) {
    ResolvedExpression left_op = resolved_const(type, left);
    ResolvedExpression right_op = resolved_const(type, right);
    ResolvedExpression result;

    if (is_integer_type(type)) {
        if (issigned(type)) {
            convert_expression(&left_op, type_llong);
            convert_expression(&right_op, type_llong);
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
            convert_expression(&left_op, type_ullong);
            convert_expression(&right_op, type_ullong);
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
    } else {
        convert_expression(&left_op, type_double);
        convert_expression(&right_op, type_double);
        double x = left_op.val.d;
        double y = right_op.val.d;
        double res;
        switch (op) {
            case TOKEN_MUL:
                res = x * y;
                break;
            case TOKEN_DIV:
                res = x / y;
                break;
            case TOKEN_ADD:
                res = x + y;
                break;
            case TOKEN_SUB:
                res = x - y;
                break;
            default:
                assert(0);
                break;
        }
        result = resolved_const(type_double, (Value) {.d = res});
    }
    convert_expression(&result, type);
    return result.val;
}

Entity* resolve_entity_name(const char *name);
ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type);
ResolvedExpression resolve_expression(Expression *expr);
ResolvedExpression resolve_const_expression(Expression *expr);
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
    ResolvedExpression operand = resolve_expression(expr->unary.operand);
    switch (expr->unary.op) {
        case TOKEN_MUL:
            operand = pointer_decay(operand);
            if (operand.type->kind != TYPE_POINTER) {
                fatal_error(expr->location, "Can't dereference non pointer typespec");
            }
            return resolved_lvalue(operand.type->pointer.base);
        case TOKEN_BIN_AND:
            if (!operand.is_lvalue) {
                fatal_error(expr->location, "Can't take address of non-lvalue");
            }
            return (ResolvedExpression){type_pointer(operand.type)};
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
        default:
            assert(0);
            break;
    }

    return (ResolvedExpression){0};
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

ResolvedExpression resolve_expression_binary(Expression *expr) {
    ResolvedExpression left = resolve_expression(expr->binary.left);
    ResolvedExpression right = resolve_expression(expr->binary.right);
    switch (expr->binary.op) {
        case TOKEN_MUL:
        case TOKEN_DIV:
            if (!is_math_type(left.type)) {
                fatal_error(expr->location, "Left operand of %s must have mathematics type",
                        token_kind_names[expr->binary.op]);
            }
            if (!is_math_type(right.type)) {
                fatal_error(expr->location, "Right operand of %s must have mathematics type",
                            token_kind_names[expr->binary.op]);
            }
            return resolve_binary_math(expr->binary.op, left, right);
        case TOKEN_MOD:
            if (!is_integer_type(left.type)) {
                fatal_error(expr->binary.left->location, "Left operand of %% must have integer type");
            }
            if (!is_integer_type(right.type)) {
                fatal_error(expr->binary.right->location, "Right operand of %% must have integer type");
            }
            return resolve_binary_math(expr->binary.op, left, right);
        case TOKEN_ADD:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                return resolve_binary_math(expr->binary.op, left, right);
            } else if (left.type->kind == TYPE_POINTER && is_integer_type(right.type)) {
                return resolved_rvalue(left.type);
            } else if (right.type->kind == TYPE_POINTER && is_integer_type(left.type)) {
                return resolved_rvalue(right.type);
            } else {
                fatal_error(expr->location, "Invalid types of operands of +");
            }
        case TOKEN_SUB:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                return resolve_binary_math(expr->binary.op, left, right);
            } else if (left.type->kind == TYPE_POINTER && is_integer_type(right.type)) {
                return resolved_rvalue(left.type);
            } else if (left.type->kind == TYPE_POINTER && right.type->kind == TYPE_POINTER) {
                if (left.type->pointer.base != right.type->pointer.base) {
                    fatal_error(expr->location, "Can't subtract different pointers");
                }
                return resolved_rvalue(type_size);
            } else {
                fatal_error(expr->location, "Invalid types of operands of -");
            }
        case TOKEN_LSHIFT:
        case TOKEN_RSHIFT:
            if (is_integer_type(left.type) && is_integer_type(right.type)) {
                promote_expression(&left);
                promote_expression(&right);
                if (issigned(left.type)) {
                    convert_expression(&left, type_llong);
                    convert_expression(&right, type_llong);
                } else {
                    convert_expression(&left, type_ullong);
                    convert_expression(&right, type_ullong);
                }
                ResolvedExpression result = resolve_binary(expr->binary.op, left, right);
                convert_expression(&result, left.type);
                return result;
            } else  {
                fatal_error(expr->location, "Invalid types of operands of %s", token_kind_names[expr->binary.op]);
            }
        case TOKEN_LT:
        case TOKEN_LTEQ:
        case TOKEN_GT:
        case TOKEN_GTEQ:
        case TOKEN_EQ:
        case TOKEN_NOTEQ:
            if (is_math_type(left.type) && is_math_type(right.type)) {
                ResolvedExpression result = resolve_binary_math(expr->binary.op, left, right);
                convert_expression(&result, type_int);
                return result;
            } else if (left.type->kind == TYPE_POINTER && right.type->kind == TYPE_POINTER) {
                if (left.type->pointer.base != right.type->pointer.base) {
                    fatal_error(expr->location, "Can't compare different pointer")
                }
                return resolved_rvalue(type_int);
            } else {
                fatal_error(expr->location, "Invalid types of operands of %s", token_kind_names[expr->binary.op]);
            }
        case TOKEN_AND:
        case TOKEN_OR:
            if (is_scalar_type(left.type) && is_scalar_type(right.type)) {
                return resolved_rvalue(type_int);
            } else {
                fatal_error(expr->location, "Invalid types of operands of %s", token_kind_names[expr->binary.op]);
            }
        case TOKEN_BIN_AND:
        case TOKEN_BIN_OR:
        case TOKEN_XOR:
            if (is_integer_type(left.type) && is_integer_type(right.type)) {
                return resolve_binary_math(expr->binary.op, left, right);
            } else {
                fatal_error(expr->location, "Invalid types of operands of %s", token_kind_names[expr->binary.op]);
            }
        default:
            assert(0);
            break;
    }

    return (ResolvedExpression){0};
}

ResolvedExpression resolve_expression_field(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->field.operand);
    complete_type(operand.type);
    if (operand.type->kind != TYPE_STRUCT && operand.type->kind != TYPE_UNION) {
        fatal_error(expr->location, "Can only access fields on aggregate types");
    }

    for (TypeField *it = operand.type->aggregate.fields; it != operand.type->aggregate.fields + operand.type->aggregate.num_fields; it++) {
        if (it->name == expr->field.name) {
            return operand.is_lvalue ? resolved_lvalue(it->type) : resolved_rvalue(it->type);
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
        fatal_error(expr->location, "Impossible to determine typespec of compound literal");
    }

    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
    } else {
        type = expected_type;
    }
    complete_type(type);

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
            ResolvedExpression init = resolve_expression_expected_type(field.init, field_type);
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
                if (!convert_expression(&tmp_indx, type_int)) {
                    fatal_error(field.location, "Illegal conversion in initializer index");
                }
                if (tmp_indx.val.i < 0) {
                    fatal_error(field.location, "Field initializer index can't be negative");
                }
                index = tmp_indx.val.i;
            }

            if (type->array.size && index >= type->aggregate.num_fields) {
                fatal_error(field.location, "Field initializer in array out of range");
            }
            ResolvedExpression init_val = resolve_expression_expected_type(expr->compound.fields[i].init,
                    type->array.base);
            if (!convert_expression(&init_val, type->array.base)) {
                fatal_error(field.location, "Illegal conversion in initializer");
            }
            index_max = MAX(index_max, index);
            index++;
        }
        if (type->array.size == 0) {
            type = type_array(type->array.base, index_max + 1);
        }
    } else {
        if (expr->compound.num_fields > 1) {
            fatal_error(expr->location, "Compound literal is invalid");
        }
        if (expr->compound.num_fields == 1) {
            CompoundField field = expr->compound.fields[0];
            ResolvedExpression init_val = resolve_expression_expected_type(field.init, type);
            if (!convert_expression(&init_val, type)) {
                fatal_error(field.location, "Illegal conversion in compound literal");
            }
        }
    }
    return resolved_lvalue(type);
}

ResolvedExpression resolve_expression_call(Expression *expr) {
    ResolvedExpression operand = resolve_expression(expr->call.operand);
    complete_type(operand.type);
    if (operand.type->kind != TYPE_FUNC) {
        fatal_error(expr->location, "Calling non-function value");
    }

    size_t num_args = operand.type->func.num_args;
    if (expr->call.num_args < num_args) {
        fatal_error(expr->location, "Calling function with too few arguments");
    }

    if (expr->call.num_args > num_args && !operand.type->func.is_variadic) {
        fatal_error(expr->location, "Calling function with too many arguments");
    }

    for (size_t i = 0; i < num_args; i++) {
        Type *param = operand.type->func.args[i];
        ResolvedExpression arg = resolve_expression_expected_type(expr->call.args[i], param);
        if (!convert_expression(&arg, param)) {
            fatal_error(expr->call.args[i]->location, "Call argument expression typespec doesn't match expected typespec");
        }
    }

    for (size_t i = num_args; i < expr->call.num_args; i++) {
        resolve_expression(expr->call.args[i]);
    }

    return resolved_rvalue(operand.type->func.ret);
}

ResolvedExpression resolve_expression_ternary(Expression *expr, Type *expected_type) {
    ResolvedExpression cond = pointer_decay(resolve_expression(expr->ternary.cond));
    if (!is_scalar_type(cond.type)) {
        fatal_error(expr->location, "Condition expression of ternary operator ? must have scalar type");
    }

    ResolvedExpression then_ex = pointer_decay(resolve_expression_expected_type(expr->ternary.then_ex, expected_type));
    ResolvedExpression else_ex = pointer_decay(resolve_expression_expected_type(expr->ternary.else_ex, expected_type));
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

ResolvedExpression resolve_expression_index(Expression *expr) {
    ResolvedExpression operand = pointer_decay(resolve_expression(expr->index.operand));
    if (operand.type->kind != TYPE_POINTER) {
        fatal_error(expr->location, "Can only index arrays or pointers");
    }
    ResolvedExpression index = resolve_expression(expr->index.index);

    if (index.type->kind != TYPE_INT) {
        fatal_error(expr->location, "Index must have typespec int");
    }
    return resolved_lvalue(operand.type->pointer.base);
}

ResolvedExpression resolve_expression_cast(Expression *expr) {
    Type *type = resolve_typespec(expr->cast.typespec);
    ResolvedExpression result = pointer_decay(resolve_expression(expr->cast.expr));
    if (!convert_expression(&result, type)) {
        fatal_error(expr->location, "Illegal conversion");
    }
    return result;
}

ResolvedExpression resolve_expression_expected_type(Expression *expr, Type *expected_type) {
    ResolvedExpression resolved;
    switch (expr->kind) {
        case EXPR_INT:
            resolved = resolved_const(type_int, (Value){.i = expr->int_val});
            break;
        case EXPR_FLOAT:
            resolved = resolved_const(type_float, (Value) {.f = (float)expr->float_val});
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
            resolved = resolved_const(type_usize, (Value){.ull = type->size});
            break;
        }
        case EXPR_SIZEOF_TYPE: {
            Type *type = resolve_typespec(expr->size_of_type);
            complete_type(type);
            resolved = resolved_const(type_usize, (Value){.ull = type->size});
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

ResolvedExpression resolve_const_expression(Expression *expr) {
    ResolvedExpression result = resolve_expression(expr);
    if (!result.is_const) {
        fatal_error(expr->location, "Expected constant expression");
    }
    return result;
}

void resolve_conditional_expression(Expression *expr) {
    ResolvedExpression cond = resolve_expression(expr);
    if (!is_math_type(cond.type) && cond.type->kind != TYPE_POINTER) {
        fatal_error(expr->location, "Conditional expression must have mathematics or pointer type");
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
                    if (!convert_expression(&case_expr, expr.type)) {
                        fatal_error(stmt->location, "Case expression in switch typespec mismatch");
                    }
                }
                resolve_statement_block(_case.body, ret_type);
            }
            break;
        }
        case STMT_ASSIGN: {
            ResolvedExpression left = resolve_expression(stmt->assign.left);
            if (!left.is_lvalue) {
                fatal_error(stmt->location, "Can't assign to non-lvalue");
            }
            if (stmt->assign.right) {
                ResolvedExpression right = resolve_expression_expected_type(stmt->assign.right, left.type);
                if (!convert_expression(&right, left.type)) {
                    fatal_error(stmt->location, "Illegal conversion right to left operand");
                }
            }
            break;
        }
        case STMT_AUTO_ASSIGN:
            local_entities_push_var(stmt->auto_assign.name, resolve_expression(stmt->auto_assign.init).type);
            break;
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
            break;
        case STMT_BREAK:
        case STMT_CONTINUE:
            break;
        case STMT_EXPR:
            resolve_expression(stmt->expr);
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
            if (typespec->array.size) {
                ResolvedExpression size_expr = resolve_const_expression(typespec->array.size);
                if (!is_integer_type(size_expr.type)) {
                    fatal_error(typespec->location, "Array size must definition by integer");
                }
                convert_expression(&size_expr, type_int);
                size = size_expr.val.i;
                if (size < 0) {
                    fatal_error(typespec->location, "Negative array size");
                }
            }
            type = type_array(resolve_typespec(typespec->array.base), size);
            break;
        }
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
            type = type_func(args, buf_len(args), ret, typespec->func.is_variadic);
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
            if (!(type->kind == TYPE_ARRAY && result.type->kind == TYPE_ARRAY && type->array.base == result.type->array.base
                && !type->array.size)) {
                if (!convert_expression(&result, type)) {
                    fatal_error(decl->location, "Illegal conversion in variable initializer");
                }
            }
        }

        type = result.type;
    }

    complete_type(type);
    return type;
}

Type* resolve_declaration_const(Declaration *decl, Value *val) {
    ResolvedExpression result = resolve_const_expression(decl->const_decl.expr);
    if (!is_math_type(result.type)) {
        fatal_error(decl->location, "Const must have mathematics type");
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
    return type_func(params, buf_len(params), ret, decl->func.is_variadic);
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
}

void entities_append_declaration_list(DeclarationList *decl_list) {
    for (size_t i = 0; i < decl_list->num_declarations; i++) {
        entity_append_declaration(decl_list->declarations[i]);
    }
}