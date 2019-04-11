#include <malloc.h>
#include <memory.h>
#include <inttypes.h>
#include <ast.h>

#include "ast.h"
#include "aalloc.h"

ArenaMem ast_arena;

void* ast_alloc(size_t size) {
    void *ptr = arena_alloc(&ast_arena, size);
    memset(ptr, 0, size);
    return ptr;
}
void* ast_dup(const void *src, size_t size) {
    if (size == 0) {
        return NULL;
    }

    void *ptr = arena_alloc(&ast_arena, size);
    memcpy(ptr, src, size);
    return ptr;
}

Type *type_new(TypeKind kind) {
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}

Attribute attribute_new(const char *name, AttributeArgument *args, size_t num_args, SrcLocation location) {
    return (Attribute) {.name = name, .args = args, .num_args = num_args, .location = location};
}

Typespec* typespec_new(TypespecKind kind, SrcLocation loc) {
    Typespec *typespec = ast_alloc(sizeof(Typespec));
    typespec->kind = kind;
    typespec->location = loc;
    return typespec;
}
Typespec* typespec_name(const char *name, SrcLocation loc) {
    Typespec *typespec = typespec_new(TYPESPEC_NAME, loc);
    typespec->name = name;
    return typespec;
}
Typespec* typespec_pointer(Typespec *base, SrcLocation loc) {
    Typespec *typespec = typespec_new(TYPESPEC_POINTER, loc);
    typespec->base = base;
    return typespec;
}
Typespec* typespec_array(Typespec *base, Expression *size, SrcLocation loc) {
    Typespec *typespec = typespec_new(TYPESPEC_ARRAY, loc);
    typespec->base = base;
    typespec->size = size;
    return typespec;
}
Typespec* typespec_func(Typespec **args, size_t num_args, Typespec *ret, bool is_variadic, SrcLocation loc) {
    Typespec *typespec = typespec_new(TYPESPEC_FUNC, loc);
    typespec->func.args = args;
    typespec->func.num_args = num_args;
    typespec->func.ret = ret;
    typespec->func.is_variadic = is_variadic;
    return typespec;
}
Typespec* typespec_const(Typespec *base, SrcLocation loc) {
    Typespec *typespec = typespec_new(TYPESPEC_CONST, loc);
    typespec->base = base;
    return typespec;
}

Declaration* declaration_new(DeclarationKind kind, const char *name, SrcLocation loc) {
    Declaration *decl = ast_alloc(sizeof(Declaration));
    decl->kind = kind;
    decl->name = name;
    decl->location = loc;
    return decl;
}
Declaration* declaration_enum(const char *name, EnumItem *items, size_t num_items, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_ENUM, name, loc);
    decl->enum_delc.items = items;
    decl->enum_delc.num_items = num_items;
    return decl;
}
Declaration* declaration_struct(const char *name, AggregateItem *items, size_t num_items, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_STRUCT, name, loc);
    decl->aggregate.items = items;
    decl->aggregate.num_items = num_items;
    return decl;
}
Declaration* declaration_union(const char *name, AggregateItem *items, size_t num_items, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_UNION, name, loc);
    decl->aggregate.items = items;
    decl->aggregate.num_items = num_items;
    return decl;
}
Declaration* declaration_aggregate(DeclarationKind kind, const char *name, AggregateItem *items, size_t num_items,
        SrcLocation loc) {
    Declaration *decl = declaration_new(kind, name, loc);
    decl->aggregate.items = items;
    decl->aggregate.num_items = num_items;
    return decl;
}
Declaration* declaration_var(const char *name, Typespec *type, Expression *expr, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_VAR, name, loc);
    decl->var.type = type;
    decl->var.expr = expr;
    return decl;
}
Declaration* declaration_const(const char *name, Expression *expr, Typespec *type, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_CONST, name, loc);
    decl->const_decl.expr = expr;
    decl->const_decl.type = type;
    return decl;
}
Declaration* declaration_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret_type,
        bool is_variadic, StatementBlock body, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_FUNC, name, loc);
    decl->func.params = params;
    decl->func.num_params = num_params;
    decl->func.return_type = ret_type;
    decl->func.is_variadic = is_variadic;
    decl->func.body = body;
    return decl;
}
Declaration* declaration_typedef(const char *name, Typespec *type, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_TYPEDEF, name, loc);
    decl->typedef_decl.type = type;
    return decl;
}

Declaration* declaration_attribute(Attribute attribute, SrcLocation loc) {
    Declaration *decl = declaration_new(DECL_ATTRIBUTE, NULL, loc);
    decl->attribute = attribute;
    return decl;
}

Attribute* get_declaration_attribute(Declaration *declaration, const char *name) {
    for (size_t i = 0; i < declaration->attributes.num_attributes; i++) {
        Attribute *attr = declaration->attributes.attributes + i;
        if (attr->name == name) {
            return attr;
        }
    }
    return NULL;
}

DeclarationList *declaration_list_new(Declaration **declarations, size_t num) {
    DeclarationList *decl_list = ast_alloc(sizeof(DeclarationList));
    decl_list->declarations = declarations;
    decl_list->num_declarations = num;
    return decl_list;
}

Expression* expression_new(ExpressionKind kind, SrcLocation loc) {
    Expression *expr = ast_alloc(sizeof(Declaration));
    expr->kind = kind;
    expr->location = loc;
    return expr;
}
Expression* expression_int(unsigned long long int_val, TokenSuffix suffix, TokenMod mod, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_INT, loc);
    expr->int_lit.val = int_val;
    expr->int_lit.suffix = suffix;
    expr->int_lit.mod = mod;
    return expr;
}
Expression* expression_float(double float_val, TokenSuffix suffix, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_FLOAT, loc);
    expr->float_lit.val = float_val;
    expr->float_lit.suffix = suffix;
    return expr;
}
Expression* expression_str(const char *val, TokenMod mod, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_STR, loc);
    expr->str_lit.val = val;
    expr->str_lit.mod = mod;
    return expr;
}
Expression* expression_name(const char *name, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_NAME, loc);
    expr->name = name;
    return expr;
}
Expression* expression_cast(Typespec *cast_type, Expression *cast_expr, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_CAST, loc);
    expr->cast.typespec = cast_type;
    expr->cast.expr = cast_expr;
    return expr;
}
Expression* expression_unary(TokenKind operator, Expression *operand, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_UNARY, loc);
    expr->unary.op = operator;
    expr->unary.operand = operand;
    return expr;
}
Expression* expression_binary(TokenKind operator, Expression *left, Expression *right, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_BINARY, loc);
    expr->binary.op = operator;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}
Expression* expression_ternary(Expression *cond, Expression *then_expr, Expression *else_expr, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_TERNARY, loc);
    expr->ternary.cond = cond;
    expr->ternary.then_ex = then_expr;
    expr->ternary.else_ex = else_expr;
    return expr;
}
Expression* expression_call(Expression *operand, Expression **args, size_t num_args, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_CALL, loc);
    expr->call.operand = operand;
    expr->call.args = args;
    expr->call.num_args = num_args;
    return expr;
}
Expression* expression_index(Expression *operand, Expression *index, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_INDEX, loc);
    expr->index.operand = operand;
    expr->index.index = index;
    return expr;
}
Expression* expression_field(Expression *operand, const char *field, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_FIELD, loc);
    expr->field.operand = operand;
    expr->field.name = field;
    return expr;
}

Expression* expression_compound(Typespec *type, CompoundField *fields, size_t num_fields, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_COMPOUND, loc);
    expr->compound.type = type;
    expr->compound.fields = fields;
    expr->compound.num_fields = num_fields;
    return expr;
}

Expression* expression_sizeof_type(Typespec *type, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_SIZEOF_TYPE, loc);
    expr->size_of_type = type;
    return expr;
}
Expression* expression_sizeof_expr(Expression *sizeof_expr, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_SIZEOF_EXPR, loc);
    expr->size_of_expr = sizeof_expr;
    return expr;
}
Expression* expression_alignof_type(Typespec *type, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_ALIGNOF_TYPE, loc);
    expr->align_of_type = type;
    return expr;
}
Expression* expression_alignof_expr(Expression *alignof_expr, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_ALIGNOF_EXPR, loc);
    expr->align_of_expr = alignof_expr;
    return expr;
}
Expression* expression_offsetof(Typespec *type, const char *name, SrcLocation loc) {
    Expression *expr = expression_new(EXPR_OFFSETOF, loc);
    expr->offset_of_field.type = type;
    expr->offset_of_field.name = name;
    return expr;
}

Statement* statement_new(StatementKind kind, SrcLocation loc) {
    Statement *stmt = ast_alloc(sizeof(Statement));
    stmt->kind = kind;
    stmt->location = loc;
    return stmt;
}
Statement* statement_if(Expression *cond, StatementBlock then, ElseIf *else_ifs, size_t num_else_ifs,
                        StatementBlock else_body, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_IF, loc);
    stmt->if_stmt.cond = cond;
    stmt->if_stmt.then = then;
    stmt->if_stmt.else_ifs = else_ifs;
    stmt->if_stmt.num_else_ifs = num_else_ifs;
    stmt->if_stmt.else_body = else_body;
    return stmt;
}
Statement* statement_for(Statement *init, Expression *cond, Statement *next, StatementBlock body, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_FOR, loc);
    stmt->for_stmt.init = init;
    stmt->for_stmt.cond = cond;
    stmt->for_stmt.next = next;
    stmt->for_stmt.body = body;
    return stmt;
}
Statement* statement_while(Expression *cond, StatementBlock body, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_WHILE, loc);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.body = body;
    return stmt;
}
Statement* statement_do_while(Expression *cond, StatementBlock body, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_DO_WHILE, loc);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.body = body;
    return stmt;
}
Statement* statement_switch(Expression *expr, SwitchCase *cases, size_t num_cases, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_SWITCH, loc);
    stmt->switch_stmt.expr = expr;
    stmt->switch_stmt.cases = cases;
    stmt->switch_stmt.num_cases = num_cases;
}
Statement* statement_assign(TokenKind op, Expression *left, Expression *right, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_ASSIGN, loc);
    stmt->assign.op = op;
    stmt->assign.left = left;
    stmt->assign.right = right;
    return stmt;
}
Statement* statement_auto_assign(const char *name, Typespec *type, Expression *init, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_AUTO_ASSIGN, loc);
    stmt->auto_assign.name = name;
    stmt->auto_assign.type = type;
    stmt->auto_assign.init = init;
    return stmt;
}
Statement* statement_return(Expression *expr, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_RETURN, loc);
    stmt->expr = expr;
    return stmt;
}
Statement* statement_break(SrcLocation loc) {
    return statement_new(STMT_BREAK, loc);
}
Statement* statement_continue(SrcLocation loc) {
    return statement_new(STMT_CONTINUE, loc);
}

Statement* statement_block(StatementBlock block, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_BLOCK, loc);
    stmt->block = block;
    return stmt;
}
Statement* statement_expr(Expression *expr, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_EXPR, loc);
    stmt->expr = expr;
    return stmt;
}
Statement* statement_decl(Declaration *decl, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_DECL, loc);
    stmt->decl = decl;
    return stmt;
}
Statement* statement_attribute(Attribute attr, SrcLocation loc) {
    Statement *stmt = statement_new(STMT_ATTR, loc);
    stmt->attribute = attr;
    return stmt;
}
