#include <malloc.h>
#include <memory.h>
#include <inttypes.h>


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

Typespec* typespec_new(TypespecKind kind) {
    Typespec *typespec = ast_alloc(sizeof(Typespec));
    typespec->kind = kind;
    return typespec;
}
Typespec* typespec_name(const char *name) {
    Typespec *typespec = typespec_new(TYPESPEC_NAME);
    typespec->name = name;
    return typespec;
}
Typespec* typespec_pointer(Typespec *base) {
    Typespec *typespec = typespec_new(TYPESPEC_POINTER);
    typespec->ptr.base = base;
    return typespec;
}
Typespec* typespec_array(Typespec *base, Expression *size) {
    Typespec *typespec = typespec_new(TYPESPEC_ARRAY);
    typespec->arr.base = base;
    typespec->arr.size = size;
    return typespec;
}
Typespec* typespec_func(Typespec **args, size_t num_args, Typespec *ret) {
    Typespec *typespec = typespec_new(TYPESPEC_FUNC);
    typespec->func.args_types = args;
    typespec->func.num_args_types = num_args;
    typespec->func.return_type = ret;
    return typespec;
}

Declaration* declaration_new(DeclarationKind kind, const char *name) {
    Declaration *decl = ast_alloc(sizeof(Declaration));
    decl->kind = kind;
    decl->name = name;
    return decl;
}
Declaration* declaration_enum(const char *name, EnumItem *items, size_t num_items) {
    Declaration *decl = declaration_new(DECL_ENUM, name);
    decl->enum_delc.items = items;
    decl->enum_delc.num_items = num_items;
    return decl;
}
Declaration* declaration_struct(const char *name, AggregateItem *items, size_t num_items) {
    Declaration *decl = declaration_new(DECL_STRUCT, name);
    decl->agg.items = items;
    decl->agg.num_items = num_items;
    return decl;
}
Declaration* declaration_union(const char *name, AggregateItem *items, size_t num_items) {
    Declaration *decl = declaration_new(DECL_UNION, name);
    decl->agg.items = items;
    decl->agg.num_items = num_items;
    return decl;
}
Declaration* declaration_var(const char *name, Typespec *type, Expression *expr) {
    Declaration *decl = declaration_new(DECL_VAR, name);
    decl->var.type = type;
    decl->var.expr = expr;
    return decl;
}
Declaration* declaration_const(const char *name, Expression *expr) {
    Declaration *decl = declaration_new(DECL_CONST, name);
    decl->const_decl.expr = expr;
    return decl;
}
Declaration* declaration_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret_type,
        StatementBlock body) {
    Declaration *decl = declaration_new(DECL_FUNC, name);
    decl->func.params = params;
    decl->func.num_params = num_params;
    decl->func.return_type = ret_type;
    decl->func.body = body;
    return decl;
}
Declaration* declaration_typedef(const char *name, Typespec *type) {
    Declaration *decl = declaration_new(DECL_TYPEDEF, name);
    decl->typedef_decl.type = type;
    return decl;
}

Expression* expression_new(ExpressionKind kind) {
    Expression *expr = ast_alloc(sizeof(Declaration));
    expr->kind = kind;
    return expr;
}
Expression* expression_int(uint64_t int_val) {
    Expression *expr = expression_new(EXPR_INT);
    expr->int_val = int_val;
    return expr;
}
Expression* expression_float(double float_val) {
    Expression *expr = expression_new(EXPR_FLOAT);
    expr->float_val = float_val;
    return expr;
}
Expression* expression_str(const char *str_val) {
    Expression *expr = expression_new(EXPR_STR);
    expr->str_val = str_val;
    return expr;
}
Expression* expression_name(const char *name) {
    Expression *expr = expression_new(EXPR_NAME);
    expr->name = name;
    return expr;
}
Expression* expression_cast(Typespec *cast_type, Expression *cast_expr) {
    Expression *expr = expression_new(EXPR_CAST);
    expr->cast.type = cast_type;
    expr->cast.expr = cast_expr;
    return expr;
}
Expression* expression_unary(TokenKind operator, Expression *operand) {
    Expression *expr = expression_new(EXPR_UNARY);
    expr->unary.op = operator;
    expr->unary.operand = operand;
    return expr;
}
Expression* expression_binary(TokenKind operator, Expression *left, Expression *right) {
    Expression *expr = expression_new(EXPR_BINARY);
    expr->binary.op = operator;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}
Expression* expression_ternary(Expression *cond, Expression *then_expr, Expression *else_expr) {
    Expression *expr = expression_new(EXPR_TERNARY);
    expr->ternary.cond = cond;
    expr->ternary.then_ex = then_expr;
    expr->ternary.else_ex = else_expr;
    return expr;
}
Expression* expression_call(Expression *operand, Expression **args, size_t num_args) {
    Expression *expr = expression_new(EXPR_CALL);
    expr->call.operand = operand;
    expr->call.args = args;
    expr->call.num_args = num_args;
    return expr;
}
Expression* expression_index(Expression *operand, Expression *index) {
    Expression *expr = expression_new(EXPR_INDEX);
    expr->index.operand = operand;
    expr->index.index = index;
    return expr;
}
Expression* expression_field(Expression *operand, const char *field) {
    Expression *expr = expression_new(EXPR_FIELD);
    expr->field.operand = operand;
    expr->field.name = field;
    return expr;
}

Expression* expression_compound(Typespec *type, Expression **args, size_t num_args) {
    Expression *expr = expression_new(EXPR_COMPOUND);
    expr->compound.type = type;
    expr->compound.args = args;
    expr->compound.num_args = num_args;
    return expr;
}

Expression* expression_sizeof_type(Typespec *type) {
    Expression *expr = expression_new(EXPR_SIZEOF);
    expr->size_of.kind = SIZEOF_TYPE;
    expr->size_of.type = type;
    return expr;
}
Expression* expression_sizeof_expr(Expression *sizeof_expr) {
    Expression *expr = expression_new(EXPR_SIZEOF);
    expr->size_of.kind = SIZEOF_TYPE;
    expr->size_of.expr = sizeof_expr;
    return expr;
}

Statement* statement_new(StatementKind kind) {
    Statement *stmt = ast_alloc(sizeof(Statement));
    stmt->kind = kind;
    return stmt;
}
Statement* statement_if(Expression *cond, StatementBlock then, ElseIf *else_ifs, size_t num_else_ifs,
                        StatementBlock else_body) {
    Statement *stmt = statement_new(STMT_IF);
    stmt->if_stmt.cond = cond;
    stmt->if_stmt.then = then;
    stmt->if_stmt.else_ifs = else_ifs;
    stmt->if_stmt.num_else_ifs = num_else_ifs;
    stmt->if_stmt.else_body = else_body;
    return stmt;
}
Statement* statement_for(Statement *init, Expression *cond, Statement *next, StatementBlock body) {
    Statement *stmt = statement_new(STMT_FOR);
    stmt->for_stmt.init = init;
    stmt->for_stmt.cond = cond;
    stmt->for_stmt.next = next;
    stmt->for_stmt.body = body;
    return stmt;
}
Statement* statement_while(Expression *cond, StatementBlock body) {
    Statement *stmt = statement_new(STMT_WHILE);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.body = body;
    return stmt;
}
Statement* statement_do_while(Expression *cond, StatementBlock body) {
    Statement *stmt = statement_new(STMT_DO_WHILE);
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.body = body;
    return stmt;
}
Statement* statement_switch(Expression *expr, SwitchCase *cases, size_t num_cases) {
    Statement *stmt = statement_new(STMT_SWITCH);
    stmt->switch_stmt.expr = expr;
    stmt->switch_stmt.cases = cases;
    stmt->switch_stmt.num_cases = num_cases;
}
Statement* statement_assign(TokenKind op, Expression *left, Expression *right) {
    Statement *stmt = statement_new(STMT_ASSIGN);
    stmt->assign.op = op;
    stmt->assign.left = left;
    stmt->assign.right = right;
    return stmt;
}
Statement* statement_auto_assign(const char *name, Expression *init) {
    Statement *stmt = statement_new(STMT_AUTO_ASSIGN);
    stmt->auto_assign.name = name;
    stmt->auto_assign.init = init;
    return stmt;
}
Statement* statement_return(Expression *expr) {
    Statement *stmt = statement_new(STMT_RETURN);
    stmt->return_stmt.expr = expr;
    return stmt;
}
Statement* statement_break() {
    return statement_new(STMT_BREAK);
}
Statement* statement_continue() {
    return statement_new(STMT_CONTINUE);
}

Statement* statement_block(StatementBlock block) {
    Statement *stmt = statement_new(STMT_BLOCK);
    stmt->block = block;
    return stmt;
}
Statement* statement_expr(Expression *expr) {
    Statement *stmt = statement_new(STMT_EXPR);
    stmt->expr = expr;
    return stmt;
}
