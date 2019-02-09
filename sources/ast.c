#include <ast.h>
#include <malloc.h>

Typespec* typespec_new(TypespecKind kind) {
    Typespec *typespec = calloc(1, sizeof(Typespec));
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
    typespec->base_type = base;
    return typespec;
}
Typespec* typespec_array(Typespec *base, Expression *size) {
    Typespec *typespec = typespec_new(TYPESPEC_ARRAY);
    typespec->base_type = base;
    typespec->size = size;
    return typespec;
}
Typespec* typespec_func(FuncTypesec *func) {
    Typespec *typespec = typespec_new(TYPESPEC_FUNC);
    typespec->func = func;
    return typespec;
}

Expression* expression_new(ExpressionKind kind) {
    Expression *expr = calloc(1, sizeof(Expression));
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
    expr->cast_type = cast_type;
    expr->cast_expr = cast_expr;
    return expr;
}
Expression* expression_unary(TokenKind operator, Expression *operand) {
    Expression *expr = expression_new(EXPR_UNARY);
    expr->op = operator;
    expr->operand = operand;
    return expr;
}
Expression* expression_binary(TokenKind operator, Expression *left, Expression *right) {
    Expression *expr = expression_new(EXPR_BINARY);
    expr->op = operator;
    expr->left = left;
    expr->right = right;
    return expr;
}
Expression* expression_ternary(Expression *cond, Expression *then_expr, Expression *else_expr) {
    Expression *expr = expression_new(EXPR_TERNARY);
    expr->cond = cond;
    expr->then_expr = then_expr;
    expr->else_expr = else_expr;
    return expr;
}
Expression* expression_call(Expression *operand, Expression **args) {
    Expression *expr = expression_new(EXPR_CALL);
    expr->operand = operand;
    expr->args = args;
    return expr;
}
Expression* expression_index(Expression *operand, Expression *index) {
    Expression *expr = expression_new(EXPR_INDEX);
    expr->operand = operand;
    expr->index = index;
    return expr;
}
Expression* expression_field(Expression *operand, const char *field) {
    Expression *expr = expression_new(EXPR_FIELD);
    expr->operand = operand;
    expr->field = field;
    return expr;
}