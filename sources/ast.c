#include <ast.h>
#include <malloc.h>
#include <inttypes.h>

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

void print_type(Typespec *type) {
    switch (type->kind) {
        case TYPESPEC_NAME:
            printf("%s", type->name);
            break;
        case TYPESPEC_POINTER:
            printf("(pointer ");
            print_type(type->base_type);
            printf(")");
            break;
        case TYPESPEC_ARRAY:
            printf("(array ");
            print_type(type->base_type);
            printf(" ");
            print_expression(type->size);
            printf(")");
            break;
        case TYPESPEC_FUNC:
            printf("(func (");
            for (Typespec **it = type->func->args_types; it != type->func->args_types + type->func->num_args_types; it++) {
                printf(" ");
                print_type(*it);
            }
            printf(") ");
            print_type(type->func->return_type);
            printf(" )");
            break;
        default:
            break;
    }
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
Expression* expression_call(Expression *operand, Expression **args, size_t num_args) {
    Expression *expr = expression_new(EXPR_CALL);
    expr->operand = operand;
    expr->args = args;
    expr->num_args = num_args;
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

void print_expression(Expression *expr) {
    switch (expr->kind) {
        case EXPR_INT:
            printf("%"PRIu64, expr->int_val);
            break;
        case EXPR_FLOAT:
            printf("%f", expr->float_val);
            break;
        case EXPR_STR:
            printf("\"%s\"", expr->str_val);
            break;
        case EXPR_NAME:
            printf("%s", expr->name);
            break;
        case EXPR_CAST:
            printf("(cast ");
            print_type(expr->cast_type);
            print_expression(expr->cast_expr);
            printf(")");
            break;
        case EXPR_CALL:
            printf("(call ");
            print_expression(expr->operand);
            for (Expression **it = expr->args; it != expr->num_args + expr->args; it++){
                print_expression(*it);
            }
            printf(")");
            break;
        case EXPR_INDEX:
            printf("(index ");
            print_expression(expr->operand);
            printf(" ");
            print_expression(expr->index);
            printf(")");
            break;
        case EXPR_FIELD:
            printf("(field ");
            print_expression(expr->operand);
            printf(" %s)", expr->field);
            break;
        case EXPR_UNARY:
            printf("(%c ", expr->op);
            print_expression(expr->operand);
            printf(")");
            break;
        case EXPR_BINARY:
            printf("(%c ", expr->op);
            print_expression(expr->left);
            printf(" ");
            print_expression(expr->right);
            printf(")");
            break;
        case EXPR_TERNARY:
            printf("( ");
            print_expression(expr->cond);
            printf(" ? ");
            print_expression(expr->then_expr);
            printf(" : ");
            print_expression(expr->else_expr);
            printf(")");
        default:
            break;
    }
}