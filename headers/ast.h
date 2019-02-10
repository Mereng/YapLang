#ifndef YAP_AST_H
#define YAP_AST_H

#include <stddef.h>
#include <stdint.h>
#include <token.h>

typedef enum TypespecKind TypespecKind;
typedef struct FuncTypesec FuncTypesec;
typedef struct Typespec Typespec;
typedef enum DeclarationKind DeclarationKind;
typedef struct EnumItem EnumItem;
typedef struct AggregateItem AggregateItem;
typedef struct FuncParam FuncParam;
typedef struct FuncDeclaration FuncDeclaration;
typedef struct Declaration Declaration;
typedef enum ExpressionKind ExpressionKind;
typedef struct Expression Expression;
typedef enum StatementKind StatementKind;
typedef struct Statement Statement;
typedef struct StatementBlock StatementBlock;
typedef struct ElseIf ElseIf;
typedef struct Case Case;


enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
    TYPESPEC_NAME,
    TYPESPEC_FUNC
};

struct FuncTypesec {
    Typespec **args_types;
    size_t num_args_types;
    Typespec *return_type;
};

struct Typespec {
    TypespecKind kind;
    struct {
        FuncTypesec *func;
        const char *name;
        struct {
            Typespec *base_type;
            Expression *size;
        };
    };
};

Typespec* typespec_new(TypespecKind kind);
Typespec* typespec_name(const char *name);
Typespec* typespec_pointer(Typespec *base);
Typespec* typespec_array(Typespec *base, Expression *size);
Typespec* typespec_func(FuncTypesec *func);

enum DeclarationKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPEDEF,
    DECL_FUNC
};

struct EnumItem {
    const char *name;
    Typespec *type;
};

struct AggregateItem {
    const char **names;
    size_t num_names;
    Typespec *type;
};

struct FuncParam {
    const char *name;
    Typespec *type;
};

struct FuncDeclaration {
    FuncParam *params;
    size_t num_params;
    Typespec *return_type;
};


struct Declaration {
    DeclarationKind kind;
    const char *name;
    union {
        struct {
            EnumItem *enum_items;
            size_t num_enum_items;
        };
        struct {
            AggregateItem *aggregate_items;
            size_t num_aggregate_items;
        };
        struct {
            Typespec *type;
            Expression *expr;
        };
    };
};

enum ExpressionKind {
    EXPR_NONE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CAST,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_COMPOUND,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY
};

struct Expression {
    ExpressionKind kind;
    TokenKind op;
    union {
        uint64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        //compound
        struct {
            Typespec *compound_type;
            Expression **compound_args;
            size_t num_compound_args;
        };
        //cast
        struct {
            Typespec *cast_type;
            Expression *cast_expr;
        };
        //unary
        struct {
            Expression *operand;
            union {
                struct {
                    Expression **args;
                    size_t num_args;
                };
                Expression *index;
                const char *field;
            };
        };
        //binary
        struct {
            Expression *left;
            Expression *right;
        };
        //ternary
        struct {
            Expression *cond;
            Expression *then_expr;
            Expression *else_expr;
        };

    };
};

Expression* expression_new(ExpressionKind kind);
Expression* expression_int(uint64_t int_val);
Expression* expression_float(double float_val);
Expression* expression_str(const char *str_val);
Expression* expression_name(const char *name);
Expression* expression_cast(Typespec *cast_type, Expression *cast_expr);
Expression* expression_unary(TokenKind operator, Expression *operand);
Expression* expression_binary(TokenKind operator, Expression *left, Expression *right);
Expression* expression_ternary(Expression *cond, Expression *then_expr, Expression *else_expr);
Expression* expression_call(Expression *operand, Expression **args, size_t num_args);
Expression* expression_index(Expression *operand, Expression *index);
Expression* expression_field(Expression *operand, const char *field);

void print_expression(Expression *expr);

enum StatementKind {
    STMT_NONE,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_FOR,
    STMT_DO,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_AUTO_ASSIGN,
    STMT_EXPR,
};

struct StatementBlock {
    Statement **statements;
    size_t num_statements;
};

struct ElseIf {
    Expression *cond;
    StatementBlock *body;
};

struct Case {
    Expression **expressions;
    size_t num_expressions;
    StatementBlock *block;
};

struct Statement {
    StatementKind kind;
    Expression *expr;
    StatementBlock block;
    union {
        //if
        struct {
            ElseIf *else_ifs;
            size_t num_else_ifs;
            StatementBlock else_block;
        };
        //for
        struct {
            StatementBlock *for_init_block;
            StatementBlock *for_update_block;
        };
        //switch
        struct {
            Case **cases;
            size_t num_cases;
        };
        //auto assign
        struct {
            const char *name;
        };
        //assign ops
        struct {
            Expression *rexpr;
        };
    };
};

#endif