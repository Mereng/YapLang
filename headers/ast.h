#ifndef YAP_AST_H
#define YAP_AST_H

#include <stddef.h>
#include <stdint.h>
#include <token.h>
#include <stdbool.h>

typedef enum TypespecKind TypespecKind;
typedef struct FuncTypespec FuncTypespec;
typedef struct PointerTypespec PointerTypespec;
typedef struct ArrayTypespec ArrayTypespec;
typedef struct Typespec Typespec;
typedef enum DeclarationKind DeclarationKind;
typedef struct EnumItem EnumItem;
typedef struct AggregateItem AggregateItem;
typedef struct FuncParam FuncParam;
typedef struct EnumDeclaration EnumDeclaration;
typedef struct AggregateDeclaration AggregateDeclaration;
typedef struct VarDeclaration VarDeclaration;
typedef struct ConstDeclaration ConstDeclaration;
typedef struct FuncDeclaration FuncDeclaration;
typedef struct TypedefDeclaration TypedefDeclaration;
typedef struct Declaration Declaration;
typedef enum ExpressionKind ExpressionKind;
typedef struct CompoundExpression CompundExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct BinaryExpression BinaryExpression;
typedef struct TernaryExpression TernaryExpression;
typedef struct CastExpression CastExpression;
typedef struct CallExpression CallExpression;
typedef struct IndexExpression IndexExpression;
typedef struct FieldExpression FieldExpression;
typedef enum SizeofKind SizeofKind;
typedef struct SizeofExpression SizeofExpression;
typedef struct Expression Expression;
typedef enum StatementKind StatementKind;
typedef struct ElseIf ElseIf;
typedef struct IfStatement IfStatement;
typedef struct ForStatement ForStatement;
typedef struct WhileStatement WhileStatement;
typedef struct SwitchCase SwitchCase;
typedef struct SwitchStatement SwitchStatement;
typedef struct AssignStatement AssignStatement;
typedef struct AutoAssignStatement AutoAssignStatement;
typedef struct ReturnStatement ReturnStatement;
typedef struct Statement Statement;
typedef struct StatementBlock StatementBlock;


void* ast_alloc(size_t size);
void* ast_dup(const void *src, size_t size);

enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
    TYPESPEC_NAME,
    TYPESPEC_FUNC
};

struct FuncTypespec {
    Typespec **args_types;
    size_t num_args_types;
    Typespec *return_type;
};

struct PointerTypespec {
    Typespec *base;
};

struct ArrayTypespec {
    Typespec *base;
    Expression *size;
};

struct Typespec {
    TypespecKind kind;
    union {
        const char *name;
        FuncTypespec func;
        ArrayTypespec arr;
        PointerTypespec ptr;
    };
};

Typespec* typespec_new(TypespecKind kind);
Typespec* typespec_name(const char *name);
Typespec* typespec_pointer(Typespec *base);
Typespec* typespec_array(Typespec *base, Expression *size);
Typespec* typespec_func(Typespec **args, size_t num_args, Typespec *ret);


struct StatementBlock {
    Statement **statements;
    size_t num_statements;
};

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
    Expression *init;
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

struct EnumDeclaration {
    EnumItem *items;
    size_t num_items;
};

struct AggregateDeclaration {
    AggregateItem *items;
    size_t num_items;
};

struct VarDeclaration {
    Typespec *type;;
    Expression *expr;
};

struct ConstDeclaration {
    Expression *expr;
};

struct FuncDeclaration {
    FuncParam *params;
    size_t num_params;
    Typespec *return_type;
    StatementBlock body;
};

struct TypedefDeclaration {
    Typespec *type;
};


struct Declaration {
    DeclarationKind kind;
    const char *name;
    union {
        EnumDeclaration enum_delc;
        AggregateDeclaration agg;
        VarDeclaration var;
        ConstDeclaration const_decl;
        FuncDeclaration func;
        TypedefDeclaration typedef_decl;
    };
};

Declaration* declaration_new(DeclarationKind kind, const char *name);
Declaration* declaration_enum(const char *name, EnumItem *items, size_t num_items);
Declaration* declaration_struct(const char *name, AggregateItem *items, size_t num_items);
Declaration* declaration_union(const char *name, AggregateItem *items, size_t num_items);
Declaration* declaration_aggregate(DeclarationKind kind, const char *name, AggregateItem *items, size_t num_items);
Declaration* declaration_var(const char *name, Typespec *type, Expression *expr);
Declaration* declaration_const(const char *name, Expression *expr);
Declaration* declaration_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret_type, StatementBlock body);
Declaration* declaration_typedef(const char *name, Typespec *type);

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
    EXPR_TERNARY,
    EXPR_SIZEOF_TYPE,
    EXPR_SIZEOF_EXPR
};

struct CompoundExpression {
    Typespec *type;
    Expression **args;
    size_t num_args;
};

struct CastExpression {
    Typespec *type;
    Expression *expr;
};

struct UnaryExpression {
    TokenKind op;
    Expression *operand;
};

struct BinaryExpression {
    TokenKind op;
    Expression *left;
    Expression *right;
};

struct TernaryExpression {
    Expression *cond;
    Expression *then_ex;
    Expression *else_ex;
};

struct CallExpression {
    Expression *operand;
    Expression **args;
    size_t num_args;
};

struct IndexExpression {
    Expression *operand;
    Expression *index;
};

struct FieldExpression {
    Expression *operand;
    const char *name;
};

struct Expression {
    ExpressionKind kind;
    union {
        uint64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        CompundExpression compound;
        CastExpression cast;
        UnaryExpression unary;
        BinaryExpression binary;
        TernaryExpression ternary;
        CallExpression call;
        IndexExpression index;
        FieldExpression field;
        Typespec *size_of_type;
        Expression *size_of_expr;
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
Expression* expression_compound(Typespec *type, Expression **args, size_t num_args);
Expression* expression_sizeof_type(Typespec *type);
Expression* expression_sizeof_expr(Expression *sizeof_expr);

enum StatementKind {
    STMT_NONE,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_FOR,
    STMT_WHILE,
    STMT_DO_WHILE,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_AUTO_ASSIGN,
    STMT_EXPR,
    STMT_DECL
};

struct ElseIf {
    Expression *cond;
    StatementBlock body;
};

struct IfStatement {
    Expression *cond;
    StatementBlock then;
    ElseIf *else_ifs;
    size_t num_else_ifs;
    StatementBlock else_body;
};

struct ForStatement {
    Statement *init;
    Expression *cond;
    Statement *next;
    StatementBlock body;
};

struct WhileStatement {
    Expression *cond;
    StatementBlock body;
};

struct SwitchCase {
    Expression **expressions;
    size_t num_expressions;
    StatementBlock body;
    bool is_default;
};

struct SwitchStatement {
    Expression *expr;
    SwitchCase *cases;
    size_t num_cases;
};

struct AssignStatement {
    TokenKind op;
    Expression *left;
    Expression *right;
};

struct AutoAssignStatement {
    const char *name;
    Expression *init;
};

struct ReturnStatement {
    Expression *expr;
};

struct Statement {
    StatementKind kind;
    union {
        IfStatement if_stmt;
        ForStatement for_stmt;
        WhileStatement while_stmt;
        SwitchStatement switch_stmt;
        AssignStatement assign;
        AutoAssignStatement auto_assign;
        ReturnStatement return_stmt;
        StatementBlock block;
        Expression *expr;
        Declaration *decl;
    };
};

Statement* statement_new(StatementKind kind);
Statement* statement_if(Expression *cond, StatementBlock then, ElseIf *else_ifs, size_t num_else_ifs,
        StatementBlock else_body);
Statement* statement_for(Statement *init, Expression *cond, Statement *next, StatementBlock body);
Statement* statement_while(Expression *cond, StatementBlock body);
Statement* statement_do_while(Expression *cond, StatementBlock body);
Statement* statement_switch(Expression *expr, SwitchCase *cases, size_t num_cases);
Statement* statement_assign(TokenKind op, Expression *left, Expression *right);
Statement* statement_auto_assign(const char *name, Expression *init);
Statement* statement_return(Expression *expr);
Statement* statement_break();
Statement* statement_continue();
Statement* statement_block(StatementBlock block);
Statement* statement_expr(Expression *expr);
Statement* statement_decl(Declaration *decl);
#endif