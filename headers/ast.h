#ifndef YAP_AST_H
#define YAP_AST_H

#include <stddef.h>
#include <stdint.h>
#include <token.h>
#include <stdbool.h>

typedef struct Type Type;
typedef enum TypespecKind TypespecKind;
typedef struct Typespec Typespec;
typedef struct EnumItem EnumItem;
typedef struct AggregateItem AggregateItem;
typedef struct FuncParam FuncParam;
typedef enum DeclarationKind DeclarationKind;
typedef struct Declaration Declaration;
typedef struct DeclarationList DeclarationList;
typedef enum ExpressionKind ExpressionKind;
typedef struct Expression Expression;
typedef enum StatementKind StatementKind;
typedef struct ElseIf ElseIf;
typedef struct SwitchCase SwitchCase;
typedef struct Statement Statement;
typedef struct StatementBlock StatementBlock;


void* ast_alloc(size_t size);
void* ast_dup(const void *src, size_t size);

typedef struct SrcLocation {
    const char *name;
    int line;
} SrcLocation;

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


typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_CHAR,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ARRAY,
    TYPE_POINTER,
    TYPE_FUNC,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING
} TypeKind;


struct Type {
    TypeKind kind;
    size_t size;
    size_t align;
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
            Type **args;
            size_t num_args;
            Type *ret;
        } func;
    };
};

Type *type_new(TypeKind kind);

enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
    TYPESPEC_NAME,
    TYPESPEC_FUNC
};

struct Typespec {
    SrcLocation location;
    TypespecKind kind;
    Type *type;
    union {
        const char *name;
        struct {
            Typespec **args;
            size_t num_args;
            Typespec *ret;
        } func;
        struct {
            Typespec *base;
            Expression *size;
        } array;
        struct {
            Typespec *base;
        } pointer;
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


struct Declaration {
    SrcLocation location;
    DeclarationKind kind;
    Entity *entity;
    const char *name;
    union {
        struct {
            EnumItem *items;
            size_t num_items;
        } enum_delc;
        struct {
            AggregateItem *items;
            size_t num_items;
        } aggregate;
        struct {
            Typespec *type;;
            Expression *expr;
        } var;
        struct {
            Expression *expr;
        } const_decl;
        struct {
            FuncParam *params;
            size_t num_params;
            Typespec *return_type;
            StatementBlock body;
        } func;
        struct {
            Typespec *type;
        } typedef_decl;
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

struct DeclarationList {
    Declaration **declarations;
    size_t num_declarations;
};

DeclarationList *declaration_list_new(Declaration **declarations, size_t num);

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

typedef enum CompoundFieldKind {
    COMPOUNDFIELD_DEFAULT,
    COMPOUNDFIELD_INDEX,
    COMPOUNDFIELD_NAME,
} CompoundFieldKind;

typedef struct CompoundField {
    CompoundFieldKind kind;
    Expression *init;
    union {
        const char *name;
        Expression *index;
    };
} CompoundField;

struct Expression {
    SrcLocation location;
    ExpressionKind kind;
    Type *type;
    union {
        int64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        struct {
            Typespec *type;
            CompoundField *fields;
            size_t num_fields;
        } compound;
        struct {
            Typespec *typespec;
            Expression *expr;
        } cast;
        struct {
            TokenKind op;
            Expression *operand;
        } unary;
        struct {
            TokenKind op;
            Expression *left;
            Expression *right;
        } binary;
        struct {
            Expression *cond;
            Expression *then_ex;
            Expression *else_ex;
        } ternary;
        struct {
            Expression *operand;
            Expression **args;
            size_t num_args;
        } call;
        struct {
            Expression *operand;
            Expression *index;
        } index;
        struct {
            Expression *operand;
            const char *name;
        } field;
        Typespec *size_of_type;
        Expression *size_of_expr;
    };
};

Expression* expression_new(ExpressionKind kind);
Expression* expression_int(int64_t int_val);
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
Expression* expression_compound(Typespec *type, CompoundField *fields, size_t num_fields);
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

struct SwitchCase {
    Expression **expressions;
    size_t num_expressions;
    StatementBlock body;
    bool is_default;
};

struct Statement {
    SrcLocation location;
    StatementKind kind;
    union {
        struct {
            Expression *cond;
            StatementBlock then;
            ElseIf *else_ifs;
            size_t num_else_ifs;
            StatementBlock else_body;
        } if_stmt;
        struct {
            Statement *init;
            Expression *cond;
            Statement *next;
            StatementBlock body;
        } for_stmt;
        struct {
            Expression *cond;
            StatementBlock body;
        } while_stmt;
        struct {
            Expression *expr;
            SwitchCase *cases;
            size_t num_cases;
        } switch_stmt;
        struct {
            TokenKind op;
            Expression *left;
            Expression *right;
        } assign;
        struct {
            const char *name;
            Expression *init;
        } auto_assign;
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