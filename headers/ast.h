#ifndef YAP_AST_H
#define YAP_AST_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "token.h"
#include "tools.h"

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

typedef union Value {
    char c;
    signed char sc;
    unsigned char uc;
    bool b;
    short s;
    unsigned short us;
    int i;
    unsigned int ui;
    long l;
    unsigned long ul;
    long long ll;
    unsigned long long ull;
    uintptr_t p;
} Value;

typedef struct Entity {
    EntityKind kind;
    EntityState state;
    const char *name;
    Declaration *decl;
    Type *type;
    Value val;

} Entity;


typedef struct TypeField {
    const char *name;
    Type *type;
    size_t offset;
} TypeField;

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_BOOL,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LLONG,
    TYPE_ULLONG,
    TYPE_ENUM,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_POINTER,
    TYPE_FUNC,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ARRAY,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_CONST,
    TYPE_MAX
} TypeKind;


struct Type {
    TypeKind kind;
    size_t size;
    size_t align;
    Entity *entity;
    Type *base;
    bool is_nonmodify;
    union {
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        size_t num_elements;
        struct {
            Type **args;
            size_t num_args;
            Type *ret;
            bool is_variadic;
        } func;
    };
};

Type *type_new(TypeKind kind);

typedef struct AttributeArgument {
    const char *name;
    Expression *expr;
    SrcLocation location;
} AttributeArgument;

typedef struct Attribute {
    const char *name;
    AttributeArgument *args;
    size_t num_args;
    SrcLocation location;
} Attribute;

typedef struct AttributeList {
    Attribute *attributes;
    size_t num_attributes;
} AttributeList;

Attribute attribute_new(const char *name, AttributeArgument *args, size_t num_args, SrcLocation location);

enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_CONST
};

struct Typespec {
    SrcLocation location;
    TypespecKind kind;
    Typespec *base;
    union {
        const char *name;
        struct {
            Typespec **args;
            size_t num_args;
            Typespec *ret;
            bool is_variadic;
        } func;
        Expression *size;
    };
};

Typespec* typespec_new(TypespecKind kind, SrcLocation loc);
Typespec* typespec_name(const char *name, SrcLocation loc);
Typespec* typespec_pointer(Typespec *base, SrcLocation loc);
Typespec* typespec_array(Typespec *base, Expression *size, SrcLocation loc);
Typespec* typespec_func(Typespec **args, size_t num_args, Typespec *ret, bool is_variadic, SrcLocation loc);
Typespec* typespec_const(Typespec *base, SrcLocation loc);

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
    DECL_FUNC,
    DECL_ATTRIBUTE
};

struct EnumItem {
    const char *name;
    Expression *init;
    SrcLocation location;
};

struct AggregateItem {
    const char **names;
    size_t num_names;
    Typespec *type;
    SrcLocation location;
};

struct FuncParam {
    const char *name;
    Typespec *type;
    SrcLocation location;
};


struct Declaration {
    DeclarationKind kind;
    const char *name;
    AttributeList attributes;
    bool is_incomplete;
    union {
        Attribute attribute;
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
            Typespec *type;
            Expression *expr;
        } const_decl;
        struct {
            FuncParam *params;
            size_t num_params;
            Typespec *return_type;
            bool is_variadic;
            StatementBlock body;
        } func;
        struct {
            Typespec *type;
        } typedef_decl;
    };
    SrcLocation location;
};

Declaration* declaration_new(DeclarationKind kind, const char *name, SrcLocation loc);
Declaration* declaration_enum(const char *name, EnumItem *items, size_t num_items, SrcLocation loc);
Declaration* declaration_struct(const char *name, AggregateItem *items, size_t num_items, SrcLocation loc);
Declaration* declaration_union(const char *name, AggregateItem *items, size_t num_items, SrcLocation loc);
Declaration* declaration_aggregate(DeclarationKind kind, const char *name, AggregateItem *items, size_t num_items,
                                   SrcLocation loc);
Declaration* declaration_var(const char *name, Typespec *type, Expression *expr, SrcLocation loc);
Declaration* declaration_const(const char *name, Expression *expr, Typespec *type, SrcLocation loc);
Declaration* declaration_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret_type,
                              bool is_variadic, StatementBlock body, SrcLocation loc);
Declaration* declaration_typedef(const char *name, Typespec *type, SrcLocation loc);
Declaration* declaration_attribute(Attribute attribute, SrcLocation loc);
Attribute* get_declaration_attribute(Declaration *declaration, const char *name);

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
    SrcLocation location;
} CompoundField;

struct Expression {
    ExpressionKind kind;
    union {
        struct {
            TokenSuffix suffix;
            TokenMod mod;
            unsigned long long val;
        } int_lit;
        struct {
            TokenSuffix suffix;
            double val;
        } float_lit;
        struct {
            const char *val;
            TokenMod mod;
        } str_lit;
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
    SrcLocation location;
};

Expression* expression_new(ExpressionKind kind, SrcLocation loc);
Expression* expression_int(unsigned long long int_val, TokenSuffix suffix, TokenMod mod, SrcLocation loc);
Expression* expression_float(double float_val, TokenSuffix suffix, SrcLocation loc);
Expression* expression_str(const char *val, TokenMod mod, SrcLocation loc);
Expression* expression_name(const char *name, SrcLocation loc);
Expression* expression_cast(Typespec *cast_type, Expression *cast_expr, SrcLocation loc);
Expression* expression_unary(TokenKind operator, Expression *operand, SrcLocation loc);
Expression* expression_binary(TokenKind operator, Expression *left, Expression *right, SrcLocation loc);
Expression* expression_ternary(Expression *cond, Expression *then_expr, Expression *else_expr, SrcLocation loc);
Expression* expression_call(Expression *operand, Expression **args, size_t num_args, SrcLocation loc);
Expression* expression_index(Expression *operand, Expression *index, SrcLocation loc);
Expression* expression_field(Expression *operand, const char *field, SrcLocation loc);
Expression* expression_compound(Typespec *type, CompoundField *fields, size_t num_fields, SrcLocation loc);
Expression* expression_sizeof_type(Typespec *type, SrcLocation loc);
Expression* expression_sizeof_expr(Expression *sizeof_expr, SrcLocation loc);

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
    STMT_DECL,
    STMT_ATTR
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
            Typespec *type;
            Expression *init;
        } auto_assign;
        StatementBlock block;
        Expression *expr;
        Declaration *decl;
        Attribute attribute;
    };
    SrcLocation location;
};

Statement* statement_new(StatementKind kind, SrcLocation loc);
Statement* statement_if(Expression *cond, StatementBlock then, ElseIf *else_ifs, size_t num_else_ifs,
                        StatementBlock else_body, SrcLocation loc);
Statement* statement_for(Statement *init, Expression *cond, Statement *next, StatementBlock body, SrcLocation loc);
Statement* statement_while(Expression *cond, StatementBlock body, SrcLocation loc);
Statement* statement_do_while(Expression *cond, StatementBlock body, SrcLocation loc);
Statement* statement_switch(Expression *expr, SwitchCase *cases, size_t num_cases, SrcLocation loc);
Statement* statement_assign(TokenKind op, Expression *left, Expression *right, SrcLocation loc);
Statement* statement_auto_assign(const char *name, Typespec *type, Expression *init, SrcLocation loc);
Statement* statement_return(Expression *expr, SrcLocation loc);
Statement* statement_break(SrcLocation loc);
Statement* statement_continue(SrcLocation loc);
Statement* statement_block(StatementBlock block, SrcLocation loc);
Statement* statement_expr(Expression *expr, SrcLocation loc);
Statement* statement_decl(Declaration *decl, SrcLocation loc);
Statement* statement_attribute(Attribute attr, SrcLocation loc);
#endif