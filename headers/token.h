#ifndef YAP_TOKEN_H
#define YAP_TOKEN_H

typedef enum TokenKind {
    TOKEN_EOF = 0,
    TOKEN_START = 127,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_CHAR,
    TOKEN_STR,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_ADD_ASSIGN,
    TOKEN_START_ASSIGN = TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_AUTO_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_END_ASSIGN = TOKEN_RSHIFT_ASSIGN,
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_KEYWORD
} TokenKind;

typedef enum TokenMod {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_BIN,
    TOKENMOD_OCT,
    TOKENMOD_CHAR
} TokenMod;

typedef struct Token {
    TokenKind kind;
    TokenMod mod;
    const char *start;
    const char *end;
    union {
        uint64_t int_val;
        double float_val;
        const char *name;
        const char *str_val;
    };
} Token;

#endif