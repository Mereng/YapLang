#ifndef YAP_TOKEN_H
#define YAP_TOKEN_H

#include "tools.h"

typedef enum TokenKind {
    TOKEN_EOF,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_STR,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_BIN_AND,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_BIN_OR,
    TOKEN_XOR,
    TOKEN_ASSIGN,
    TOKEN_START_ASSIGN = TOKEN_ASSIGN,
    TOKEN_ADD_ASSIGN,
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
    TOKEN_BIN_NOT,
    TOKEN_NOT,
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_KEYWORD,
    TOKEN_COLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_QUESTION_MARK
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
        int64_t int_val;
        double float_val;
        const char *name;
        const char *str_val;
    };
    SrcLocation location;
} Token;

#endif