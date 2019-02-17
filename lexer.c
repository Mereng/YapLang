#include <token.h>

struct {
    const char *func_keyword;
    const char *var_keyword;
    const char *const_keyword;
    const char *struct_keyword;
    const char *union_keyword;
    const char *typedef_keyword;
    const char *sizeof_keyword;
    const char *if_keyword;
    const char *else_keyword;
    const char *while_keyword;
    const char *do_keyword;
    const char *for_keyword;
    const char *switch_keyword;
    const char *case_keyword;
    const char *default_keyword;
    const char *break_keyword;
    const char *return_keyword;
    const char *continue_keyword;
    const char *enum_keyword;
} keywords;

const char **keywords_buf;
const char *start_keywords;
const char *end_keywords;

#define INIT_KEYWORD(name) keywords.name##_keyword = str_intern(#name); buf_push(keywords_buf, keywords.name##_keyword)

void keywords_init() {
    INIT_KEYWORD(func);
    INIT_KEYWORD(var);
    INIT_KEYWORD(const);
    INIT_KEYWORD(struct);
    INIT_KEYWORD(union);
    INIT_KEYWORD(typedef);
    INIT_KEYWORD(sizeof);
    INIT_KEYWORD(if);
    INIT_KEYWORD(else);
    INIT_KEYWORD(while);
    INIT_KEYWORD(do);
    INIT_KEYWORD(for);
    INIT_KEYWORD(switch);
    INIT_KEYWORD(case);
    INIT_KEYWORD(default);
    INIT_KEYWORD(break);
    INIT_KEYWORD(return);
    INIT_KEYWORD(continue);
    INIT_KEYWORD(enum);

    start_keywords = keywords.func_keyword;
    end_keywords = keywords.enum_keyword;
}

#undef INIT_KEYWORD

static inline bool is_keyword_str(const char *str) {
    return start_keywords <= str && str <= end_keywords;
}

Token token;
const char *stream;


uint8_t char_to_digit[] = {
        ['0'] = 0,
        ['1'] = 1,
        ['2'] = 2,
        ['3'] = 3,
        ['4'] = 4,
        ['5'] = 5,
        ['6'] = 6,
        ['7'] = 7,
        ['8'] = 8,
        ['9'] = 9,
        ['a'] = 10,
        ['b'] = 11,
        ['c'] = 12,
        ['d'] = 13,
        ['e'] = 14,
        ['f'] = 15,
        ['A'] = 10,
        ['B'] = 11,
        ['C'] = 12,
        ['D'] = 13,
        ['E'] = 14,
        ['F'] = 15
};


void parse_int() {
    uint64_t base = 10;
    if (*stream == '0') {
        stream++;
        if (tolower(*stream) == 'x') {
            base = 16;
            token.mod = TOKENMOD_HEX;
            stream++;
        } else if (tolower(*stream) == 'b') {
            token.mod = TOKENMOD_BIN;
            base = 2;
            stream++;
        } else if (isdigit(*stream)) {
            base = 8;
            token.mod = TOKENMOD_OCT;
        }
    }
    uint64_t val = 0;

    for (;;) {
        uint64_t digit = char_to_digit[*stream];

        if (digit == 0 && *stream != '0') {
            break;
        }

        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);
            digit = 0;
        }

        if (val > (UINT64_MAX - digit) / base) {
            syntax_error("Integer literal is overflow overflow");
            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
        }

        val = val * base + digit;
        stream++;
    }

    token.kind = TOKEN_INT;
    token.int_val = val;
}

void parse_float() {
    const char* start = stream;
    while (isdigit(*stream)) {
        stream++;
    }

    if (*stream == '.' ) {
        stream++;
    }

    while (isdigit(*stream)) {
        stream++;
    }
    if (tolower(*stream) == 'e') {
        stream++;
        if (*stream == '+' || *stream == '-') {
            stream++;
        }

        if (!isdigit(*stream)) {
            syntax_error("Expected digit after float literal exponent, found '%c'", *stream);
        }

        while (isdigit(*stream)) {
            stream++;
        }
    }

    double val = strtod(start, NULL);

    if (val == HUGE_VAL || val == -HUGE_VAL) {
        syntax_error("Float literal overflow");
    }

    token.kind = TOKEN_FLOAT;
    token.float_val = val;
}

char escape_to_char[] = {
        ['n'] = '\n',
        ['t'] = '\t',
        ['v'] = '\v',
        ['r'] = '\r',
        ['b'] = '\b',
        ['a'] = '\a',
        ['0'] = 0,
};

void parse_char() {
    assert(*stream == '\'');
    stream++;
    char val = 0;
    if (*stream == '\'') {
        syntax_error("Char literal can not be empty");
        stream++;
    } else if (*stream == '\n') {
        syntax_error("Char literal can not contain new line");
    } else if (*stream == '\\') {
        stream++;
        val = escape_to_char[*stream];
        if (val == 0 && *stream != '0') {
            syntax_error("Invalid char literal escape '\\%c", *stream);
        }
        stream++;
    } else {
        val = *stream;
        stream++;
    }

    if (*stream != '\'') {
        syntax_error("Expected closing char quote, got '%c'", *stream);
    }
    stream++;

    token.kind = TOKEN_CHAR;
    token.mod = TOKENMOD_CHAR;
    token.int_val = (uint64_t) val;
}

void parse_str() {
    assert(*stream == '"');
    char *str_buf = NULL;
    stream++;
    while (*stream && *stream != '"') {
        char val = *stream;
        if (val == '\n') {
            syntax_error("String literal can not contain new line");
        } else if (val == '\\') {
            stream++;
            val = escape_to_char[*stream];
            if (val == 0 && *stream != '0') {
                syntax_error("Invalid string literal escape '\\%c", *stream);
            }
        }

        buf_push(str_buf, val);
        stream++;
    }

    if (*stream) {
        assert(*stream == '"');
        stream++;
    } else {
        syntax_error("Unexpected end of file within string literal");
    }

    buf_push(str_buf, 0);
    token.kind = TOKEN_STR;
    token.str_val = str_buf;
}

#define CASE1(c1, c2, k) \
    case c1: \
        token.kind = *stream++; \
        if (*stream == c2) { \
            token.kind = k; \
            stream++; \
        } \
        break;

#define CASE2(c1, c2, c3, k1, k2) \
    case c1: \
        token.kind = *stream++; \
        if (*stream == c2) {\
            token.kind = k1; \
            stream++; \
        } else if (*stream == c3) {\
            token.kind = k2;\
            stream++; \
        } \
        break;


void next_token() {
    top:
    token.start = stream;
    token.mod = 0;
    switch (*stream) {
        case ' ': case '\n': case '\t': case '\r': case '\v':
            while (isspace(*stream)) {
                stream++;
            }
            goto top;
            break;
        case '\'':
            parse_char();
            break;
        case '"':
            parse_str();
            break;
        case '.':
            parse_float();
            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            while (isdigit(*stream)) {
                stream++;
            }
            char c = *stream;
            stream = token.start;
            if (c == '.' || tolower(c) == 'e') {
                parse_float();
            } else {
                parse_int();
            }

            break;
        }
        case 'a': case 'b': case 'c':  case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k':
        case 'l': case 'm': case 'n':  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v':
        case 'w': case 'x': case 'y':  case 'z':
        case 'A': case 'B': case 'C':  case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K':
        case 'L': case 'M': case 'N':  case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
        case 'W': case 'X': case 'Y': case 'Z':
        case '_':
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.name = str_intern_range(token.start, stream);
            token.kind = is_keyword_str(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
            break;
        case '<':
            token.kind = *stream++;
            if (*stream == '<') {
                token.kind = TOKEN_LSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_LSHIFT_ASSIGN;
                    stream++;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_LTEQ;
                stream++;
            }
            break;
        case '>':
            token.kind = *stream++;
            if (*stream == '>') {
                token.kind = TOKEN_RSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_RSHIFT_ASSIGN;
                    stream++;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_GTEQ;
                stream++;
            }
            break;
        CASE1('=', '=', TOKEN_EQ)
        CASE1(':', '=', TOKEN_AUTO_ASSIGN)
        CASE1('/', '=', TOKEN_DIV_ASSIGN)
        CASE1('*', '=', TOKEN_MUL_ASSIGN)
        CASE1('^', '=', TOKEN_XOR_ASSIGN)
        CASE1('%', '=', TOKEN_MOD_ASSIGN)
        CASE2('+', '+', '=', TOKEN_INC, TOKEN_ADD_ASSIGN)
        CASE2('-', '-', '=', TOKEN_DEC, TOKEN_SUB_ASSIGN)
        CASE2('&', '&', '=', TOKEN_AND, TOKEN_AND_ASSIGN)
        CASE2('|', '|', '=', TOKEN_OR, TOKEN_OR_ASSIGN)

        default:
            token.kind = *stream++;
            break;
    }
    token.end = stream;
}

#undef CASE1
#undef CASE2

static inline bool is_token(TokenKind kind) {
    return token.kind == kind;
}

static inline bool is_keyword(const char *name) {
    return is_token(TOKEN_KEYWORD) && token.name == name;
}

static inline bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    }
    return false;
}

static inline bool match_keyword(const char *name) {
    if (is_keyword(name)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

static inline bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        fatal("expected token %s, got %s", kind, token.kind);
        return false;
    }
}

int copy_kind_name(char *buf, size_t size,TokenKind kind) {
    int n;
    switch (kind) {
        case TOKEN_INT:
            n = snprintf(buf, size, "integer");
            break;
        case TOKEN_FLOAT:
            n = snprintf(buf, size, "float");
            break;
        case TOKEN_NAME:
            n = snprintf(buf, size, "name");
            break;
        default:
            if (kind < 128 && isprint(kind)) {
                n = snprintf(buf, size, "%c", kind);
            } else {
                n = snprintf(buf, size, "<ASCII %d>", kind);
            }
            break;
    }
    return n;
}

void init_stream(const char* str) {
    stream = str;
    next_token();
}


void print_token(Token token) {
    switch (token.kind) {
        case TOKEN_INT:
            printf("TOKEN INT %" PRIu64 "\n", token.int_val);
            break;
        case TOKEN_FLOAT:
            printf("TOKEN FLOAT %f\n", token.float_val);
            break;
        case TOKEN_NAME:
            printf("TOKEN NAME %.*s\n", (int)(token.end - token.start), token.name);
            break;
        default:
            printf("TOKEN %c\n", token.kind);
            break;
    }
}

void keywords_test() {
    keywords_init();
    assert(is_keyword_str(start_keywords));
    assert(is_keyword_str(end_keywords));
    for (const char **it = keywords_buf; it != buf_end(keywords_buf); it++) {
        assert(is_keyword_str(*it));
    }
    assert(!is_keyword_str(str_intern("Lol")));
}

void lex_test() {
    init_stream("0 0x100000000000000 01777777777777777777777 0b1111111111111111111111111111111111111111111111111111111111111111");
    assert(token.int_val == 0);
    next_token();
    assert(token.int_val == 0x100000000000000ul);
    next_token();
    assert(token.int_val == 01777777777777777777777ul);
    next_token();
    assert(token.int_val == 0b1111111111111111111111111111111111111111111111111111111111111111ul);
    init_stream("3.14 3e-3 .2 42.");
    assert(token.float_val == 3.14);
    next_token();
    assert(token.float_val == 3e-3);
    next_token();
    assert(token.float_val == .2);
    next_token();
    assert(token.float_val == 42.);
    init_stream("'a' '\\n'");
    assert(token.int_val == 'a');
    next_token();
    assert(token.int_val == '\n');
    init_stream("\"b\\n\\t\"");
    assert(strcmp(token.str_val, "b\n\t") == 0);

    init_stream("= := + += - -= <= <<= >>= >=");
    assert(token.kind == '=');
    next_token();
    assert(token.kind == TOKEN_AUTO_ASSIGN);
    next_token();
    assert(token.kind == '+');
    next_token();
    assert(token.kind == TOKEN_ADD_ASSIGN);
    next_token();
    assert(token.kind == '-');
    next_token();
    assert(token.kind == TOKEN_SUB_ASSIGN);
    next_token();
    assert(token.kind == TOKEN_LTEQ);
    next_token();
    assert(token.kind == TOKEN_LSHIFT_ASSIGN);
    next_token();
    assert(token.kind == TOKEN_RSHIFT_ASSIGN);
    next_token();
    assert(token.kind == TOKEN_GTEQ);
}