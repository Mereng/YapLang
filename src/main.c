#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <limits.h>
#include <math.h>

#define MAX(x, y) (x >= y ? x : y)


typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[0];
} BufHdr;

#define buf__hdr(b) ((BufHdr*)((char*)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + n <= buf_cap(b))
#define buf__fit(b, n) (buf__fits(b, n) ? 0 : (b = buf__grow(b, buf_len(b) + n, sizeof(*b))))

#define buf_len(b) (b ? buf__hdr(b)->len : 0)
#define buf_cap(b) (b ? buf__hdr(b)->cap : 0)
#define buf_push(b, x) (buf__fit(b, 1), b[buf_len(b)] = x, buf__hdr(b)->len++)
#define buf_free(b) (b ? (free(buf__hdr(b)), b = NULL) : 0)


void* buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    size_t new_cap = MAX(1 + 2 * buf_cap(buf), elem_size);
    assert(new_len <= new_cap);
    size_t new_size = offsetof(BufHdr, buf) + new_cap * elem_size;
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = realloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = malloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;

    return new_hdr->buf;
}

void fatal(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    va_end(args);
    exit(1);
}

void syntax_error(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("Syntax error: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

typedef enum TokenKind {
    TOKEN_START = 127,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_CHAR
} TokenKind;

typedef enum TokenMod {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_BIN,
    TOKENMOD_OCT,
    TOKENMOD_CHAR
} TokenMod;

extern inline bool is_token(TokenKind kind);
extern inline bool match_token(TokenKind kind);
extern inline bool expect_token(TokenKind kind);


typedef struct Token {
    TokenKind kind;
    TokenMod mod;
    const char *start;
    const char *end;
    union {
        uint64_t int_val;
        double float_val;
        const char *name;
    };
} Token;


typedef struct InternStr {
    size_t len;
    const char *str;
} InternStr;

static InternStr *interns;

const char* str_intern_range(const char *start, const char *end) {
    size_t len = end - start;

    for (int i = 0; i < buf_len(interns); i++) {
        if (interns[i].len == len && strncmp(interns[i].str, start, len) == 0) {
            return interns[i].str;
        }
    }

    char *str = malloc(len + 1);
    memcpy(str, start, len);
    str[len] = 0;

    buf_push(interns, ((InternStr){len, str}));
    return str;
}

const char* str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

Token token;
const char *stream;

uint8_t char_to_digit[256] = {
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
            stream++;
        } else if (tolower(*stream) == 'b') {
            base = 2;
            stream++;
        } else if (isdigit(*stream)) {
            base = 8;
        } else {
            syntax_error("Invalid literal suffix: '%c'", *stream);
        }
    }
    uint64_t val = 0;

    for (;;) {
        uint64_t digit = char_to_digit[*stream];

        if (digit == 0 && *stream != '0') {
            break;
        }

        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %lu", *stream, base);
        }

        if (val > (UINT64_MAX - digit) / base) {
            syntax_error("Integer literal is overflow overflow");
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

char escape_to_char[256] = {
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
    token.int_val = val;
}

void parse_str() {

}

void next_token() {
    token.start = stream;
    token.mod = 0;
    switch (*stream) {
    case ' ': case '\n': case '\t': case '\r': case '\v':
        while (isspace(*stream)) {
            stream++;
        }
        break;
    case '\'':
        parse_char();
        break;
    case '"':
        break;
    case '.':
        parse_float();
        break;
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
        while (isdigit(*stream)) {
            stream++;
        }
        if (*stream == '.' || tolower(*stream) == 'e') {
            stream = token.start;
            parse_float();
        } else {
            stream = token.start;
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
        while (isalpha(*stream) || *stream == '_') {
            stream++;
        }
        token.kind = TOKEN_NAME;
        token.name = str_intern_range(token.start, stream);
        break;
    default:
        token.kind = *stream++;
        break;
    }
    token.end = stream;
}

inline bool is_token(TokenKind kind) {
    return token.kind == kind;
}

inline bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    }
    return false;
}

inline bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        fatal("expected token %s, got %s", kind, token.kind);
        return false;
    }
}

const int copy_kind_name(char *buf, size_t size,TokenKind kind) {
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


void buf_test() {
    int *buf = NULL;

    buf_push(buf, 2);
    buf_push(buf, 3);

    assert(buf[0] == 2);
    assert(buf[1] == 3);

    buf_free(buf);
    assert(buf == NULL);
}

void print_token(Token token) {
    switch (token.kind) {
    case TOKEN_INT:
        printf("TOKEN INT %ld\n", token.int_val);
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

void lex_test() {
    init_stream("18446744073709551615 0x100000000000000 01777777777777777777777 0b1111111111111111111111111111111111111111111111111111111111111111");
    next_token();
    assert(token.int_val == 18446744073709551615ul);
    printf("%lu\n", token.int_val);
    next_token();
    next_token();
    assert(token.int_val == 0x100000000000000ul);
    printf("%lu\n", token.int_val);
    next_token();
    next_token();
    assert(token.int_val == 01777777777777777777777ul);
    printf("%lu\n", token.int_val);
    next_token();
    next_token();
    assert(token.int_val == 0b1111111111111111111111111111111111111111111111111111111111111111ul);
    printf("%lu\n", token.int_val);

    init_stream("3.14 3e-3 .2 42.");
    assert(token.float_val == 3.14);
    printf("%f\n", token.float_val);
    next_token();
    next_token();
    assert(token.float_val == 3e-3);
    printf("%f\n", token.float_val);
    next_token();
    next_token();
    assert(token.float_val == .2);
    printf("%f\n", token.float_val);
    next_token();
    next_token();
    assert(token.float_val == 42.);
    printf("%f\n", token.float_val);

    init_stream("'a' '\\n'");
    assert(token.int_val == 'a');
    printf("%c\n", token.int_val);
    next_token();
    next_token();
    assert(token.int_val == '\n');
    printf("%c\n", token.int_val);
}

void str_intern_test() {
    char x[] = "Hello";
    char y[] = "Hello";
    assert(x != y);
    const char *px = str_intern(x);
    const char *py = str_intern(y);
    assert(px == py);
    char z[] = "hello!";
    const char *pz = str_intern(z);
    assert(px != pz);
}

int main() {
    buf_test();
    lex_test();
    str_intern_test();
    return 0;
}
