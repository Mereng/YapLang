
#include <ast.h>

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
}


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

void buf_test() {
    int *buf = NULL;

    buf_push(buf, 2);
    buf_push(buf, 3);

    assert(buf[0] == 2);
    assert(buf[1] == 3);

    buf_free(buf);
    assert(buf == NULL);
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


void expr_test() {
    Expression *exprs[] = {
        expression_unary('-', expression_float(3.14)),
        expression_binary('/', expression_int(5), expression_int(10)),
        expression_ternary(expression_name("isTrue"), expression_str("yes"), expression_str("no")),
        expression_field(expression_name("user"), "name"),
        expression_cast(typespec_pointer(typespec_name("float")), expression_name("void_ptr")),
        expression_call(expression_name("sum"), (Expression*[]){expression_int(2), expression_int(5)}, 2),
        expression_index(expression_field(expression_name("user"), "photos"), expression_int(2))
    };

    for (Expression **it = exprs; it != exprs + sizeof(exprs) / sizeof(*exprs) ; it++) {
        print_expression(*it);
        printf("\n");
    }
}

void ast_test() {
    expr_test();
}