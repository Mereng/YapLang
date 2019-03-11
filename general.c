Token token;
const char *stream;
const char *line_start;
int src_line;
const char *src_name;

#define fatal_syntax(...) syntax_error(__VA_ARGS__); exit(-1);
#define syntax_error(...) error((SrcLocation){src_name, src_line}, __VA_ARGS__);
#define fatal_error(...) error(__VA_ARGS__); exit(-2);

void error(SrcLocation location, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("%s (%d) ", location.name, location.line);
    vprintf(fmt, args);
    va_end(args);
}

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    va_end(args);
    exit(1);
}

typedef struct InternStr {
    size_t len;
    const char *str;
} InternStr;

ArenaMem strs_arena;
InternStr *interns;

const char* str_intern_range(const char *start, const char *end) {
    size_t len = end - start;

    for (int i = 0; i < buf_len(interns); i++) {
        if (interns[i].len == len && strncmp(interns[i].str, start, len) == 0) {
            return interns[i].str;
        }
    }

    char *str = arena_alloc(&strs_arena, len + 1);
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