Token token;
const char *stream;
const char *line_start;
SrcLocation location_builtin = {.name = "<builtin>"};

#define fatal_syntax(...) error_here(__VA_ARGS__); exit(-1)
#define error_here(...) error(token.location, __VA_ARGS__)
#define fatal_error(...) error(__VA_ARGS__); exit(-2)
#define warning_here(...) warning(token.location, __VA_ARGS__)

void error(SrcLocation location, const char* fmt, ...) {
    if (location.name == NULL) {
        location = location_builtin;
    }
    va_list args;
    va_start(args, fmt);
    printf("%s (%d): error: ", location.name, location.line);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
}

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
    exit(1);
}

void warning(SrcLocation loc, const char *fmt, ...) {
    if (loc.name == NULL) {
        loc = location_builtin;
    }
    va_list args;
    va_start(args, fmt);
    printf("%s (%d): warning: ", loc.name, loc.line);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
}

typedef struct InternStr {
    size_t len;
    struct InternStr *next;
    char str[];
} InternStr;

ArenaMem strs_arena;
Map interns;

const char* str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    uint64_t hash = hash_bytes(start, len);
    if (hash == 0) {
        hash = 1;
    }
    InternStr *intern = map_get(&interns, (void*)(uintptr_t)hash);
    for (InternStr *it = intern; it; it = it->next) {
        if (it->len == len && strncmp(it->str, start, len) == 0) {
            return it->str;
        }
    }

    InternStr *new = arena_alloc(&strs_arena, offsetof(InternStr, str) + len + 1);
    new->len = len;
    new->next = intern;
    memcpy(new->str, start, len);
    new->str[len] = 0;
    map_put(&interns, (void*)(uintptr_t)hash, new);
    return new->str;
}

const char* str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

#define MAX_SEARCH_PATHS 256
struct {
    const char *paths[MAX_SEARCH_PATHS];
    size_t num_paths;
} packages_search_paths;

void packages_search_paths_add(const char *path) {
    packages_search_paths.paths[packages_search_paths.num_paths++] = str_intern(path);
}