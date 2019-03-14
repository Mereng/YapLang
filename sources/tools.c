#include <tools.h>
#include <stdarg.h>
#include <malloc.h>
#include <string.h>

char* stringf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t n = (size_t)(vsnprintf(NULL, 0, fmt, args)) + 1;
    va_end(args);
    char *str = malloc(n);
    va_start(args, fmt);
    vsnprintf(str, n, fmt, args);
    va_end(args);
    return str;
}

char* read_file(const char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *buf = malloc(size + 1);
    if (size != 0) {
        if (fread(buf, size, 1, file) != 1) {
            fclose(file);
            free(buf);
            return NULL;
        }
    }
    fclose(file);
    return buf;
}

bool write_file(const char *path, const char *buf, size_t size) {
    FILE *file = fopen(path, "w");
    if (!file) {
        return false;
    }
    size_t n = fwrite(buf, size, 1, file);
    fclose(file);
    return n == 1;
}

const char* get_ext(const char *path) {
    for (const char *it = path + strlen(path); it != path; it--) {
        if (it[-1] == '.') {
            return it;
        }
    }
    return NULL;
}

char* replace_ext(const char *path, const char *new_ext) {
    const char *ext = get_ext(path);
    if (!ext) {
        return NULL;
    }
    size_t base_len = ext - path;
    size_t new_ext_len = strlen(new_ext);
    size_t new_path_len = base_len + new_ext_len;
    char *new_path = malloc(new_path_len + 1);
    memcpy(new_path, path, base_len);
    memcpy(new_path + base_len, new_ext, new_ext_len);
    new_path[new_path_len] = 0;
    return new_path;
}