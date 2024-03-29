#include <stdio.h>
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
    buf[size] = 0;
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
