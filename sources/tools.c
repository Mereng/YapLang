#include <tools.h>
#include <stdarg.h>
#include <malloc.h>

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