#ifndef YAP_TOOLS_H
#define YAP_TOOLS_H

#include <stdbool.h>
#include <stddef.h>

#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) (ALIGN_DOWN((n) + (a) - 1, (a)))
#define ALIGN_DOWN_PTR(p, a) ((void*)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void*)ALIGN_UP((uintptr_t)p, (a)))

typedef struct SrcLocation {
    const char *name;
    int line;
} SrcLocation;

char* stringf(const char *fmt, ...);
char* read_file(const char *path);
bool write_file(const char *path, const char *buf, size_t size);

#endif
