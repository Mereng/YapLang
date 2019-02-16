#ifndef YAP_RALLOC_H
#define YAP_RALLOC_H

#include <stddef.h>

typedef struct ArenaMem {
    char *ptr;
    char *end;
    char **blocks;
} ArenaMem;

void* arena_alloc(ArenaMem *arena, size_t size);

#endif