#include <malloc.h>
#include <stdint.h>
#include <assert.h>

#include "aalloc.h"
#include "tools.h"
#include "buf.h"

#define ARENA_ALIGNMENT 8
#define ARENA_BLOCK_SIZE 1024 * 1024

void arena_grow(ArenaMem *arena, size_t size_min) {
    size_t size = ALIGN_UP(MAX(ARENA_BLOCK_SIZE, size_min), ARENA_ALIGNMENT);
    arena->ptr = malloc(size);
    arena->end = arena->ptr + size;
    buf_push(arena->blocks, arena->ptr);
}

void* arena_alloc(ArenaMem *arena, size_t size) {
    if (size > (size_t)(arena->end - arena->ptr)) {
        arena_grow(arena, size);
        assert(size <= (size_t)(arena->end - arena->ptr));
    }

    void *ptr = arena->ptr;
    arena->ptr = ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT);
    assert(arena->ptr <= arena->end);
    return ptr;
}

void arena_free(ArenaMem *arena) {
    for (char **it = arena->blocks; it != buf_end(arena->blocks); it++) {
        free(*it);
    }
    buf_free(arena->blocks);
}