#include <stddef.h>
#include <stdint.h>

typedef struct Map {
    void **keys;
    void **vals;
    size_t len;
    size_t cap;
} Map;

uint64_t uint64_hash(uint64_t x);
uint64_t pointer_hash(void *ptr);
uint64_t string_hash(const char *str, size_t len);

void map_grow(Map *map, size_t new_cap);
void map_put(Map *map, void *key, void *val);
void* map_get(Map *map, void *key);