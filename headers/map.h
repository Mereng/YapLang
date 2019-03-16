#include <stddef.h>
#include <stdint.h>

typedef struct Map {
    void **keys;
    void **vals;
    size_t len;
    size_t cap;
} Map;

uint64_t hash_uint64(uint64_t x);
uint64_t hash_pointer(void *ptr);
uint64_t hash_bytes(const char *buf, size_t len);

void map_grow(Map *map, size_t new_cap);
void map_put(Map *map, void *key, void *val);
void* map_get(Map *map, void *key);