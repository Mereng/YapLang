#include <stddef.h>
#include <stdint.h>

typedef struct MapItem {
    void *key;
    void *val;
    uint64_t hash;
} MapItem;

typedef struct Map {
    MapItem *items;
    size_t len;
    size_t cap;
} Map;

uint64_t uint64_hash(uint64_t x);
uint64_t pointer_hash(void *ptr);
uint64_t string_hash(const char *str, size_t len);

void map_grow(Map *map, size_t new_cap);
void **map_put_hashed(Map *map, void *key, void *val, uint64_t hash);
void** map_put(Map *map, void *key, void *val);
void* map_get_hashed(Map *map, void *key, uint64_t hash);
void* map_get(Map *map, void *key);