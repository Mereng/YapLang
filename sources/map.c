#include <malloc.h>
#include <assert.h>

#include "map.h"
#include "tools.h"

uint64_t uint64_hash(uint64_t x) {
    x *= 0xff51afd7ed558ccdul;
    x ^= x >> 32;
    return x;
}

uint64_t pointer_hash(void *ptr) {
    return  uint64_hash((uintptr_t)ptr);
}

uint64_t string_hash(const char *str, size_t len) {
    uint64_t fnv_int = 14695981039346656037ull;
    uint64_t fnv_mul = 1099511628211ull;
    uint64_t h = fnv_int;
    for (size_t i = 0; i < len; i++) {
        h ^= str[i];
        h *= fnv_mul;
    }
    return h;
}

void map_grow(Map *map, size_t new_cap) {
    new_cap = MAX(16, new_cap);
    Map new_map = {
        .items = calloc(new_cap, sizeof(MapItem)),
        .cap = new_cap
    };
    for (size_t i = 0; i < map->cap; i++) {
        MapItem *item = map->items + i;
        if (item->key) {
            map_put_hashed(&new_map, item->key, item->val, item->hash);
        }
    }
    free(map->items);
    *map = new_map;
}

void** map_put_hashed(Map *map, void *key, void *val, uint64_t hash) {
    assert(key && val);
    if (2 * map->len >= map->cap) {
        map_grow(map, 2 * map->cap);
    }
    uint32_t i = (uint32_t)(hash & (map->cap - 1));
    for (;;) {
        MapItem *item = map->items + i;
        if (!item->key) {
            map->len++;
            item->key = key;
            item->val = val;
            item->hash = hash;
            return &item->val;
        } else if (item->key == key) {
            item->val = val;
            return &item->val;
        }

        i++;
        if (i == map->cap) {
            i = 0;
        }
    }
}

void** map_put(Map *map, void *key, void *val) {
    return map_put_hashed(map, key, val, pointer_hash(key));
}

void* map_get_hashed(Map *map, void *key, uint64_t hash) {
    if (map->len <= 0) {
        return NULL;
    }
    uint32_t i = (uint32_t)(hash & (map->cap - 1));
    for (;;) {
        MapItem *item = map->items + i;
        if (item->key == key) {
            return item->val;
        } else if (!item->key) {
            return NULL;
        }
        i++;
        if (i == map->cap) {
            i = 0;
        }
    }
}

void* map_get(Map *map, void *key) {
    return map_get_hashed(map, key, pointer_hash(key));
}