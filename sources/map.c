#include <malloc.h>
#include <assert.h>

#include "map.h"
#include "tools.h"

uint64_t hash_uint64(uint64_t x) {
    x *= 0xff51afd7ed558ccdul;
    x ^= x >> 32;
    return x;
}

uint64_t hash_pointer(void *ptr) {
    return hash_uint64((uintptr_t) ptr);
}

uint64_t hash_mix(uint64_t x, uint64_t y) {
    x ^= y;
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;
    return x;
}

uint64_t hash_bytes(const void *ptr, size_t len) {
    uint64_t x = 0xcbf29ce484222325;
    const char *buf = (const char*)ptr;
    for (size_t i = 0; i < len; i++) {
        x ^= buf[i];
        x *= 0x100000001b3;
        x ^= x >> 32;
    }
    return x;
}

void map_grow(Map *map, size_t new_cap) {
    new_cap = MAX(16, new_cap);
    Map new_map = {
        .keys = calloc(new_cap, sizeof(void*)),
        .vals = malloc(new_cap * sizeof(void*)),
        .cap = new_cap
    };
    for (size_t i = 0; i < map->cap; i++) {
        if (map->keys[i]) {
            map_put(&new_map, map->keys[i], map->vals[i]);
        }
    }
    free(map->keys);
    free(map->vals);
    *map = new_map;
}

void map_put(Map *map, void *key, void *val) {
    assert(key && val);
    if (2 * map->len >= map->cap) {
        map_grow(map, 2 * map->cap);
    }
    size_t i = (size_t) hash_pointer(key);
    for (;;) {
        i &= map->cap - 1;
        if (!map->keys[i]) {
            map->len++;
            map->keys[i] = key;
            map->vals[i] = val;
            return;
        } else if (map->keys[i] == key) {
            map->vals[i] = val;
            return;
        }
        i++;
    }
}

void* map_get(Map *map, void *key) {
    if (map->len <= 0) {
        return NULL;
    }
    size_t i = (size_t) hash_pointer(key);
    for (;;) {
        i &= map->cap - 1;
        if (map->keys[i] == key) {
            return map->vals[i];
        } else if (!map->keys[i]) {
            return NULL;
        }
        i++;
    }
}