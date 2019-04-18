#ifndef YAP_OS_H
#define YAP_OS_H

#include <limits.h>
#include <stdbool.h>
#include <stddef.h>

#ifndef PATH_MAX
#define PATH_MAX _MAX_PATH
#endif

typedef struct DirectoryIterator {
    char base[PATH_MAX];
    char name[PATH_MAX];
    size_t size;
    bool is_dir;
    bool is_valid;
    bool is_error;
    void *__handle;
} DirectoryIterator;

DirectoryIterator* dir_new(const char *path);
void dir_free(DirectoryIterator *it);
void dir_next(DirectoryIterator *it);

void path_normalize(char *path);
bool path_copy(char path[PATH_MAX], const char *src);
bool dir_is_excluded(DirectoryIterator *it);

#endif