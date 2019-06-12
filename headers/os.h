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

void dir(DirectoryIterator *it, const char *path);
void dir_free(DirectoryIterator *it);
void dir_next(DirectoryIterator *it);
bool dir_is_excluded(DirectoryIterator *it);
bool dir_subdir(DirectoryIterator *it);

void path_normalize(char *path);
void path_copy(char path[PATH_MAX], const char *src);
void path_absolute(char path[PATH_MAX]);
void path_join(char path[PATH_MAX], const char *src);
char* path_filename(char path[PATH_MAX]);
char* path_ext(char path[PATH_MAX]);
void path_dir(char path[PATH_MAX]);
void get_path_executable(char dest[PATH_MAX]);

#endif