#include <memory.h>
#include <string.h>
#include <stdio.h>

#include "os.h"
#include "tools.h"

void path_normalize(char *path) {
    char *i;
    for (i = path; *i; i++) {
        if (*i == '\\') {
            *i = '/';
        }
    }

    if (i != path && i[-1] =='/') {
        i[-1] = 0;
    }
}

bool path_copy(char path[PATH_MAX], const char *src) {
    size_t src_len = strlen(src);
    size_t copy_len = MIN(src_len, PATH_MAX - 1);
    memcpy(path, src, copy_len);
    path[copy_len] = 0;
    path_normalize(path);
    return src_len < PATH_MAX;
}
void path_join(char path[PATH_MAX], const char *src) {
    char *end = path + strlen(path);
    if (end != path && end[-1] == '/') {
        end--;
    }
    if (*src == '/') {
        src++;
    }
    snprintf(end, path + PATH_MAX - end, "/%s", src);
}
char* path_filename(char path[PATH_MAX]) {
    path_normalize(path);
    for (char *i = path + strlen(path); i != path; i--) {
        if (i[-1] == '/') {
            return i;
        }
    }
    return path;
}
char* path_ext(char path[PATH_MAX]) {
    for (char *i = path + strlen(path); i != path; i--) {
        if (i[-1] == '.') {
            return i;
        }
    }
    return path;
}
void path_dir(char path[PATH_MAX]) {
    char *i;
    for (i = path + strlen(path); i != path && *i != '/'; i--) {
        *i = 0;
    }
    if (i != path && *i == '/') {
        *i = 0;
    }
}

bool dir_is_excluded(DirectoryIterator *it) {
    return it->is_valid && (strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0);
}

bool dir_subdir(DirectoryIterator *it) {
    if (!it->is_valid || !it->is_dir) {
        return false;
    }

    path_join(it->base, it->name);
    DirectoryIterator subdir;
    dir(&subdir, it->base);
    dir_free(it);
    *it = subdir;
    return true;
}