#include <memory.h>
#include <string.h>

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

bool dir_is_excluded(DirectoryIterator *it) {
    return it->is_valid && (strcmp(it->name, ".") == 0 || strcmp(it->name, "..") == 0);
}