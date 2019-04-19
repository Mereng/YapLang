#include <dirent.h>
#include <malloc.h>
#include <stdlib.h>

#include "os.h"

void dir_free(DirectoryIterator *it) {
    if (it->is_valid) {
        it->is_valid = false;
        it->is_error = false;
        closedir(it->__handle);
    }
}

void dir_next(DirectoryIterator *it) {
    if (!it->is_valid) {
        return;
    }

    do {
        struct dirent *entry = readdir(it->__handle);
        if (!entry) {
            dir_free(it);
            return;;
        }
        path_copy(it->name, entry->d_name);
        it->is_dir = entry->d_type & DT_DIR;
    } while (dir_is_excluded(it));
}

DirectoryIterator* dir_new(const char *path) {
    DirectoryIterator *it = calloc(1, sizeof(DirectoryIterator));
    DIR *dir = opendir(path);
    if (!dir) {
        it->is_valid = false;
        it->is_error = true;
        return it;
    }
    it->__handle = dir;
    path_copy(it->base, path);
    it->is_valid = true;
    dir_next(it);
    return it;
}

void path_absolute(char path[PATH_MAX]) {
    char rel_path[PATH_MAX];
    path_copy(rel_path, path);
    realpath(rel_path, path);
}
