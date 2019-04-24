#include <io.h>
#include <stdlib.h>
#include <errno.h>

#include "os.h"

void dir_free(DirectoryIterator *it) {
    if (it->is_valid) {
        _findclose((intptr_t)it->__handle);
        it->is_valid = false;
        it->is_error = false;
    }
}

void dir__update(DirectoryIterator *it, bool is_done, struct _finddata_t *fileinfo) {
    it->is_valid = !is_done;
    it->is_error = is_done && errno != ENOENT;
    if (!is_done) {
        it->size = fileinfo->size;
        memcpy(it->name, fileinfo->name, sizeof(it->name) - 1);
        it->name[PATH_MAX - 1] = 0;
        it->is_dir = fileinfo->attrib & _A_SUBDIR;
    }
}

void dir_next(DirectoryIterator *it) {
    if (!it->is_valid) {
        return;;
    }

    do {
        struct _finddata_t fileinfo;
        int res = _findnext((intptr_t)it->__handle, &fileinfo);
        dir__update(it, res != 0, &fileinfo);
        if (res != 0) {
            dir_free(it);
            return;
        }
    } while (dir_is_excluded(it));
}

void dir(DirectoryIterator *it, const char *path) {
    memset(it, 0, sizeof(*it));
    path_copy(it->base, path);
    char file[PATH_MAX];
    path_copy(file, path);
    path_join(file, "*");
    struct _finddata_t fileinfo;
    intptr_t handle = _findfirst(file, &fileinfo);
    it->__handle = (void*)handle;
    dir__update(it, handle == -1, &fileinfo);
    if (dir_is_excluded(it)) {
        dir_next(it);
    }
}

void path_absolute(char path[PATH_MAX]) {
    char rel_path[PATH_MAX];
    path_copy(rel_path, path);
    _fullpath(path, rel_path, PATH_MAX);
}