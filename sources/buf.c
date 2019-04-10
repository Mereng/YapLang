#include <buf.h>
#include <assert.h>
#include <malloc.h>
#include <stdarg.h>
#include <stdio.h>

#include "tools.h"
void* buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    size_t new_cap = MAX(16, MAX(1 + 2 * buf_cap(buf), new_len));
    assert(new_len <= new_cap);
    size_t new_size = offsetof(BufHdr, buf) + new_cap * elem_size;
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = realloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = malloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;

    return new_hdr->buf;
}

char* buf__printf(char *buf, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t cap = buf_cap(buf) - buf_len(buf);
    int n = vsnprintf(buf_end(buf), cap, fmt, args) + 1;
    va_end(args);
    if (n > cap) {
        buf__fit(buf, n + buf_len(buf));
        va_start(args, fmt);
        cap = buf_cap(buf) - buf_len(buf);
        n = vsnprintf(buf_end(buf), cap, fmt, args) + 1;
        assert(n <= cap);
        va_end(args);
    }
    buf__hdr(buf)->len += n - 1;
    return buf;
}
