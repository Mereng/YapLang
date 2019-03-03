#ifndef YAP_TOOLS_H
#define YAP_TOOLS_H

#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) (ALIGN_DOWN((n) + (a) - 1, (a)))
#define ALIGN_DOWN_PTR(p, a) ((void*)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void*)ALIGN_UP((uintptr_t)p, (a)))

#endif
