#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <limits.h>
#include <math.h>
#include <inttypes.h>

#include "tools.h"
#include "buf.h"
#include "yap.c"
#include "lexer.c"
#include "ast.h"

int main() {
    buf_test();
    lex_test();
    str_intern_test();
    ast_test();
    return 0;
}