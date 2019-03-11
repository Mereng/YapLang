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
#include "aalloc.h"
#include "ast.h"
#include "general.c"
#include "lexer.c"
#include "parser.c"
#include "resolver.c"
#include "generator.c"
#include "yap.c"
#include "fortests.c"


int main(int argc, char **argv) {
//    keywords_init();
//    buf_test();
//    keywords_test();
//    lex_test();
//    str_intern_test();
//    ast_test();
//    parser_test();
//    resolver_test();
//    gen_test();
    return yap_main(argc, argv);
}
