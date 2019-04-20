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
#include "map.h"
#include "os.h"
#include "general.c"
#include "lexer.c"
#include "parser.c"
#include "resolver.c"
#include "generator.c"
#include "yap.c"


int main(int argc, char **argv) {
    return yap_main(argc, argv);
}
