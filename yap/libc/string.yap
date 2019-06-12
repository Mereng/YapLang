#foreign (header="<string.h>")

@foreign func memset(s: void*,  c : int, n : usize) : void*;
@foreign func strlen(s: char const*) : usize;