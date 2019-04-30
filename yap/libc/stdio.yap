#foreign(header = "<stdio.h>")

@foreign
struct FILE;

@foreign
var stdin: FILE*;

@foreign
var stdout: FILE*;

@foreign
var stderr: FILE*;

@foreign
struct fpos_t;

@foreign
typedef va_list = char*;

@foreign
const EOF = -1;
@foreign
const SEEK_SET = 0;
@foreign
const SEEK_CUR = 1;
@foreign
const SEEK_END = 2;

@foreign
func remove(filename: char const*) : int;

@foreign
func rename (oldname: char const*, newname: char const*) : int;

@foreign
func tmpfile() : FILE*;

@foreign
func tmpname(s: char*) : char;

@foreign
func fclose(stream: FILE*) : int;

@foreign
func fflush(stream: FILE*) : int;

@foreign
func fopen(filename: char const*, mode: char const*) : FILE*;

@foreign
func freopen(filename: char const*, mode: char const*, stream: FILE*) : FILE*;

@foreign
func printf(fmt: char const*, ...): int;