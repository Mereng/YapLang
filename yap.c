const char* compile_str(const char *code) {
    init_stream(NULL, code);
    init_entities();
    entities_append_declaration_list(parse_file());
    complete_entities();
    generate_c_code();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    return c_code;
}

bool compile_file(const char *path) {
    char *code = read_file(path);
    if (!code) {
        return false;
    }
    init_stream(path, code);
    init_entities();
    entities_append_declaration_list(parse_file());
    complete_entities();
    generate_c_code();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    const char *c_file = replace_ext(path, "c");
    if (!c_file) {
        return false;
    }
    if (!write_file(c_file, c_code, buf_len(c_code))) {
        return false;
    }
    return true;
}

int yap_main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <source file>\n", argv[0]);
        return 1;
    }
    keywords_init();
    if (!compile_file(argv[1])) {
        printf("Failed\n");
        return 2;
    }
    printf("Succeeded\n");
}