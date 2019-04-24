void search_paths_init(const char *path_to_bin) {
    const char *yaproot = getenv("YAPROOT");
    char root_path[PATH_MAX];
    if (yaproot) {
        path_copy(root_path, yaproot);
    } else {
        path_copy(root_path, path_to_bin);
        path_absolute(root_path),
        path_normalize(root_path);
        path_dir(root_path);
    }
    packages_search_paths_add(root_path);

    const char *yappath_var = getenv("YAPPATH");
    char yap_path[PATH_MAX];
    if (yappath_var) {
        path_copy(yap_path, yappath_var);
        packages_search_paths_add(yap_path);
    }

    packages_search_paths_add(".");
}

void init(const char *path_to_bin) {
    search_paths_init(path_to_bin);
    keywords_init();
}

int yap_main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <package> [<output-c-file>]\n", argv[0]);
        return 1;
    }
    const char *package = argv[1];
    init(argv[0]);

    builtin_package = calloc(1, sizeof(Package));

    init_builtin_entities();

    Package *main_package = import_package(package);
    if (!main_package) {
        printf("error: Failed to compile package '%s'\n", package);
        return 1;
    }
    const char *main = str_intern("main");
    Entity *main_entity = get_package_entity(main_package, main);
    if (!main_entity) {
        printf("error: No main entry point defined in package '%s'\n", package);
        return 1;
    }
    main_entity->external_name = main;
    resolve_package_entities(builtin_package);
    resolve_package_entities(main_package);
    complete_reachable_entities();
    char c_path[PATH_MAX];
    if (argc >= 3) {
        path_copy(c_path, argv[2]);
    } else {
        snprintf(c_path, sizeof(c_path), "out_%s.c", package);
    }

    generate_c_code();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    if (!write_file(c_path, c_code, buf_len(c_code))) {
        printf("error: Failed to write file '%s'\n", c_path);
        return 1;
    }

    printf("Succeeded");
    return 0;
}