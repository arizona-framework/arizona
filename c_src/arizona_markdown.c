#include "cmark-gfm/cmark-gfm-extension_api.h"
#include "cmark-gfm/cmark-gfm.h"
#include "extensions/cmark-gfm-core-extensions.h"

#include <erl_nif.h>
#include <string.h>

// Safety limits
#define MAX_INPUT_SIZE (16UL * 1024UL * 1024UL) // 16MB limit to prevent DoS
#define MIN_INPUT_SIZE 0

// Atom declarations
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_parse_failed;
static ERL_NIF_TERM atom_invalid_utf8;
static ERL_NIF_TERM atom_nomem;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_input_too_large;
static ERL_NIF_TERM atom_source_pos;
static ERL_NIF_TERM atom_hard_breaks;
static ERL_NIF_TERM atom_unsafe;
static ERL_NIF_TERM atom_smart;

// Global extension pointers (initialized once in load callback)
static cmark_syntax_extension *g_table_ext = NULL;
static cmark_syntax_extension *g_autolink_ext = NULL;
static cmark_syntax_extension *g_strikethrough_ext = NULL;
static cmark_syntax_extension *g_tagfilter_ext = NULL;
static cmark_syntax_extension *g_tasklist_ext = NULL;

// Helper function to convert options list to cmark flags
static int parse_options(ErlNifEnv *env, ERL_NIF_TERM options_list, int *flags) {
    ERL_NIF_TERM head, tail;
    *flags = CMARK_OPT_DEFAULT;

    if (enif_is_list(env, options_list)) {
        tail = options_list;
        while (enif_get_list_cell(env, tail, &head, &tail)) {
            if (enif_is_identical(head, atom_source_pos)) {
                *flags |= CMARK_OPT_SOURCEPOS;
            } else if (enif_is_identical(head, atom_hard_breaks)) {
                *flags |= CMARK_OPT_HARDBREAKS;
            } else if (enif_is_identical(head, atom_unsafe)) {
                *flags |= CMARK_OPT_UNSAFE;
            } else if (enif_is_identical(head, atom_smart)) {
                *flags |= CMARK_OPT_SMART;
            } else {
                return 0; // Invalid option
            }
        }
        return 1; // Success
    }
    return 0; // Not a list
}

// Helper function to create error tuple
static ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
    return enif_make_tuple2(env, atom_error, reason);
}

// Helper function to create ok tuple
static ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM value) {
    return enif_make_tuple2(env, atom_ok, value);
}

// Helper function to validate input binary
static ERL_NIF_TERM validate_input(ErlNifEnv *env, ERL_NIF_TERM markdown_term,
                                   ErlNifBinary *markdown_bin) {
    // Convert iodata to binary (supports binaries, strings, iolists)
    if (!enif_inspect_iolist_as_binary(env, markdown_term, markdown_bin)) {
        return make_error(env, atom_badarg);
    }

    // Check input size limits to prevent DoS attacks
    if (markdown_bin->size > MAX_INPUT_SIZE) {
        return make_error(env, atom_input_too_large);
    }

    // Validate input is not NULL (defensive programming)
    if (markdown_bin->data == NULL && markdown_bin->size > 0) {
        return make_error(env, atom_badarg);
    }

    return atom_ok; // Success indicator
}

// Helper function to setup parser with GFM extensions
static cmark_parser *setup_parser_with_extensions(int options) {
    cmark_parser *parser = cmark_parser_new(options);
    if (parser == NULL) {
        return NULL;
    }

    // Attach pre-registered GFM extensions (thread-safe, already initialized in load callback)
    if (g_table_ext) {
        cmark_parser_attach_syntax_extension(parser, g_table_ext);
    }
    if (g_autolink_ext) {
        cmark_parser_attach_syntax_extension(parser, g_autolink_ext);
    }
    if (g_strikethrough_ext) {
        cmark_parser_attach_syntax_extension(parser, g_strikethrough_ext);
    }
    if (g_tagfilter_ext) {
        cmark_parser_attach_syntax_extension(parser, g_tagfilter_ext);
    }
    if (g_tasklist_ext) {
        cmark_parser_attach_syntax_extension(parser, g_tasklist_ext);
    }

    return parser;
}

// Main conversion function
static ERL_NIF_TERM do_markdown_to_html(ErlNifEnv *env, ERL_NIF_TERM markdown_term, int options) {
    ErlNifBinary markdown_bin;
    char *html_result;
    ERL_NIF_TERM result_term;
    cmark_parser *parser;
    cmark_node *document;
    cmark_mem *mem = cmark_get_default_mem_allocator();

    // Validate input
    ERL_NIF_TERM validation_result = validate_input(env, markdown_term, &markdown_bin);
    if (!enif_is_identical(validation_result, atom_ok)) {
        return validation_result; // Return error from validation
    }

    // Create parser with GFM extensions
    parser = setup_parser_with_extensions(options);
    if (parser == NULL) {
        return make_error(env, atom_nomem);
    }

    // Parse markdown
    cmark_parser_feed(parser, (const char *)markdown_bin.data, markdown_bin.size);
    document = cmark_parser_finish(parser);
    cmark_parser_free(parser);

    if (document == NULL) {
        return make_error(env, atom_parse_failed);
    }

    // Convert to HTML
    html_result = cmark_render_html(document, options, NULL);
    cmark_node_free(document);

    if (html_result == NULL) {
        return make_error(env, atom_parse_failed);
    }

    // Create Erlang binary from result
    size_t html_len = strlen(html_result);
    unsigned char *result_data = enif_make_new_binary(env, html_len, &result_term);
    if (result_data == NULL) {
        mem->free(html_result);
        return make_error(env, atom_nomem);
    }

    // Copy HTML result to Erlang binary (no null termination needed for binary data)
    // NOLINTBEGIN(bugprone-not-null-terminated-result)
    memcpy(result_data, html_result, html_len);
    // NOLINTEND(bugprone-not-null-terminated-result)
    mem->free(html_result);

    return make_ok(env, result_term);
}

// NIF function: to_html/1
static ERL_NIF_TERM to_html_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    return do_markdown_to_html(env, argv[0], CMARK_OPT_DEFAULT);
}

// NIF function: to_html/2
static ERL_NIF_TERM to_html_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int options;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if (!parse_options(env, argv[1], &options)) {
        return make_error(env, atom_badarg);
    }

    return do_markdown_to_html(env, argv[0], options);
}

// Load callback - initialize atoms and register extensions once
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    (void)priv_data; // Unused parameter
    (void)load_info; // Unused parameter
    // Initialize atoms
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_parse_failed = enif_make_atom(env, "parse_failed");
    atom_invalid_utf8 = enif_make_atom(env, "invalid_utf8");
    atom_nomem = enif_make_atom(env, "nomem");
    atom_badarg = enif_make_atom(env, "badarg");
    atom_input_too_large = enif_make_atom(env, "input_too_large");
    atom_source_pos = enif_make_atom(env, "source_pos");
    atom_hard_breaks = enif_make_atom(env, "hard_breaks");
    atom_unsafe = enif_make_atom(env, "unsafe");
    atom_smart = enif_make_atom(env, "smart");

    // Register all GFM extensions once during load (thread-safe initialization)
    cmark_gfm_core_extensions_ensure_registered();

    // Cache extension pointers for reuse
    g_table_ext = cmark_find_syntax_extension("table");
    g_autolink_ext = cmark_find_syntax_extension("autolink");
    g_strikethrough_ext = cmark_find_syntax_extension("strikethrough");
    g_tagfilter_ext = cmark_find_syntax_extension("tagfilter");
    g_tasklist_ext = cmark_find_syntax_extension("tasklist");

    return 0; // Success
}

// Upgrade callback - called when NIF is reloaded (e.g., during testing)
static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
    (void)env;           // Unused parameter
    (void)priv_data;     // Unused parameter
    (void)old_priv_data; // Unused parameter
    (void)load_info;     // Unused parameter

    // For this NIF, no special upgrade handling is needed since we don't
    // maintain state between loads. The extension pointers are re-initialized
    // in the load callback.
    return 0; // Success
}

// NIF function exports
static ErlNifFunc nif_funcs[] = {{"to_html", 1, to_html_1, 0}, {"to_html", 2, to_html_2, 0}};

// NIF initialization
ERL_NIF_INIT(arizona_markdown, nif_funcs, load, NULL, upgrade, NULL)