# Editor Support

- [arizona.nvim](#arizonanvim)
- [tree-sitter-arizona](#tree-sitter-arizona)

## arizona.nvim

A Neovim plugin for Arizona development. It provides syntax highlighting for Arizona template
expressions within Erlang files, integration with the Arizona development server, and convenient
commands for common development tasks.

Features include:

- Syntax highlighting for `{}` template expressions embedded in Erlang string literals
- Recognition of Arizona-specific macros and function calls
- Integration with the file watcher for editor-triggered reloads

Install via your preferred Neovim plugin manager. For example, with `lazy.nvim`:

```lua
{
    "arizona-framework/arizona.nvim",
    ft = { "erlang" },
    config = true,
}
```

The plugin depends on `tree-sitter-arizona` for accurate syntax parsing. Ensure the Tree-sitter
grammar is installed (see below) for the best experience.

## tree-sitter-arizona

A Tree-sitter grammar for Arizona's template syntax. It enables accurate syntax highlighting of `{}`
template expressions inside HTML, Erlang terms, and Markdown templates. The grammar understands
Arizona's binding syntax, event attributes, and conditional expressions within templates.

The grammar is used by `arizona.nvim` and can be integrated with any editor that supports
Tree-sitter grammars, including:

- **Neovim** -- via `nvim-treesitter` (installed automatically by `arizona.nvim`)
- **Helix** -- add the grammar to your `languages.toml` configuration
- **Zed** -- add as a Tree-sitter grammar extension

To install the grammar manually with `nvim-treesitter`:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.arizona = {
    install_info = {
        url = "https://github.com/arizona-framework/tree-sitter-arizona",
        files = { "src/parser.c" },
    },
    filetype = "erlang",
}
```

After installation, Arizona template expressions will be highlighted with distinct colors from the
surrounding Erlang or HTML code, making it easier to distinguish between static markup and dynamic
bindings at a glance.

See also: [Templates Overview](../templates/overview.md)
