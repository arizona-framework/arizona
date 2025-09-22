-module(arizona_markdown).
-moduledoc ~"""
CommonMark markdown parsing for Arizona templates.

Provides fast, standards-compliant markdown to HTML conversion using the
cmark C library. Designed to integrate seamlessly with Arizona's template
system while preserving dynamic expressions.

## Supported Options

- `source_pos` - Include `data-sourcepos` attributes on block elements
- `hard_breaks` - Render softbreak elements as hard line breaks
- `unsafe` - Allow raw HTML and unsafe links (disabled by default for security)
- `smart` - Convert straight quotes to curly, `---` to em dashes, `--` to en dashes

## Example

```erlang
% Binary input
1> arizona_markdown:to_html(~"# Hello *world*").
{ok, ~"<h1>Hello <em>world</em></h1>\n"}

% String input
2> arizona_markdown:to_html("# Hello **world**").
{ok, ~"<h1>Hello <strong>world</strong></h1>\n"}

% Iolist input
3> arizona_markdown:to_html([~"# ", "Hello", [" *", ~"world", "*"]]).
{ok, ~"<h1>Hello <em>world</em></h1>\n"}

% With options
4> arizona_markdown:to_html(~"# Hello", [smart, source_pos]).
{ok, ~"<h1 data-sourcepos=\"1:1-1:7\">Hello</h1>\n"}
```

## Error Handling

Returns `{error, Reason}` for:
- `parse_failed` - Markdown parsing failed
- `invalid_utf8` - Input contains invalid UTF-8 sequences
- `nomem` - Memory allocation failed
- `badarg` - Invalid arguments provided
""".

%% --------------------------------------------------------------------
%% NIF declarations
%% --------------------------------------------------------------------

-nifs([to_html/1]).
-nifs([to_html/2]).

-on_load(init/0).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/1]).
-export([to_html/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([to_html/1]).
-ignore_xref([to_html/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([markdown/0]).
-export_type([option/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal markdown() :: iodata().
-nominal option() :: source_pos | hard_breaks | unsafe | smart.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-doc ~"""
Convert markdown to HTML using default options.
""".
-spec to_html(Markdown) -> Result when
    Markdown :: markdown(),
    Result :: {ok, HTML} | {error, ErrReason},
    HTML :: arizona_html:html(),
    ErrReason ::
        parse_failed
        | invalid_utf8
        | nomem
        | badarg.
to_html(Markdown) when is_binary(Markdown); is_list(Markdown) ->
    erlang:nif_error(nif_library_not_loaded).

-doc ~"""
Convert markdown to HTML with specified options.
""".
-spec to_html(Markdown, Options) -> Result when
    Markdown :: markdown(),
    Options :: [option()],
    Result :: {ok, HTML} | {error, ErrReason},
    HTML :: arizona_html:html(),
    ErrReason ::
        parse_failed
        | invalid_utf8
        | nomem
        | badarg.
to_html(Markdown, Options) when
    (is_binary(Markdown) orelse is_list(Markdown)),
    is_list(Options)
->
    erlang:nif_error(nif_library_not_loaded).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

init() ->
    PrivDir = code:priv_dir(arizona),
    SoName = filename:join(PrivDir, "arizona_markdown"),
    erlang:load_nif(SoName, 0).
