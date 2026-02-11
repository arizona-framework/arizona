-module(arizona_markdown).
-moduledoc ~"""
CommonMark markdown parsing for Arizona templates.

Provides standards-compliant GitHub Flavored Markdown (GFM) to HTML conversion
using the `erlang-markdown` library. Designed to integrate seamlessly with
Arizona's template system while preserving dynamic expressions.

## Supported Options

- `unsafe` - Allow raw HTML and unsafe links (disabled by default for security)

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
4> arizona_markdown:to_html(~"<div>hello</div>", [unsafe]).
{ok, ~"<div>hello</div>"}
```
""".

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
-nominal option() :: unsafe.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-doc ~"""
Convert markdown to HTML using GFM defaults.
""".
-spec to_html(Markdown) -> Result when
    Markdown :: markdown(),
    Result :: {ok, HTML},
    HTML :: arizona_html:html().
to_html(Markdown) when is_binary(Markdown); is_list(Markdown) ->
    markdown:to_html_with_options(iolist_to_binary(Markdown), gfm_options()).

-doc ~"""
Convert markdown to HTML with specified options.
""".
-spec to_html(Markdown, Options) -> Result when
    Markdown :: markdown(),
    Options :: [option()],
    Result :: {ok, HTML},
    HTML :: arizona_html:html().
to_html(Markdown, Options) when
    (is_binary(Markdown) orelse is_list(Markdown)),
    is_list(Options)
->
    markdown:to_html_with_options(iolist_to_binary(Markdown), build_options(Options)).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec gfm_options() -> markdown_options:t().
gfm_options() ->
    markdown_options:gfm().

-spec build_options(Options :: [option()]) -> markdown_options:t().
build_options([]) ->
    gfm_options();
build_options(Options) ->
    CompileOverrides = lists:flatmap(fun option_to_compile_opts/1, Options),
    markdown_options:gfm([{compile, CompileOverrides}]).

-spec option_to_compile_opts(option()) -> [{atom(), boolean()}].
option_to_compile_opts(unsafe) ->
    [{allow_dangerous_html, true}, {allow_dangerous_protocol, true}, {gfm_tagfilter, false}].
