-module(arizona_markdown_processor).
-moduledoc ~"""
Markdown processing utilities for Arizona templates.

Provides functionality to process markdown content with embedded Arizona template
syntax while preserving dynamic expressions and Erlang comments during markdown
conversion.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([process_markdown_template/2]).

%% --------------------------------------------------------------------
%% Utility function exports (for parse transform and testing)
%% --------------------------------------------------------------------

-export([protect_dynamic_tokens/1]).
-export([restore_dynamic_tokens/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([protect_dynamic_tokens/1]).
-ignore_xref([restore_dynamic_tokens/2]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-doc ~"""
Processes markdown content with Arizona template syntax into HTML.

Takes markdown text containing Arizona template expressions and converts it
to HTML while preserving the template syntax for later processing.

## Parameters

- `Markdown` - Markdown text with embedded Arizona template syntax
- `Line` - Line number for error reporting

## Returns

HTML string with preserved Arizona template syntax

## Processing Steps

1. Scans markdown into Arizona tokens (static/dynamic/comment)
2. Protects dynamic expressions and comments with unique placeholders
3. Processes protected content through GitHub Flavored Markdown parser
4. Restores original Arizona syntax in the generated HTML

## Example

```erlang
HTML = arizona_markdown_processor:process_markdown_template(
    ~"# Hello {Name}!\n\nWelcome to **Arizona**.",
    1
).
% Returns: ~"<h1>Hello {Name}!</h1>\n<p>Welcome to <strong>Arizona</strong>.</p>\n"
```
""".
-spec process_markdown_template(Markdown, Line) -> HTML when
    Markdown :: iodata(),
    Line :: arizona_token:line(),
    HTML :: binary().
process_markdown_template(Markdown, Line) ->
    % Scan markdown content into tokens
    Tokens = arizona_scanner:scan_string(Line, Markdown),

    % Protect dynamic tokens from markdown processing
    {ProtectedMarkdown, TokenMap} = protect_dynamic_tokens(Tokens),

    % Convert protected markdown to HTML with unsafe option to allow raw HTML
    {ok, HTML} = arizona_markdown:to_html(ProtectedMarkdown, [unsafe]),

    % Restore dynamic tokens in HTML
    restore_dynamic_tokens(HTML, TokenMap).

%% --------------------------------------------------------------------
%% Utility functions
%% --------------------------------------------------------------------

-doc ~"""
Protects dynamic and comment tokens by replacing them with unique placeholders.

Used internally by the markdown processor and exported for use by the parse
transform during compile-time processing.

Returns a tuple containing the protected markdown content and a token map
that preserves the original token information including line numbers.
""".
%% Protects dynamic and comment tokens by replacing them with unique placeholders.
%% Returns {ProtectedMarkdown, TokenMap} where TokenMap maps IDs to original tokens.
-spec protect_dynamic_tokens(Tokens) -> {ProtectedMarkdown, TokenMap} when
    Tokens :: [arizona_token:token()],
    ProtectedMarkdown :: binary(),
    TokenMap :: #{binary() => arizona_token:token()}.
protect_dynamic_tokens(Tokens) ->
    protect_dynamic_tokens(Tokens, [], #{}, 0).

protect_dynamic_tokens([], Acc, TokenMap, _Counter) ->
    {iolist_to_binary(lists:reverse(Acc)), TokenMap};
protect_dynamic_tokens([Token | Rest], Acc, TokenMap, Counter) ->
    case arizona_token:get_category(Token) of
        dynamic ->
            % Generate unique ID for this dynamic token
            ID = integer_to_binary(Counter),

            % Create protected placeholder using unique text that won't be modified by markdown
            Placeholder = [~"ARIZONA_DYNAMIC_TOKEN_", ID, ~"_END"],

            % Store original token in map (preserves line info)
            NewTokenMap = TokenMap#{ID => Token},

            protect_dynamic_tokens(Rest, [Placeholder | Acc], NewTokenMap, Counter + 1);
        comment ->
            % Skip comments so they don't generate <p>{% Comment }</p>
            protect_dynamic_tokens(Rest, Acc, TokenMap, Counter);
        static ->
            % Static tokens pass through, but restore escaped braces
            Content = arizona_token:get_content(Token),
            % Restore \{ from { in static content (scanner converted \{ to {)
            RestoredContent = binary:replace(Content, ~"\{", ~"\\{", [global]),
            protect_dynamic_tokens(Rest, [RestoredContent | Acc], TokenMap, Counter)
    end.

-doc ~"""
Restores dynamic and comment tokens by replacing unique placeholders with original content.

Used internally by the markdown processor and exported for use by the parse
transform during compile-time processing.

Takes the processed HTML with placeholders and the token map to restore the
original Arizona template syntax.
""".
%% Restores dynamic and comment tokens by replacing unique placeholders with original content.
-spec restore_dynamic_tokens(HTML, TokenMap) -> RestoredHTML when
    HTML :: binary(),
    TokenMap :: #{binary() => arizona_token:token()},
    RestoredHTML :: binary().
restore_dynamic_tokens(HTML, TokenMap) ->
    maps:fold(
        fun(ID, Token, CurrentHTML) ->
            Placeholder = [~"ARIZONA_DYNAMIC_TOKEN_", ID, ~"_END"],
            Content = arizona_token:get_content(Token),
            OriginalContent = [~"{", Content, ~"}"],
            binary:replace(
                CurrentHTML,
                iolist_to_binary(Placeholder),
                iolist_to_binary(OriginalContent)
            )
        end,
        HTML,
        TokenMap
    ).
