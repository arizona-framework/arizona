-module(arizona_markdown_processor_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, markdown_processing_tests}
    ].

groups() ->
    [
        {markdown_processing_tests, [parallel], [
            process_simple_markdown_test,
            process_markdown_with_dynamic_test,
            process_markdown_with_escaped_braces_test,
            process_markdown_with_mixed_content_test,
            process_markdown_with_comments_test,
            protect_dynamic_tokens_test,
            restore_dynamic_tokens_test
        ]}
    ].

%% --------------------------------------------------------------------
%% Markdown processing tests
%% --------------------------------------------------------------------

process_simple_markdown_test(Config) when is_list(Config) ->
    ct:comment("process_markdown_template/2 should handle simple markdown"),
    Markdown = ~"# Hello World\n\nThis is **bold** text.",
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, 1),
    Expected = ~"<h1>Hello World</h1>\n<p>This is <strong>bold</strong> text.</p>",
    ?assertEqual(Expected, HTML).

process_markdown_with_dynamic_test(Config) when is_list(Config) ->
    ct:comment("process_markdown_template/2 should preserve dynamic expressions"),
    Markdown = ~"# {~\"Test Title\"}\n\nContent here.",
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, 1),
    Expected = ~"<h1>{~\"Test Title\"}</h1>\n<p>Content here.</p>",
    ?assertEqual(Expected, HTML).

process_markdown_with_escaped_braces_test(Config) when is_list(Config) ->
    ct:comment("process_markdown_template/2 should handle escaped braces correctly"),
    Markdown = ~"""
    <button onclick="arizona.pushEvent('test', \{id: {42}});">Click</button>
    """,
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, 1),
    Expected = ~"""
    <p><button onclick="arizona.pushEvent('test', \{id: {42}});">Click</button></p>
    """,
    ?assertEqual(Expected, HTML).

process_markdown_with_mixed_content_test(Config) when is_list(Config) ->
    ct:comment("process_markdown_template/2 should handle complex mixed content"),
    Markdown = ~"""
    # Title {~"Dynamic"}

    Some **markdown** with `code` and {42}.

    <button onclick="arizona.pushEvent('click', \{value: {~"test"}});">
        Click Me
    </button>

    - List item
    - Another {~"item"}
    """,
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, 1),

    Expected = ~"""
    <h1>Title {~"Dynamic"}</h1>
    <p>Some <strong>markdown</strong> with <code>code</code> and {42}.</p>
    <button onclick="arizona.pushEvent('click', \{value: {~"test"}});">
        Click Me
    </button>
    <ul>
    <li>List item</li>
    <li>Another {~"item"}</li>
    </ul>
    """,
    ?assertEqual(Expected, HTML).

process_markdown_with_comments_test(Config) when is_list(Config) ->
    ct:comment("process_markdown_template/2 should preserve Erlang comments"),
    Markdown = ~"""
    # Title
    {% This is an Erlang comment}
    {% This is an Erlang
     % multiline comment}
    Hello {~"World"}!
    """,
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, 1),

    Expected = ~"""
    <h1>Title</h1>
    <p>Hello {~"World"}!</p>
    """,
    ?assertEqual(Expected, HTML).

protect_dynamic_tokens_test(Config) when is_list(Config) ->
    ct:comment("protect_dynamic_tokens/1 should replace dynamic tokens with placeholders"),
    Tokens = arizona_scanner:scan_string(1, ~"Hello {name} and {age}!"),
    {ProtectedMarkdown, TokenMap} = arizona_markdown_processor:protect_dynamic_tokens(Tokens),

    Expected = ~"Hello ARIZONA_DYNAMIC_TOKEN_0_END and ARIZONA_DYNAMIC_TOKEN_1_END!",
    ?assertEqual(Expected, ProtectedMarkdown),

    % Token map should have 2 entries
    ?assertEqual(2, maps:size(TokenMap)),

    % Verify tokens are preserved correctly
    ?assert(maps:is_key(~"0", TokenMap)),
    ?assert(maps:is_key(~"1", TokenMap)),

    Token0 = maps:get(~"0", TokenMap),
    Token1 = maps:get(~"1", TokenMap),
    ?assertEqual(dynamic, arizona_token:get_category(Token0)),
    ?assertEqual(dynamic, arizona_token:get_category(Token1)),
    ?assertEqual(~"name", arizona_token:get_content(Token0)),
    ?assertEqual(~"age", arizona_token:get_content(Token1)).

restore_dynamic_tokens_test(Config) when is_list(Config) ->
    ct:comment("restore_dynamic_tokens/2 should restore original content"),
    % Create some test tokens
    Token0 = arizona_token:new(dynamic, 1, ~"name"),
    Token1 = arizona_token:new(dynamic, 1, ~"age"),
    TokenMap = #{~"0" => Token0, ~"1" => Token1},

    % Create HTML with placeholders
    HTML = ~"<p>Hello ARIZONA_DYNAMIC_TOKEN_0_END and ARIZONA_DYNAMIC_TOKEN_1_END!</p>",

    % Restore tokens
    RestoredHTML = arizona_markdown_processor:restore_dynamic_tokens(HTML, TokenMap),

    % Should have original content back
    Expected = ~"<p>Hello {name} and {age}!</p>",
    ?assertEqual(Expected, RestoredHTML).
