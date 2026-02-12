-module(arizona_markdown_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_conversion},
        {group, options_support},
        {group, complex_markdown}
    ].

groups() ->
    [
        {basic_conversion, [parallel], [
            to_html_simple_header,
            to_html_simple_paragraph,
            to_html_emphasis,
            to_html_strong,
            to_html_empty_input,
            to_html_string_input,
            to_html_iolist_input,
            to_html_mixed_iodata
        ]},
        {options_support, [parallel], [
            to_html_with_unsafe
        ]},
        {complex_markdown, [parallel], [
            to_html_code_blocks,
            to_html_lists,
            to_html_links,
            to_html_blockquotes,
            to_html_nested_elements,
            to_html_tables,
            to_html_mixed_content
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic conversion tests
%% --------------------------------------------------------------------

to_html_simple_header(Config) when is_list(Config) ->
    ct:comment("Simple header conversion"),
    Input = ~"# Hello World",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<h1>Hello World</h1>",
    ?assertEqual(Expected, Html).

to_html_simple_paragraph(Config) when is_list(Config) ->
    ct:comment("Simple paragraph conversion"),
    Input = ~"This is a paragraph.",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<p>This is a paragraph.</p>",
    ?assertEqual(Expected, Html).

to_html_emphasis(Config) when is_list(Config) ->
    ct:comment("Emphasis conversion"),
    Input = ~"*italic text*",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<p><em>italic text</em></p>",
    ?assertEqual(Expected, Html).

to_html_strong(Config) when is_list(Config) ->
    ct:comment("Strong emphasis conversion"),
    Input = ~"**bold text**",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<p><strong>bold text</strong></p>",
    ?assertEqual(Expected, Html).

to_html_empty_input(Config) when is_list(Config) ->
    ct:comment("Empty input should return empty result"),
    Input = ~"",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"",
    ?assertEqual(Expected, Html).

to_html_string_input(Config) when is_list(Config) ->
    ct:comment("String input should be converted correctly"),
    Input = "# Hello *World*",
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<h1>Hello <em>World</em></h1>",
    ?assertEqual(Expected, Html).

to_html_iolist_input(Config) when is_list(Config) ->
    ct:comment("Iolist input should be converted correctly"),
    Input = [~"# ", "Hello", " ", ~"**World**"],
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<h1>Hello <strong>World</strong></h1>",
    ?assertEqual(Expected, Html).

to_html_mixed_iodata(Config) when is_list(Config) ->
    ct:comment("Complex nested iodata should work"),
    Input = [~"# ", ["Hello", [" *", ~"nested", "*"]], " ", <<"**text**">>],
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"<h1>Hello <em>nested</em> <strong>text</strong></h1>",
    ?assertEqual(Expected, Html).

%% --------------------------------------------------------------------
%% Options support tests
%% --------------------------------------------------------------------

to_html_with_unsafe(Config) when is_list(Config) ->
    ct:comment("Unsafe option should allow raw HTML"),
    Input = ~"<script>alert('test')</script>",
    {ok, Html} = arizona_markdown:to_html(Input, [unsafe]),
    Expected = ~"<script>alert('test')</script>",
    ?assertEqual(Expected, Html).

%% --------------------------------------------------------------------
%% Complex markdown tests
%% --------------------------------------------------------------------

to_html_code_blocks(Config) when is_list(Config) ->
    ct:comment("Code blocks should be properly converted"),
    Input = ~"""
    ```erlang
    hello() -> world.
    ```
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <pre><code class="language-erlang">hello() -&gt; world.
    </code></pre>
    """,
    ?assertEqual(Expected, Html).

to_html_lists(Config) when is_list(Config) ->
    ct:comment("Lists should be properly converted including nested lists"),
    Input = ~"""
    - Unordered item 1
    - Unordered item 2
      - Nested item 2.1
      - Nested item 2.2
    - Item 3

    1. Ordered item 1
    2. Ordered item 2
    3. Item with **bold** text
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <ul>
    <li>Unordered item 1</li>
    <li>Unordered item 2
    <ul>
    <li>Nested item 2.1</li>
    <li>Nested item 2.2</li>
    </ul>
    </li>
    <li>Item 3</li>
    </ul>
    <ol>
    <li>Ordered item 1</li>
    <li>Ordered item 2</li>
    <li>Item with <strong>bold</strong> text</li>
    </ol>
    """,
    ?assertEqual(Expected, Html).

to_html_links(Config) when is_list(Config) ->
    ct:comment("Links and images should be properly converted"),
    Input = ~"""
    [Simple link](https://example.com)
    [Link with title](https://example.com "Example Title")
    ![Image alt text](https://example.com/image.jpg)
    ![Image with title](https://example.com/image.jpg "Image Title")

    <https://autolink.com>
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <p><a href="https://example.com">Simple link</a>
    <a href="https://example.com" title="Example Title">Link with title</a>
    <img src="https://example.com/image.jpg" alt="Image alt text" />
    <img src="https://example.com/image.jpg" alt="Image with title" title="Image Title" /></p>
    <p><a href="https://autolink.com">https://autolink.com</a></p>
    """,
    ?assertEqual(Expected, Html).

to_html_blockquotes(Config) when is_list(Config) ->
    ct:comment("Blockquotes should be properly converted including nested ones"),
    Input = ~"""
    > This is a simple blockquote.
    >
    > It can span multiple lines.

    > ## Blockquote with header
    >
    > - And lists
    > - Inside it
    >
    > > Nested blockquote
    > > with multiple lines
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <blockquote>
    <p>This is a simple blockquote.</p>
    <p>It can span multiple lines.</p>
    </blockquote>
    <blockquote>
    <h2>Blockquote with header</h2>
    <ul>
    <li>And lists</li>
    <li>Inside it</li>
    </ul>
    <blockquote>
    <p>Nested blockquote
    with multiple lines</p>
    </blockquote>
    </blockquote>
    """,
    ?assertEqual(Expected, Html).

to_html_nested_elements(Config) when is_list(Config) ->
    ct:comment("Complex nested markdown structures"),
    Input = ~"""
    # Main Header

    ## Subsection with *emphasis* and **bold**

    Here's a paragraph with `inline code` and
    [a link](https://example.com).

    > Blockquote containing:
    >
    > 1. An ordered list
    > 2. With **bold** and *italic* text
    > 3. And `inline code`
    >
    > ```erlang
    > % Code block inside blockquote
    > hello() -> world.
    > ```

    ### Another Section

    - Unordered list
      - With nested items
      - And [nested links](https://nested.com)
    - Back to top level
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <h1>Main Header</h1>
    <h2>Subsection with <em>emphasis</em> and <strong>bold</strong></h2>
    <p>Here's a paragraph with <code>inline code</code> and
    <a href="https://example.com">a link</a>.</p>
    <blockquote>
    <p>Blockquote containing:</p>
    <ol>
    <li>An ordered list</li>
    <li>With <strong>bold</strong> and <em>italic</em> text</li>
    <li>And <code>inline code</code></li>
    </ol>
    <pre><code class="language-erlang">% Code block inside blockquote
    hello() -&gt; world.
    </code></pre>
    </blockquote>
    <h3>Another Section</h3>
    <ul>
    <li>Unordered list
    <ul>
    <li>With nested items</li>
    <li>And <a href="https://nested.com">nested links</a></li>
    </ul>
    </li>
    <li>Back to top level</li>
    </ul>
    """,
    ?assertEqual(Expected, Html).

to_html_tables(Config) when is_list(Config) ->
    ct:comment("Tables are properly rendered as HTML table elements with GFM support"),
    Input = ~"""
    | Header 1 | Header 2 | Header 3 |
    |----------|----------|----------|
    | Cell 1   | Cell 2   | Cell 3   |
    | **Bold** | *Italic* | `Code`   |
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <table>
    <thead>
    <tr>
    <th>Header 1</th>
    <th>Header 2</th>
    <th>Header 3</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td>Cell 1</td>
    <td>Cell 2</td>
    <td>Cell 3</td>
    </tr>
    <tr>
    <td><strong>Bold</strong></td>
    <td><em>Italic</em></td>
    <td><code>Code</code></td>
    </tr>
    </tbody>
    </table>
    """,
    ?assertEqual(Expected, Html).

to_html_mixed_content(Config) when is_list(Config) ->
    ct:comment("Complex real-world markdown document"),
    Input = ~"""
    # Arizona Framework Documentation

    Welcome to **Arizona**, a *modern* web framework for Erlang!

    ## Features

    - Real-time WebSocket updates
    - Compile-time template optimization
    - Hierarchical component rendering
    - Simple template syntax using `{}` expressions

    ### Code Example

    Here's how to create a basic view:

    ```erlang
    -module(hello_view).
    -behaviour(arizona_view).

    mount(_Arg, _Request) ->
        arizona_view:new(?MODULE, #{message => "Hello World!"}).
    ```

    ### Links and Resources

    - [GitHub Repository](https://github.com/arizona-framework/arizona)
    - [Documentation](https://docs.arizona.com)

    > **Note**: Arizona is currently in active development.
    >
    > Please report issues on our GitHub page.

    #### Installation

    1. Add to your `rebar.config`
    2. Run `rebar3 compile`
    3. Start building amazing apps!

    ---

    Happy coding! 🚀
    """,
    {ok, Html} = arizona_markdown:to_html(Input),
    Expected = ~"""
    <h1>Arizona Framework Documentation</h1>
    <p>Welcome to <strong>Arizona</strong>, a <em>modern</em> web framework for Erlang!</p>
    <h2>Features</h2>
    <ul>
    <li>Real-time WebSocket updates</li>
    <li>Compile-time template optimization</li>
    <li>Hierarchical component rendering</li>
    <li>Simple template syntax using <code>{}</code> expressions</li>
    </ul>
    <h3>Code Example</h3>
    <p>Here's how to create a basic view:</p>
    <pre><code class="language-erlang">-module(hello_view).
    -behaviour(arizona_view).

    mount(_Arg, _Request) -&gt;
        arizona_view:new(?MODULE, #{message =&gt; &quot;Hello World!&quot;}).
    </code></pre>
    <h3>Links and Resources</h3>
    <ul>
    <li><a href="https://github.com/arizona-framework/arizona">GitHub Repository</a></li>
    <li><a href="https://docs.arizona.com">Documentation</a></li>
    </ul>
    <blockquote>
    <p><strong>Note</strong>: Arizona is currently in active development.</p>
    <p>Please report issues on our GitHub page.</p>
    </blockquote>
    <h4>Installation</h4>
    <ol>
    <li>Add to your <code>rebar.config</code></li>
    <li>Run <code>rebar3 compile</code></li>
    <li>Start building amazing apps!</li>
    </ol>
    <hr />
    <p>Happy coding! 🚀</p>
    """,
    ?assertEqual(Expected, Html).
