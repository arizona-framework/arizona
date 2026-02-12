-module(arizona_template_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-elvis([{elvis_style, max_module_length, disable}]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, template_creation_tests},
        {group, template_accessor_tests},
        {group, template_binding_tests},
        {group, template_render_callback_tests},
        {group, template_render_options_tests}
    ].

groups() ->
    [
        {template_creation_tests, [parallel], [
            from_html_simple_test,
            from_html_with_dynamic_test,
            from_html_full_params_test,
            from_html_file_test,
            from_html_priv_file_test,
            from_markdown_simple_test,
            from_markdown_with_dynamic_test,
            from_markdown_mixed_content_test,
            from_markdown_with_comments_test,
            from_markdown_file_test,
            from_markdown_priv_file_test,
            is_template_test
        ]},
        {template_accessor_tests, [parallel], [
            get_static_test,
            get_dynamic_test,
            get_dynamic_sequence_test,
            get_dynamic_anno_test,
            get_fingerprint_test
        ]},
        {template_binding_tests, [parallel], [
            get_binding_test,
            get_binding_with_default_test,
            find_binding_test
        ]},
        {template_render_callback_tests, [parallel], [
            render_stateful_callback_test,
            render_stateless_callback_test,
            render_slot_view_callback_test,
            render_slot_template_callback_test,
            render_slot_term_test,
            render_list_template_callback_test,
            render_map_template_callback_test,
            render_map_error_test,
            from_html_with_module_function_test,
            render_list_error_test
        ]},
        {template_render_options_tests, [parallel], [
            render_stateful_with_options_test,
            render_stateful_options_diff_false_test,
            render_stateless_with_options_test,
            render_stateless_options_diff_false_test,
            render_list_with_options_test,
            render_list_options_diff_false_test,
            render_list_template_with_options_test,
            render_list_template_options_diff_false_test,
            render_map_with_options_test,
            render_map_options_diff_false_test,
            render_map_template_with_options_test,
            render_map_template_options_diff_false_test,
            render_slot_with_options_test,
            render_slot_options_diff_false_test,
            render_list_with_parse_transform_test,
            render_map_with_parse_transform_test
        ]}
    ].

init_per_testcase(from_html_file_test, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Filename = filename:join(DataDir, "html_template.herl"),
    [{filename, Filename} | Config];
init_per_testcase(from_html_priv_file_test, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ArizonaPrivDir = code:priv_dir(arizona),
    ok = filelib:ensure_dir(filename:join(ArizonaPrivDir, "dummy")),
    SourceFile = filename:join(DataDir, "html_template.herl"),
    Filename = "test_template.herl",
    TargetFile = filename:join(ArizonaPrivDir, Filename),
    {ok, _} = file:copy(SourceFile, TargetFile),
    [{priv_file, TargetFile}, {filename, Filename} | Config];
init_per_testcase(from_markdown_file_test, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Filename = filename:join(DataDir, "markdown_template.herl"),
    [{filename, Filename} | Config];
init_per_testcase(from_markdown_priv_file_test, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ArizonaPrivDir = code:priv_dir(arizona),
    ok = filelib:ensure_dir(filename:join(ArizonaPrivDir, "dummy")),
    SourceFile = filename:join(DataDir, "markdown_template.herl"),
    Filename = "test_markdown.herl",
    TargetFile = filename:join(ArizonaPrivDir, Filename),
    {ok, _} = file:copy(SourceFile, TargetFile),
    [{priv_file, TargetFile}, {filename, Filename} | Config];
init_per_testcase(render_list_with_parse_transform_test, Config) ->
    MockModule = test_render_list_options,
    MockModuleCode = merl:qquote(~"""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -export([test_template/0]).

    test_template() ->
        List = [~"item1", ~"item2"],
        arizona_template:from_html(~""""
        <ul>
        {arizona_template:render_list(
            fun(Item) ->
                arizona_template:from_html(~"""
                <li>{Item}</li>
                """)
            end,
            List,
            #{update => false}
        )}
        </ul>
        """").
    """"", [{module, merl:term(MockModule)}]),
    {ok, _Binary} = merl:compile_and_load(MockModuleCode, []),
    [{mock_module, MockModule} | Config];
init_per_testcase(render_map_with_parse_transform_test, Config) ->
    MockModule = test_render_map_options,
    MockModuleCode = merl:qquote(~"""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -export([test_template/0]).

    test_template() ->
        Map = #{~"key1" => ~"value1", ~"key2" => ~"value2"},
        arizona_template:from_html(~""""
        <ul>
        {arizona_template:render_map(
            fun({Key, Value}) ->
                arizona_template:from_html(~"""
                <li>{Key}: {Value}</li>
                """)
            end,
            Map,
            #{update => false}
        )}
        </ul>
        """").
    """"", [{module, merl:term(MockModule)}]),
    {ok, _Binary} = merl:compile_and_load(MockModuleCode, []),
    [{mock_module, MockModule} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(TestCase, Config) when
    TestCase =:= from_html_priv_file_test;
    TestCase =:= from_markdown_priv_file_test
->
    {priv_file, PrivFile} = proplists:lookup(priv_file, Config),
    ok = file:delete(PrivFile),
    ok;
end_per_testcase(TestCase, Config) when
    TestCase =:= render_list_with_parse_transform_test;
    TestCase =:= render_map_with_parse_transform_test
->
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    code:purge(MockModule),
    code:delete(MockModule),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%% --------------------------------------------------------------------
%% Template creation tests
%% --------------------------------------------------------------------

from_html_simple_test(Config) when is_list(Config) ->
    ct:comment("from_html/1 should create template from simple string"),
    Template = arizona_template:from_html(~"<h1>Hello World</h1>"),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>Hello World</h1>"], Static).

from_html_with_dynamic_test(Config) when is_list(Config) ->
    ct:comment("from_html/1 should create template with dynamic content"),
    Template = arizona_template:from_html(~"<h1>{~\"Test\"}</h1>"),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>"], Static).

from_html_full_params_test(Config) when is_list(Config) ->
    ct:comment("from_html/4 should create template with module, line, and bindings"),
    Bindings = #{title => ~"Test Title"},
    Template = arizona_template:from_html(
        ?MODULE, ?LINE, ~"<h1>{arizona_template:get_binding(title, Bindings)}</h1>", Bindings, []
    ),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>"], Static).

from_html_file_test(Config) when is_list(Config) ->
    ct:comment("from_html/1 should create template from file"),
    {filename, Filename} = proplists:lookup(filename, Config),
    Template = arizona_template:from_html({file, Filename}),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    % Verify it contains expected HTML structure
    ExpectedStatic = [
        ~"<html>\n<head>\n    <title>",
        ~"</title>\n</head>\n<body>\n    <h1>",
        ~"</h1>\n    <p>Welcome ",
        ~"!</p>\n    <ul>\n        ",
        ~"\n    </ul>\n</body>\n</html>\n"
    ],
    ?assertEqual(ExpectedStatic, Static).

from_html_priv_file_test(Config) when is_list(Config) ->
    ct:comment("from_html/1 should create template from priv file"),
    % Test priv_file loading (file setup handled in init_per_testcase)
    {filename, Filename} = proplists:lookup(filename, Config),
    Template = arizona_template:from_html({priv_file, arizona, Filename}),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    % Verify it contains expected HTML structure
    ExpectedStatic = [
        ~"<html>\n<head>\n    <title>",
        ~"</title>\n</head>\n<body>\n    <h1>",
        ~"</h1>\n    <p>Welcome ",
        ~"!</p>\n    <ul>\n        ",
        ~"\n    </ul>\n</body>\n</html>\n"
    ],
    ?assertEqual(ExpectedStatic, Static).

from_markdown_simple_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should create template from simple markdown"),
    Template = arizona_template:from_markdown(~"# Hello World\n\nThis is **bold** text."),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>Hello World</h1>\n<p>This is <strong>bold</strong> text.</p>"], Static).

from_markdown_with_dynamic_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should create template with dynamic content in markdown"),
    Template = arizona_template:from_markdown(~"# {~\"Test Title\"}\n\nContent here."),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>\n<p>Content here.</p>"], Static),

    % Test that dynamic parts are correctly preserved
    Dynamic = arizona_template:get_dynamic(Template),
    ?assert(is_tuple(Dynamic)),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    ?assertEqual(1, length(DynamicSequence)).

from_markdown_mixed_content_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should handle mixed static and dynamic content"),
    Template = arizona_template:from_markdown(~"""
    # Hello {~"World"}!

    You have **{42}** items.

    - Item 1
    - Item {42}
    """),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ExpectedStatic = [
        ~"<h1>Hello ",
        ~"!</h1>\n<p>You have <strong>",
        ~"</strong> items.</p>\n<ul>\n<li>Item 1</li>\n<li>Item ",
        ~"</li>\n</ul>"
    ],
    ?assertEqual(ExpectedStatic, Static),

    % Test that multiple dynamic parts are preserved
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    ?assertEqual(3, length(DynamicSequence)).

from_markdown_with_comments_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should preserve Erlang comments"),
    Template = arizona_template:from_markdown(~"""
    # Title
    % This is an Erlang comment
    Hello {~"Test"}!
    """),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    % Comments should be preserved in the final template
    ?assert(
        lists:any(
            fun(Part) ->
                binary:match(Part, ~"% This is an Erlang comment") =/= nomatch
            end,
            Static
        )
    ),

    % Test that both dynamic and comment parts are preserved
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    ?assertEqual(1, length(DynamicSequence)).

from_markdown_file_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should create template from file"),
    {filename, Filename} = proplists:lookup(filename, Config),
    Template = arizona_template:from_markdown({file, Filename}),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    % Verify it contains expected markdown-to-HTML structure
    ExpectedStatic = [
        ~"<h1>",
        ~"</h1>\n<p>Welcome <strong>",
        ~"</strong>!</p>\n<p>",
        ~"</p>\n<h2>List of items</h2>\n<p>",
        ~"</p>"
    ],
    ?assertEqual(ExpectedStatic, Static).

from_markdown_priv_file_test(Config) when is_list(Config) ->
    ct:comment("from_markdown/1 should create template from priv file"),
    % Test priv_file loading (file setup handled in init_per_testcase)
    {filename, Filename} = proplists:lookup(filename, Config),
    Template = arizona_template:from_markdown({priv_file, arizona, Filename}),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    % Verify it contains expected markdown-to-HTML structure
    ExpectedStatic = [
        ~"<h1>",
        ~"</h1>\n<p>Welcome <strong>",
        ~"</strong>!</p>\n<p>",
        ~"</p>\n<h2>List of items</h2>\n<p>",
        ~"</p>"
    ],
    ?assertEqual(ExpectedStatic, Static).

is_template_test(Config) when is_list(Config) ->
    ct:comment("is_template/1 should correctly identify template records"),
    Template = arizona_template:from_html(~"<div>Test</div>"),
    ?assert(arizona_template:is_template(Template)),
    ?assertNot(arizona_template:is_template(~"not a template")),
    ?assertNot(arizona_template:is_template(#{not_a => template})),
    ?assertNot(arizona_template:is_template(123)).

%% --------------------------------------------------------------------
%% Template accessor tests
%% --------------------------------------------------------------------

get_static_test(Config) when is_list(Config) ->
    ct:comment("get_static/1 should return static content"),
    Template = arizona_template:from_html(~"<div>Static</div>"),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<div>Static</div>"], Static).

get_dynamic_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic/1 should return dynamic content"),
    Template = arizona_template:from_html(~"<div>{~\"dynamic\"}</div>"),
    Dynamic = arizona_template:get_dynamic(Template),
    ?assert(is_tuple(Dynamic)).

get_dynamic_sequence_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic_sequence/1 should return dynamic sequence"),
    Template = arizona_template:from_html(~"<div>{~\"dynamic\"}</div>"),
    Sequence = arizona_template:get_dynamic_sequence(Template),
    ?assert(is_list(Sequence)).

get_dynamic_anno_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic_anno/1 should return dynamic annotation"),
    Template = arizona_template:from_html(~"<div>{~\"dynamic\"}</div>"),
    Anno = arizona_template:get_dynamic_anno(Template),
    ?assert(is_tuple(Anno)).

get_fingerprint_test(Config) when is_list(Config) ->
    ct:comment("get_fingerprint/1 should return template fingerprint"),
    Template = arizona_template:from_html(~"<div>Test</div>"),
    Fingerprint = arizona_template:get_fingerprint(Template),
    ?assert(is_integer(Fingerprint)),
    ?assert(Fingerprint >= 0).

%% --------------------------------------------------------------------
%% Template binding tests
%% --------------------------------------------------------------------

get_binding_test(Config) when is_list(Config) ->
    ct:comment("get_binding/2 should retrieve binding value"),
    Bindings = #{name => ~"John", age => 25},
    ?assertEqual(~"John", arizona_template:get_binding(name, Bindings)),
    ?assertEqual(25, arizona_template:get_binding(age, Bindings)).

get_binding_with_default_test(Config) when is_list(Config) ->
    ct:comment("get_binding/3 should use default when key not found"),
    Bindings = #{name => ~"John"},
    ?assertEqual(~"John", arizona_template:get_binding(name, Bindings, ~"Default")),
    ?assertEqual(
        ~"Default", arizona_template:get_binding(missing, Bindings, ~"Default")
    ).

find_binding_test(Config) when is_list(Config) ->
    ct:comment("find_binding/2 should return {ok, Value} or error"),
    Bindings = #{name => ~"John", age => 25},
    ?assertEqual({ok, ~"John"}, arizona_template:find_binding(name, Bindings)),
    ?assertEqual({ok, 25}, arizona_template:find_binding(age, Bindings)),
    ?assertEqual(error, arizona_template:find_binding(missing, Bindings)).

%% --------------------------------------------------------------------
%% Template render callback tests
%% --------------------------------------------------------------------

render_stateful_callback_test(Config) when is_list(Config) ->
    ct:comment("render_stateful/2 should return arity 4 render callback"),
    MockModule = test_module,
    Bindings = #{id => ~"test"},
    Callback = arizona_template:render_stateful(MockModule, Bindings),
    ?assert(is_function(Callback, 4)).

render_stateless_callback_test(Config) when is_list(Config) ->
    ct:comment("render_stateless/3 should return arity 4 render callback"),
    MockModule = test_module,
    MockFun = test_function,
    Bindings = #{content => ~"test"},
    Callback = arizona_template:render_stateless(MockModule, MockFun, Bindings),
    ?assert(is_function(Callback, 4)).

render_slot_view_callback_test(Config) when is_list(Config) ->
    ct:comment("render_slot/1 with view should return arity 4 render callback"),
    Callback = arizona_template:render_slot(view),
    ?assert(is_function(Callback, 4)).

render_slot_template_callback_test(Config) when is_list(Config) ->
    ct:comment("render_slot/1 with template should return arity 4 render callback"),
    Template = arizona_template:from_html(~"<span>Slot Content</span>"),
    Callback = arizona_template:render_slot(Template),
    ?assert(is_function(Callback, 4)).

render_slot_term_test(Config) when is_list(Config) ->
    ct:comment("render_slot/1 with term should return HTML"),
    % Test with binary term (converted to HTML)
    Result = arizona_template:render_slot(~"Simple text"),
    ?assertEqual(~"Simple text", Result),

    % Test with integer term (converted to HTML)
    NumberResult = arizona_template:render_slot(42),
    ?assertEqual(~"42", NumberResult).

render_list_template_callback_test(Config) when is_list(Config) ->
    ct:comment("render_list_template/2 should return arity 4 render callback"),
    Template = arizona_template:from_html(~"<li>Static Item</li>"),
    List = [~"first", ~"second", ~"third"],
    Callback = arizona_template:render_list_template(Template, List),
    ?assert(is_function(Callback, 4)).

from_html_with_module_function_test(Config) when is_list(Config) ->
    ct:comment("from_html/4 should handle module function calls"),
    TestModule = ?MODULE,
    Bindings = #{name => ~"World"},
    Template = arizona_template:from_html(
        TestModule,
        ?LINE,
        ~"<h1>{arizona_template:get_binding(name, Bindings)}</h1>",
        Bindings,
        []
    ),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>"], Static).

render_list_error_test(Config) when is_list(Config) ->
    ct:comment("render_list/2 should handle function info failure gracefully"),
    BadCallback = fun(_Item) -> ok end,
    List = [~"item1", ~"item2"],
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_list(BadCallback, List)
    ).

render_map_template_callback_test(Config) when is_list(Config) ->
    ct:comment("render_map_template/2 should return arity 4 render callback"),
    Template = arizona_template:from_html(~"<li>Key: {Key}, Value: {Value}</li>"),
    Map = #{~"first" => ~"1", ~"second" => ~"2"},
    Callback = arizona_template:render_map_template(Template, Map),
    ?assert(is_function(Callback, 4)).

render_map_error_test(Config) when is_list(Config) ->
    ct:comment("render_map/2 should handle function info failure gracefully"),
    BadCallback = fun(_Item) -> ok end,
    Map = #{~"key1" => ~"value1", ~"key2" => ~"value2"},
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_map(BadCallback, Map)
    ).

%% --------------------------------------------------------------------
%% Template render options tests
%% --------------------------------------------------------------------

render_stateful_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_stateful/3 should return arity 4 render callback with options"),
    MockModule = test_module,
    Bindings = #{id => ~"test"},
    Options = #{update => true},
    Callback = arizona_template:render_stateful(MockModule, Bindings, Options),
    ?assert(is_function(Callback, 4)).

render_stateful_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_stateful/3 with update => false should return nodiff on diff mode"),
    MockModule = test_module,
    Bindings = #{id => ~"test"},
    Options = #{update => false},
    Callback = arizona_template:render_stateful(MockModule, Bindings, Options),

    % Mock view for testing
    MockView = arizona_view:new(test_module, #{}, none),
    MockParentId = ~"parent",
    MockElementIndex = 1,

    % Call diff mode - should return nodiff
    Result = Callback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, Result).

render_stateless_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_stateless/4 should return arity 4 render callback with options"),
    MockModule = test_module,
    MockFun = test_function,
    Bindings = #{data => ~"test"},
    Options = #{update => true},
    Callback = arizona_template:render_stateless(MockModule, MockFun, Bindings, Options),
    ?assert(is_function(Callback, 4)).

render_stateless_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_stateless/4 with update => false should return nodiff on diff mode"),
    MockModule = test_module,
    MockFun = test_function,
    Bindings = #{data => ~"test"},
    Options = #{update => false},
    Callback = arizona_template:render_stateless(MockModule, MockFun, Bindings, Options),

    % Mock view for testing
    MockView = arizona_view:new(test_module, #{}, none),
    MockParentId = ~"parent",
    MockElementIndex = 1,

    % Call diff mode - should return nodiff
    Result = Callback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, Result).

render_list_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_list/3 should return arity 4 render callback with options"),
    ItemCallback = fun(_Item) ->
        arizona_template:from_html(~"<li>test</li>")
    end,
    List = [~"item1", ~"item2"],
    Options = #{update => true},
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_list(ItemCallback, List, Options)
    ).

render_list_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_list/3 with update => false should handle options correctly"),
    ItemCallback = fun(_Item) ->
        arizona_template:from_html(~"<li>test</li>")
    end,
    List = [~"item1", ~"item2"],
    Options = #{update => false},
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_list(ItemCallback, List, Options)
    ).

render_list_template_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_list_template/3 should return arity 4 render callback with options"),
    Template = arizona_template:from_html(~"<li>{Item}</li>"),
    List = [~"item1", ~"item2"],
    Options = #{update => true},
    Callback = arizona_template:render_list_template(Template, List, Options),
    ?assert(is_function(Callback, 4)).

render_list_template_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_list_template/3 with update => false should return nodiff on diff mode"),
    Template = arizona_template:from_html(~"<li>{Item}</li>"),
    List = [~"item1", ~"item2"],
    Options = #{update => false},
    Callback = arizona_template:render_list_template(Template, List, Options),

    % Mock view for testing
    MockView = arizona_view:new(test_module, #{}, none),
    MockParentId = ~"parent",
    MockElementIndex = 1,

    % Call diff mode - should return nodiff
    Result = Callback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, Result).

render_map_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_map/3 should return arity 4 render callback with options"),
    ItemCallback = fun({_Key, _Value}) ->
        arizona_template:from_html(~"<li>test</li>")
    end,
    Map = #{~"key1" => ~"value1"},
    Options = #{update => true},
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_map(ItemCallback, Map, Options)
    ).

render_map_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_map/3 with update => false should handle options correctly"),
    ItemCallback = fun({_Key, _Value}) ->
        arizona_template:from_html(~"<li>test</li>")
    end,
    Map = #{~"key1" => ~"value1"},
    Options = #{update => false},
    ?assertError(
        {function_info_failed, _},
        arizona_template:render_map(ItemCallback, Map, Options)
    ).

render_map_template_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_map_template/3 should return arity 4 render callback with options"),
    Template = arizona_template:from_html(~"<li>Key: {Key}, Value: {Value}</li>"),
    Map = #{~"key1" => ~"value1"},
    Options = #{update => true},
    Callback = arizona_template:render_map_template(Template, Map, Options),
    ?assert(is_function(Callback, 4)).

render_map_template_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_map_template/3 with update => false should return nodiff on diff mode"),
    Template = arizona_template:from_html(~"<li>Key: {Key}, Value: {Value}</li>"),
    Map = #{~"key1" => ~"value1"},
    Options = #{update => false},
    Callback = arizona_template:render_map_template(Template, Map, Options),

    % Mock view for testing
    MockView = arizona_view:new(test_module, #{}, none),
    MockParentId = ~"parent",
    MockElementIndex = 1,

    % Call diff mode - should return nodiff
    Result = Callback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, Result).

render_list_with_parse_transform_test(Config) when is_list(Config) ->
    ct:comment("render_list/3 with options should work through parse transform"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),

    % Test that the module compiles and returns a template
    Template = apply(MockModule, test_template, []),
    ?assert(arizona_template:is_template(Template)).

render_map_with_parse_transform_test(Config) when is_list(Config) ->
    ct:comment("render_map/3 with options should work through parse transform"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),

    % Test that the module compiles and returns a template
    Template = apply(MockModule, test_template, []),
    ?assert(arizona_template:is_template(Template)).

render_slot_with_options_test(Config) when is_list(Config) ->
    ct:comment("render_slot/2 should return arity 4 render callback with options for view slot"),
    Options = #{update => true},
    Callback = arizona_template:render_slot(view, Options),
    ?assert(is_function(Callback, 4)),

    % Test with template slot
    Template = arizona_template:from_html(~"<div>Test content</div>"),
    TemplateCallback = arizona_template:render_slot(Template, Options),
    ?assert(is_function(TemplateCallback, 4)),

    % Test with HTML term slot
    HtmlResult = arizona_template:render_slot(~"<span>HTML term</span>", Options),
    ?assertEqual(~"<span>HTML term</span>", HtmlResult).

render_slot_options_diff_false_test(Config) when is_list(Config) ->
    ct:comment("render_slot/2 with update => false should return nodiff on diff mode"),
    Options = #{update => false},

    % Test with view slot
    ViewCallback = arizona_template:render_slot(view, Options),
    MockView = arizona_view:new(test_module, #{}, none),
    MockParentId = ~"parent",
    MockElementIndex = 1,
    ViewResult = ViewCallback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, ViewResult),

    % Test with template slot
    Template = arizona_template:from_html(~"<div>Test content</div>"),
    TemplateCallback = arizona_template:render_slot(Template, Options),
    TemplateResult = TemplateCallback(diff, MockParentId, MockElementIndex, MockView),
    ?assertEqual({nodiff, MockView}, TemplateResult),

    % Test with HTML term slot (should not be affected by options)
    HtmlResult = arizona_template:render_slot(~"<span>HTML term</span>", Options),
    ?assertEqual(~"<span>HTML term</span>", HtmlResult).
