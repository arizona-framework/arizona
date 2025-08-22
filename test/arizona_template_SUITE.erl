-module(arizona_template_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, template_creation_tests},
        {group, template_accessor_tests},
        {group, template_binding_tests},
        {group, template_render_callback_tests}
    ].

groups() ->
    [
        {template_creation_tests, [parallel], [
            from_string_simple_test,
            from_string_with_dynamic_test,
            from_string_full_params_test,
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
            from_string_with_module_function_test,
            render_list_error_test
        ]}
    ].

%% --------------------------------------------------------------------
%% Template creation tests
%% --------------------------------------------------------------------

from_string_simple_test(Config) when is_list(Config) ->
    ct:comment("from_string/1 should create template from simple string"),
    Template = arizona_template:from_string(~"<h1>Hello World</h1>"),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>Hello World</h1>"], Static).

from_string_with_dynamic_test(Config) when is_list(Config) ->
    ct:comment("from_string/1 should create template with dynamic content"),
    Template = arizona_template:from_string(~"<h1>{~\"Test\"}</h1>"),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>"], Static).

from_string_full_params_test(Config) when is_list(Config) ->
    ct:comment("from_string/4 should create template with module, line, and bindings"),
    Bindings = #{title => ~"Test Title"},
    Template = arizona_template:from_string(
        ?MODULE, ?LINE, ~"<h1>{arizona_template:get_binding(title, Bindings)}</h1>", Bindings
    ),
    ?assert(arizona_template:is_template(Template)),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<h1>", ~"</h1>"], Static).

is_template_test(Config) when is_list(Config) ->
    ct:comment("is_template/1 should correctly identify template records"),
    Template = arizona_template:from_string(~"<div>Test</div>"),
    ?assert(arizona_template:is_template(Template)),
    ?assertNot(arizona_template:is_template(~"not a template")),
    ?assertNot(arizona_template:is_template(#{not_a => template})),
    ?assertNot(arizona_template:is_template(123)).

%% --------------------------------------------------------------------
%% Template accessor tests
%% --------------------------------------------------------------------

get_static_test(Config) when is_list(Config) ->
    ct:comment("get_static/1 should return static content"),
    Template = arizona_template:from_string(~"<div>Static</div>"),
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<div>Static</div>"], Static).

get_dynamic_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic/1 should return dynamic content"),
    Template = arizona_template:from_string(~"<div>{~\"dynamic\"}</div>"),
    Dynamic = arizona_template:get_dynamic(Template),
    ?assert(is_tuple(Dynamic)).

get_dynamic_sequence_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic_sequence/1 should return dynamic sequence"),
    Template = arizona_template:from_string(~"<div>{~\"dynamic\"}</div>"),
    Sequence = arizona_template:get_dynamic_sequence(Template),
    ?assert(is_list(Sequence)).

get_dynamic_anno_test(Config) when is_list(Config) ->
    ct:comment("get_dynamic_anno/1 should return dynamic annotation"),
    Template = arizona_template:from_string(~"<div>{~\"dynamic\"}</div>"),
    Anno = arizona_template:get_dynamic_anno(Template),
    ?assert(is_tuple(Anno)).

get_fingerprint_test(Config) when is_list(Config) ->
    ct:comment("get_fingerprint/1 should return template fingerprint"),
    Template = arizona_template:from_string(~"<div>Test</div>"),
    Fingerprint = arizona_template:get_fingerprint(Template),
    ?assert(is_integer(Fingerprint)),
    ?assert(Fingerprint >= 0).

%% --------------------------------------------------------------------
%% Template binding tests
%% --------------------------------------------------------------------

get_binding_test(Config) when is_list(Config) ->
    ct:comment("get_binding/2 should retrieve binding value"),
    BindingsMap = #{name => ~"John", age => 25},
    Bindings = arizona_binder:new(BindingsMap),
    ?assertEqual(~"John", arizona_template:get_binding(name, Bindings)),
    ?assertEqual(25, arizona_template:get_binding(age, Bindings)).

get_binding_with_default_test(Config) when is_list(Config) ->
    ct:comment("get_binding/3 should use default when key not found"),
    BindingsMap = #{name => ~"John"},
    Bindings = arizona_binder:new(BindingsMap),
    ?assertEqual(~"John", arizona_template:get_binding(name, Bindings, fun() -> ~"Default" end)),
    ?assertEqual(
        ~"Default", arizona_template:get_binding(missing, Bindings, fun() -> ~"Default" end)
    ).

find_binding_test(Config) when is_list(Config) ->
    ct:comment("find_binding/2 should return {ok, Value} or error"),
    BindingsMap = #{name => ~"John", age => 25},
    Bindings = arizona_binder:new(BindingsMap),
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
    Template = arizona_template:from_string(~"<span>Slot Content</span>"),
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
    Template = arizona_template:from_string(~"<li>Static Item</li>"),
    List = [~"first", ~"second", ~"third"],
    Callback = arizona_template:render_list_template(Template, List),
    ?assert(is_function(Callback, 4)).

from_string_with_module_function_test(Config) when is_list(Config) ->
    ct:comment("from_string/4 should handle module function calls"),
    TestModule = ?MODULE,
    Bindings = #{name => ~"World"},
    Template = arizona_template:from_string(
        TestModule,
        ?LINE,
        ~"<h1>{arizona_template:get_binding(name, Bindings)}</h1>",
        Bindings
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
