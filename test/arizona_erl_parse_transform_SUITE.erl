-module(arizona_erl_parse_transform_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, static_templates}
    ].

groups() ->
    [
        {static_templates, [parallel], [
            static_element_no_attrs,
            static_element_with_attrs,
            static_element_with_char_literals,
            non_static_binary_ignored
        ]}
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

static_element_no_attrs(_Config) ->
    % Test that parse transform works for simplest case
    MockModule = test_static_simple,

    Code = merl:qquote(~"""
    -module('@module').
    -compile({parse_transform, arizona_erl_parse_transform}).
    -export([render/0]).

    render() ->
        {'div', [], [~"Hello"]}.
    """, [{module, merl:term(MockModule)}]),

    % Compile and load with parse transform
    case merl:compile_and_load(Code) of
        {ok, _Binary} ->
            ok;
        {error, Errors, Warnings} ->
            ct:fail("Compilation failed:~nErrors: ~p~nWarnings: ~p", [Errors, Warnings]);
        error ->
            ct:fail("Compilation failed with error")
    end,

    % Call render - should return arizona_template record
    Template = MockModule:render(),

    % Verify it's a template
    ?assert(arizona_template:is_template(Template)),

    % Verify static parts
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<div>", ~"Hello", ~"</div>"], Static),

    % Verify no dynamics
    Dynamic = arizona_template:get_dynamic(Template),
    ?assertEqual({}, Dynamic),

    % Cleanup
    code:purge(MockModule),
    code:delete(MockModule),

    ok.

static_element_with_attrs(_Config) ->
    % Test static attributes
    MockModule = test_static_attrs,

    Code = merl:qquote(~"""
    -module('@module').
    -compile({parse_transform, arizona_erl_parse_transform}).
    -export([render/0]).

    render() ->
        {'div', [{class, ~"app"}, {id, ~"main"}], [~"Hello"]}.
    """, [{module, merl:term(MockModule)}]),

    % Compile and load with parse transform
    case merl:compile_and_load(Code) of
        {ok, _Binary} ->
            ok;
        {error, Errors, Warnings} ->
            ct:fail("Compilation failed:~nErrors: ~p~nWarnings: ~p", [Errors, Warnings]);
        error ->
            ct:fail("Compilation failed with error")
    end,

    % Call render
    Template = MockModule:render(),

    % Verify it's a template
    ?assert(arizona_template:is_template(Template)),

    % Verify static parts include attributes
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<div class=\"app\" id=\"main\">", ~"Hello", ~"</div>"], Static),

    % Cleanup
    code:purge(MockModule),
    code:delete(MockModule),

    ok.

static_element_with_char_literals(_Config) ->
    % Test character literals in binaries: <<"H", $e, "llo">>
    MockModule = test_char_literals,

    Code = merl:qquote(~"""
    -module('@module').
    -compile({parse_transform, arizona_erl_parse_transform}).
    -export([render/0]).

    render() ->
        {'div', [], [<<"H", $e, $l, $l, $o>>]}.
    """, [{module, merl:term(MockModule)}]),

    % Compile and load with parse transform
    case merl:compile_and_load(Code) of
        {ok, _Binary} ->
            ok;
        {error, Errors, Warnings} ->
            ct:fail("Compilation failed:~nErrors: ~p~nWarnings: ~p", [Errors, Warnings]);
        error ->
            ct:fail("Compilation failed with error")
    end,

    % Call render
    Template = MockModule:render(),

    % Verify it's a template
    ?assert(arizona_template:is_template(Template)),

    % Verify static parts - character literals should be handled correctly
    Static = arizona_template:get_static(Template),
    ?assertEqual([~"<div>", ~"Hello", ~"</div>"], Static),

    % Cleanup
    code:purge(MockModule),
    code:delete(MockModule),

    ok.

non_static_binary_ignored(_Config) ->
    % Test that non-static binaries (with variables/function calls) are handled gracefully
    % Currently they should be ignored since we haven't implemented dynamic support yet
    MockModule = test_non_static_binary,

    Code = merl:qquote(~"""
    -module('@module').
    -compile({parse_transform, arizona_erl_parse_transform}).
    -export([render/1]).

    non_static() -> ~"dynamic".

    render(NonStatic) ->
        {'div', [], [<<~"Static ", NonStatic/binary, (non_static())/binary>>]}.
    """, [{module, merl:term(MockModule)}]),

    % Compile and load with parse transform
    case merl:compile_and_load(Code) of
        {ok, _Binary} ->
            ok;
        {error, Errors, Warnings} ->
            ct:fail("Compilation failed:~nErrors: ~p~nWarnings: ~p", [Errors, Warnings]);
        error ->
            ct:fail("Compilation failed with error")
    end,

    % Call render - should not crash, but currently won't include dynamic parts
    Template = MockModule:render(~"ignored"),

    % Verify it's a template
    ?assert(arizona_template:is_template(Template)),

    % Since dynamic parts are not yet supported, they should be skipped
    % This test verifies we don't crash on dynamic binaries
    Static = arizona_template:get_static(Template),
    % Currently extracts only static parts - dynamic parts are ignored
    ?assertEqual([~"<div>", ~"", ~"</div>"], Static),

    % Cleanup
    code:purge(MockModule),
    code:delete(MockModule),

    ok.
