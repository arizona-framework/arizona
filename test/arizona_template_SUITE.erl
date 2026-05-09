-module(arizona_template_SUITE).
-include_lib("stdlib/include/assert.hrl").

%% Dialyzer flags `arizona_template:to_bin(make_ref())` as "will never
%% return" -- which is correct: the catch-all clause raises. Suppress
%% per-function rather than fixing the spec, matching the precedent in
%% arizona_parse_transform_SUITE.
-dialyzer({nowarn_function, to_bin_bad_value_routes_through_format_error_2/1}).

-export([all/0, groups/0]).
-export([
    html_stub/1,
    stateful_descriptor/1,
    stateless_descriptor_fun/1,
    stateless_descriptor/1,
    to_bin_atom/1,
    to_bin_binary/1,
    to_bin_integer/1,
    get_missing_binding_raises_named_reason/1,
    format_error_missing_binding_renders_message/1,
    to_bin_bad_value_routes_through_format_error_2/1,
    parse_transform_not_applied_routes_through_format_error_2/1,
    format_error_missing_binding_suggests_close_match/1,
    format_error_missing_binding_no_suggestion_for_far_match/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            html_stub,
            stateful_descriptor,
            stateless_descriptor_fun,
            stateless_descriptor,
            to_bin_atom,
            to_bin_binary,
            to_bin_integer,
            get_missing_binding_raises_named_reason,
            format_error_missing_binding_renders_message,
            to_bin_bad_value_routes_through_format_error_2,
            parse_transform_not_applied_routes_through_format_error_2,
            format_error_missing_binding_suggests_close_match,
            format_error_missing_binding_no_suggestion_for_far_match
        ]}
    ].

%% --- stateful/2 ---

stateful_descriptor(Config) when is_list(Config) ->
    Desc = arizona_template:stateful(arizona_counter, #{id => <<"c1">>, count => 0}),
    ?assertEqual(
        #{
            stateful => arizona_counter,
            props => #{id => <<"c1">>, count => 0}
        },
        Desc
    ).

%% --- stateless/2,3 ---

stateless_descriptor_fun(Config) when is_list(Config) ->
    Desc = arizona_template:stateless(fun arizona_counter:render/1, #{count => 5}),
    ?assertMatch(#{callback := _, props := #{count := 5}}, Desc),
    #{callback := Cb} = Desc,
    ?assert(is_function(Cb, 1)).

stateless_descriptor(Config) when is_list(Config) ->
    Desc = arizona_template:stateless(arizona_counter, render, #{count => 5}),
    ?assertMatch(#{callback := _, props := #{count := 5}}, Desc),
    %% Callback is a fun/1
    #{callback := Cb} = Desc,
    ?assert(is_function(Cb, 1)).

%% --- to_bin/1 ---

to_bin_binary(Config) when is_list(Config) ->
    ?assertEqual(<<"hello">>, arizona_template:to_bin(<<"hello">>)).

to_bin_integer(Config) when is_list(Config) ->
    ?assertEqual(<<"42">>, arizona_template:to_bin(42)).

to_bin_atom(Config) when is_list(Config) ->
    ?assertEqual(<<"true">>, arizona_template:to_bin(true)).

%% --- html/1 stub ---

html_stub(Config) when is_list(Config) ->
    ?assertError(parse_transform_not_applied, arizona_template:html(foo)).

%% --- get/2 missing-binding error ---

get_missing_binding_raises_named_reason(Config) when is_list(Config) ->
    %% A typo'd ?get/1 (e.g. `?get(tilte)` instead of `?get(title)`) hits
    %% arizona_template:get/2 with a key that's not in Bindings. The
    %% public contract: raise `error:missing_binding` carrying an
    %% `error_info` annotation that points at arizona_template, so
    %% `erl_error:format_exception/3` dispatches to format_error/2.
    {Reason, Stack} =
        try arizona_template:get(tilte, #{id => <<"p">>, title => <<"hi">>}) of
            _ -> ct:fail(expected_missing_binding)
        catch
            error:R:ST -> {R, ST}
        end,
    ?assertEqual(missing_binding, Reason),
    [{arizona_template, get, [tilte, _Bindings], Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_template},
        proplists:get_value(error_info, Info)
    ).

format_error_missing_binding_renders_message(Config) when is_list(Config) ->
    %% format_error/2 returns the human sentence the dev page surfaces
    %% via erl_error:format_exception/3.
    Bindings = #{id => <<"p">>, title => <<"hi">>},
    Stack =
        try arizona_template:get(tilte, Bindings) of
            _ -> ct:fail(expected_missing_binding)
        catch
            error:missing_binding:ST -> ST
        end,
    #{general := Msg} = arizona_template:format_error(missing_binding, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"binding tilte not found">>)),
    %% Available keys are sorted and listed.
    ?assertNotEqual(nomatch, binary:match(Bin, <<"id,title">>)).

%% --- to_bin/1 routes bad-value errors through format_error/2 ---

to_bin_bad_value_routes_through_format_error_2(Config) when is_list(Config) ->
    %% to_bin/1 raises {bad_template_value, V} with an error_info annotation
    %% so erl_error:format_exception/3 dispatches to format_error/2, which
    %% delegates to the existing format_error/1 message.
    Stack =
        try arizona_template:to_bin(make_ref()) of
            _ -> ct:fail(expected_bad_template_value)
        catch
            error:{bad_template_value, _}:ST -> ST
        end,
    [{arizona_template, to_bin, [_], Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_template},
        proplists:get_value(error_info, Info)
    ),
    Reason = {bad_template_value, make_ref()},
    #{general := Msg} = arizona_template:format_error(Reason, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"cannot convert">>)).

%% --- html/1 routes parse_transform_not_applied through format_error/2 ---

parse_transform_not_applied_routes_through_format_error_2(Config) when is_list(Config) ->
    Stack =
        try arizona_template:html(foo) of
            _ -> ct:fail(expected_parse_transform_not_applied)
        catch
            error:parse_transform_not_applied:ST -> ST
        end,
    [{arizona_template, html, _, Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_template},
        proplists:get_value(error_info, Info)
    ),
    #{general := Msg} = arizona_template:format_error(parse_transform_not_applied, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"parse transform not applied">>)).

%% --- did-you-mean hints in missing_binding messages ---

format_error_missing_binding_suggests_close_match(Config) when is_list(Config) ->
    %% A one-character typo against a binding name should produce a suggestion.
    Stack =
        try arizona_template:get(tilte, #{id => <<"p">>, title => <<"hi">>}) of
            _ -> ct:fail(expected_missing_binding)
        catch
            error:missing_binding:ST -> ST
        end,
    #{general := Msg} = arizona_template:format_error(missing_binding, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"Did you mean title">>)).

format_error_missing_binding_no_suggestion_for_far_match(Config) when is_list(Config) ->
    %% A key with no near match in the binding list should not produce a suggestion.
    Stack =
        try arizona_template:get(xyzzy, #{id => <<"p">>, title => <<"hi">>}) of
            _ -> ct:fail(expected_missing_binding)
        catch
            error:missing_binding:ST -> ST
        end,
    #{general := Msg} = arizona_template:format_error(missing_binding, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertEqual(nomatch, binary:match(Bin, <<"Did you mean">>)).
