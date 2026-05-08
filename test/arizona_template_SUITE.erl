-module(arizona_template_SUITE).
-include_lib("stdlib/include/assert.hrl").

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
    format_error_missing_binding_renders_message/1
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
            format_error_missing_binding_renders_message
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
