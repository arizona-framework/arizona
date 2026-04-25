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
    to_bin_integer/1
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
            to_bin_integer
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
