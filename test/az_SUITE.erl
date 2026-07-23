-module(az_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    get_default/1,
    get_lazy/1,
    get/1,
    with/1,
    html_stub/1,
    each_stub/1,
    stateful/1,
    stateless_2/1,
    stateless_3/1,
    track/1,
    local/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            get_default,
            get_lazy,
            get,
            with,
            html_stub,
            each_stub,
            stateful,
            stateless_2,
            stateless_3,
            track,
            local
        ]}
    ].

get(Config) when is_list(Config) ->
    ?assertEqual(1, az:get(x, #{x => 1})).

get_default(Config) when is_list(Config) ->
    ?assertEqual(2, az:get(y, #{}, 2)).

get_lazy(Config) when is_list(Config) ->
    ?assertEqual(3, az:get_lazy(z, #{}, fun() -> 3 end)).

with(Config) when is_list(Config) ->
    ?assertEqual(#{x => 1, y => 2}, az:with([x, y], #{x => 1, y => 2, z => 3})).

track(Config) when is_list(Config) ->
    ?assertEqual(ok, az:track(some_key)).

stateful(Config) when is_list(Config) ->
    Desc = az:stateful(some_mod, #{id => <<"x">>}),
    ?assertEqual(#{stateful => some_mod, props => #{id => <<"x">>}}, Desc).

stateless_2(Config) when is_list(Config) ->
    Desc = az:stateless(fun(_) -> ok end, #{id => <<"x">>}),
    ?assertMatch(#{callback := _, props := #{id := <<"x">>}}, Desc).

stateless_3(Config) when is_list(Config) ->
    Desc = az:stateless(some_mod, some_fun, #{id => <<"x">>}),
    ?assertMatch(#{callback := _, props := #{id := <<"x">>}}, Desc).

html_stub(Config) when is_list(Config) ->
    ?assertError(parse_transform_not_applied, az:html(foo)),
    ?assertError(parse_transform_not_applied, az:native(foo)),
    ?assertError(parse_transform_not_applied, az:terminal(foo)).

%% `each/2` is a parse transform stub like `html/1`, so an un-transformed call
%% must name the cause. `arizona_template:each/2` doubles as the runtime pairing
%% function the transform emits (with Source first), and forwarding the macro's
%% own (Fun, Source) order into it used to fall off its only clause as a bare
%% function_clause -- no hint that the transform simply had not run.
each_stub(Config) when is_list(Config) ->
    Fun = fun(Item) -> Item end,
    ?assertError(parse_transform_not_applied, az:each(Fun, [1, 2, 3])),
    ?assertError(parse_transform_not_applied, arizona_template:each(Fun, [1, 2, 3])),
    %% A 2-arg (stream/map) callback reports the same cause.
    StreamFun = fun(Item, _Key) -> Item end,
    ?assertError(parse_transform_not_applied, az:each(StreamFun, [1, 2, 3])),
    %% The pairing form the transform emits is untouched.
    Template = #{t => 0, s => [~"<li>", ~"</li>"], d => fun(Item) -> [Item] end},
    ?assertEqual(
        #{t => 0, source => [1, 2], template => Template},
        arizona_template:each([1, 2], Template)
    ).

local(Config) when is_list(Config) ->
    ?assertEqual(
        #{diff => false, az_local => ~"k", v => ~"v"},
        az:local(~"k", ~"v")
    ),
    %% An atom key is normalized to its binary form.
    ?assertEqual(
        #{diff => false, az_local => ~"open", v => ~"v"},
        az:local(open, ~"v")
    ).
