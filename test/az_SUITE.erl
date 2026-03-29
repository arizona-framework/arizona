-module(az_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    get_default/1,
    get_lazy/1,
    get/1,
    html_stub/1,
    stateful/1,
    stateless_2/1,
    stateless_3/1,
    track/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            get_default,
            get_lazy,
            get,
            html_stub,
            stateful,
            stateless_2,
            stateless_3,
            track
        ]}
    ].

get(Config) when is_list(Config) ->
    ?assertEqual(1, az:get(x, #{x => 1})).

get_default(Config) when is_list(Config) ->
    ?assertEqual(2, az:get(y, #{}, 2)).

get_lazy(Config) when is_list(Config) ->
    ?assertEqual(3, az:get_lazy(z, #{}, fun() -> 3 end)).

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
    ?assertError(parse_transform_not_applied, az:html(foo)).
