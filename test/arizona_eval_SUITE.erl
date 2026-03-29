-module(arizona_eval_SUITE).
-include_lib("stdlib/include/assert.hrl").
-dialyzer({nowarn_function, eval_each_def_3tuple/1}).

-export([all/0, groups/0]).
-export([
    eval_each_def_3tuple/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            eval_each_def_3tuple
        ]}
    ].

eval_each_def_3tuple(Config) when is_list(Config) ->
    %% eval_each_def with 3-tuple location
    Items = [#{name => <<"a">>}],
    Tmpl = #{
        t => 0, s => [<<"<li>">>], d => fun(I) -> [{<<"0">>, maps:get(name, I)}] end, f => <<"x">>
    },
    Def = {<<"0">>, fun() -> arizona_template:each(Items, Tmpl) end, {handler, 15}},
    {Az, Val, _Deps} = arizona_eval:eval_each_def(Def),
    ?assertEqual(<<"0">>, Az),
    ?assertMatch(#{t := 0, source := _, template := _}, Val).
