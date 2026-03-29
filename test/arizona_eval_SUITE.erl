-module(arizona_eval_SUITE).
-include_lib("stdlib/include/assert.hrl").
-dialyzer({nowarn_function, eval_each_def_3tuple/1}).

-export([all/0, groups/0]).
-export([
    eval_each_def_3tuple/1,
    eval_val_stateless_descriptor/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            eval_each_def_3tuple,
            eval_val_stateless_descriptor
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

%% eval_val processes stateless descriptors (#{callback, props}).
eval_val_stateless_descriptor(Config) when is_list(Config) ->
    Callback = fun(Props) ->
        Title = maps:get(title, Props),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, Title}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{title => <<"hello">>}},
    Dyn = {<<"0">>, fun() -> Descriptor end},
    [{<<"0">>, Result}] = arizona_eval:eval_dynamics([Dyn]),
    ?assertMatch(#{s := [<<"<b>">>, <<"</b>">>], d := [{<<"0">>, <<"hello">>}]}, Result).
