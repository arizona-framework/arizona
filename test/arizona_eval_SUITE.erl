-module(arizona_eval_SUITE).
-include_lib("stdlib/include/assert.hrl").
-dialyzer({nowarn_function, eval_each_def_3tuple/1}).

-export([all/0, groups/0]).
-export([
    eval_each_def_3tuple/1,
    eval_val_stateless_descriptor/1,
    stateless_callback_does_not_leak_deps/1,
    stateless_callback_with_no_eager_reads/1,
    stateless_callback_eager_reads_dropped_outer_reads_kept/1,
    stateless_callback_multiple_eager_reads_all_dropped/1,
    adjacent_dynamics_have_independent_deps/1,
    deeply_nested_stateless_no_leak_at_any_layer/1,
    stateless_inside_each_no_leak/1,
    stateless_callback_returning_fun_no_leak/1,
    stateful_mount_eager_read_no_leak/1
]).

all() ->
    [{group, eval_api}, {group, dep_isolation}].

groups() ->
    [
        {eval_api, [parallel], [
            eval_each_def_3tuple,
            eval_val_stateless_descriptor
        ]},
        {dep_isolation, [parallel], [
            stateless_callback_does_not_leak_deps,
            stateless_callback_with_no_eager_reads,
            stateless_callback_eager_reads_dropped_outer_reads_kept,
            stateless_callback_multiple_eager_reads_all_dropped,
            adjacent_dynamics_have_independent_deps,
            deeply_nested_stateless_no_leak_at_any_layer,
            stateless_inside_each_no_leak,
            stateless_callback_returning_fun_no_leak,
            stateful_mount_eager_read_no_leak
        ]}
    ].

%% --- eval API ---

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

%% --- dep isolation in nested templates ---

%% A stateless callback that eagerly reads a binding BEFORE returning the
%% inner template must not pollute the outer dynamic's deps.
stateless_callback_does_not_leak_deps(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        eager_value = arizona_template:get(eager_key, #{eager_key => eager_value}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_key, OuterDeps)).

%% Happy-path sanity: a callback that does no eager reads still produces an
%% empty deps map for the outer dynamic and renders correctly.
stateless_callback_with_no_eager_reads(Config) when is_list(Config) ->
    Callback = fun(Props) ->
        Title = maps:get(title, Props),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, Title}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{title => <<"hi">>}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertEqual(#{}, OuterDeps),
    ?assertMatch(#{s := [<<"<b>">>, <<"</b>">>]}, Val).

%% Outer-level reads (those happening in the outer dynamic's closure body
%% before the descriptor is constructed) MUST be tracked. Only callback-body
%% eager reads should be discarded.
stateless_callback_eager_reads_dropped_outer_reads_kept(Config) when is_list(Config) ->
    Dyn =
        {<<"0">>,
            fun() ->
                outer_value = arizona_template:get(outer_key, #{outer_key => outer_value}),
                Callback = fun(_Props) ->
                    eager_value = arizona_template:get(eager_key, #{eager_key => eager_value}),
                    #{
                        s => [<<"<b>">>, <<"</b>">>],
                        d => [{<<"0">>, <<"hi">>}],
                        f => <<"t1">>
                    }
                end,
                #{callback => Callback, props => #{}}
            end,
            {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assert(maps:is_key(outer_key, OuterDeps)),
    ?assertNot(maps:is_key(eager_key, OuterDeps)).

%% Multiple eager reads in a single callback body are all discarded together.
stateless_callback_multiple_eager_reads_all_dropped(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        a = arizona_template:get(eager_a, #{eager_a => a}),
        b = arizona_template:get(eager_b, #{eager_b => b}),
        c = arizona_template:get(eager_c, #{eager_c => c}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertEqual(#{}, OuterDeps).

%% Two adjacent dynamics: one with a leaky callback, one normal. The leaky
%% callback must not pollute the second dynamic's deps slot either.
adjacent_dynamics_have_independent_deps(Config) when is_list(Config) ->
    LeakyCallback = fun(_Props) ->
        leak_value = arizona_template:get(leak_key, #{leak_key => leak_value}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Dyn1 =
        {<<"0">>, fun() -> #{callback => LeakyCallback, props => #{}} end, {?MODULE, ?LINE}},
    Dyn2 =
        {<<"1">>,
            fun() ->
                normal_value = arizona_template:get(normal_key, #{normal_key => normal_value}),
                <<"static-value">>
            end,
            {?MODULE, ?LINE}},
    {[{<<"0">>, _, Deps0}, {<<"1">>, _, Deps1}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn1, Dyn2], {#{}, #{}}),
    ?assertEqual(#{}, Deps0),
    ?assertEqual(#{normal_key => true}, Deps1).

%% Three layers of stateless nesting, each with its own eager read in the
%% callback body. None of the eager reads should leak to any ancestor's deps.
deeply_nested_stateless_no_leak_at_any_layer(Config) when is_list(Config) ->
    InnerMost = fun(_P) ->
        c = arizona_template:get(inner_c_key, #{inner_c_key => c}),
        #{s => [<<"<i>">>, <<"</i>">>], d => [{<<"0">>, <<"deep">>}], f => <<"t-c">>}
    end,
    Middle = fun(_P) ->
        b = arizona_template:get(inner_b_key, #{inner_b_key => b}),
        #{
            s => [<<"<m>">>, <<"</m>">>],
            d => [{<<"0">>, fun() -> #{callback => InnerMost, props => #{}} end, {?MODULE, ?LINE}}],
            f => <<"t-b">>
        }
    end,
    Outer = fun(_P) ->
        a = arizona_template:get(inner_a_key, #{inner_a_key => a}),
        #{
            s => [<<"<o>">>, <<"</o>">>],
            d => [{<<"0">>, fun() -> #{callback => Middle, props => #{}} end, {?MODULE, ?LINE}}],
            f => <<"t-a">>
        }
    end,
    Dyn =
        {<<"0">>, fun() -> #{callback => Outer, props => #{}} end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(inner_a_key, OuterDeps)),
    ?assertNot(maps:is_key(inner_b_key, OuterDeps)),
    ?assertNot(maps:is_key(inner_c_key, OuterDeps)).

%% A ?each whose item template embeds a stateless with a leaky callback. The
%% outer dynamic's deps should not contain the per-item callback's eager reads.
stateless_inside_each_no_leak(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        x = arizona_template:get(eager_each_key, #{eager_each_key => x}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Items = [#{n => 1}, #{n => 2}],
    ItemTmpl = #{
        t => 0,
        s => [<<"<li>">>],
        d => fun(_I) ->
            [{<<"0">>, fun() -> #{callback => Callback, props => #{}} end, {?MODULE, ?LINE}}]
        end,
        f => <<"e">>
    },
    Dyn =
        {<<"0">>, fun() -> arizona_template:each(Items, ItemTmpl) end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_each_key, OuterDeps)).

%% A stateful handler whose mount/1 callback does an eager binding read
%% must not pollute the outer dynamic's deps. mount/1 (and handle_update/2)
%% lifecycle methods run inside eval_stateful's bracket.
stateful_mount_eager_read_no_leak(Config) when is_list(Config) ->
    Descriptor = #{stateful => arizona_leaky_mount, props => #{id => ~"leaky"}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_mount_key, OuterDeps)).

%% A callback that returns a 0-arity fun (which eval_val_v then unwraps via
%% its is_function/0 clause) must not leak the fun-body's reads into the
%% outer dynamic's deps.
stateless_callback_returning_fun_no_leak(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        fun() ->
            late_value = arizona_template:get(late_key, #{late_key => late_value}),
            #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
        end
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(late_key, OuterDeps)).
