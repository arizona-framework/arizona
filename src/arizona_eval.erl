-module(arizona_eval).
-moduledoc """
Evaluates compile-time-emitted dynamics into runtime values.

Two flavors of evaluation:

- **Plain** (`eval_dynamics/1`) -- evaluates dynamic closures into values,
  no view tracking and no dependency capture. Used for SSR-style first
  renders where there's no diff state to maintain.
- **Tracked** (`eval_dynamics_v/2`) -- evaluates dynamics through a `Views`
  accumulator, capturing per-dynamic dependencies via the process
  dictionary key `$arizona_deps`. Used during diff so the next render
  can fast-skip dynamics whose deps didn't change.

Each `eval_one_v` clause brackets its closure call with
`erlang:put('$arizona_deps', #{})` and `erlang:erase('$arizona_deps')`,
so any `arizona_template:get/2,3` calls inside the closure populate that
map. The captured map becomes the dynamic's `Deps` and is later compared
against the `Changed` set in `arizona_diff:diff/4`.

## Container handling

Evaluation recurses into the following dynamic value shapes:

- **`each` containers** -- lists (`source := [_]`), streams
  (`source := #stream{}`), and maps (`source := #{...}`)
- **Nested templates** -- maps with `s` and `d`
- **Stateful descriptors** -- `#{stateful := H, props := P}` (mounts a
  child or reuses an existing one from `Old` views)
- **Stateless descriptors** -- `#{callback := F, props := P}`

## Restricted keys

`check_restricted_keys/3` enforces that `mount/1` cannot modify
framework-owned bindings like `id`. Violations raise
`restricted_key_modified` with a human-readable message via `format_error/2`.
""".

-include("arizona.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([eval_dynamics/1]).
-export([eval_dynamics_v/2]).
-export([eval_one_v_flat/2]).
-export([eval_each_def/1]).
-export([eval_stream_items/4]).
-export([render_stream_item/4]).
-export([render_stream_items_simple/3]).
-export([render_list_items/3]).
-export([render_list_items_simple/2]).
-export([render_map_items/3]).
-export([render_map_items_simple/2]).
-export([check_restricted_keys/3]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-ifdef(TEST).
%% Inline EUnit tests intentionally repeat input/expected tuples for
%% clarity of the assertions.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).
-endif.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(RESTRICTED_KEYS, [id]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Evaluates a list of dynamics into `[{Az, Value}]` snapshot entries.

No view tracking, no dependency capture. Used for SSR-style renders
where there is no diff state to maintain.
""".
-spec eval_dynamics(Dynamics) -> EvaledDynamics when
    Dynamics :: [arizona_template:dynamic()],
    EvaledDynamics :: [{arizona_template:az(), term()}].
eval_dynamics(Dynamics) ->
    [eval_one(D) || D <:- Dynamics].

-doc """
Evaluates a list of dynamics with view tracking and dependency capture.

Returns `{Triples, Views1}` where each triple is `{Az, Value, Deps}`.
`Deps` is the set of binding keys read during evaluation, captured via
the `$arizona_deps` process dictionary entry.
""".
-spec eval_dynamics_v(Dynamics, Views) -> {Triples, Views1} when
    Dynamics :: [arizona_template:dynamic()],
    Views :: {map(), map()},
    Triples :: [{arizona_template:az(), term(), map()}],
    Views1 :: {map(), map()}.
eval_dynamics_v(Dynamics, Views0) ->
    lists:mapfoldl(fun eval_one_v/2, Views0, Dynamics).

-doc """
Convenience wrapper around `eval_one_v/2` that flattens the return shape
from `{{Az, Val, Deps}, Views}` to `{Az, Val, Deps, Views}`.
""".
-spec eval_one_v_flat(Dynamic, Views) -> {Az, Value, Deps, Views1} when
    Dynamic :: arizona_template:dynamic(),
    Views :: {map(), map()},
    Az :: arizona_template:az(),
    Value :: term(),
    Deps :: map(),
    Views1 :: {map(), map()}.
eval_one_v_flat(Def, Views0) ->
    {{Az, Val, Deps}, Views1} = eval_one_v(Def, Views0),
    {Az, Val, Deps, Views1}.

-doc """
Evaluates an each definition by invoking its closure under dep tracking.

Returns `{Az, EachValue, Deps}` where `EachValue` is the each container
map (`#{t := ?EACH, source, template}`).
""".
-spec eval_each_def(Dynamic) -> {Az, Value, Deps} when
    Dynamic :: arizona_template:dynamic(),
    Az :: arizona_template:az(),
    Value :: map(),
    Deps :: map().
eval_each_def({Az, Fun, _Loc}) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    #{t := ?EACH, source := _, template := _} = Val = Fun(),
    Deps = erlang:erase('$arizona_deps'),
    {Az, Val, Deps};
eval_each_def({Az, Fun}) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    #{t := ?EACH, source := _, template := _} = Val = Fun(),
    Deps = erlang:erase('$arizona_deps'),
    {Az, Val, Deps}.

-doc """
Evaluates a list of stream item keys against the items map and template,
threading view state. Returns a `#{Key => ItemD}` map and updated views.
""".
-spec eval_stream_items(Keys, ItemsMap, Template, Views) -> {ItemSnaps, Views1} when
    Keys :: [term()],
    ItemsMap :: #{term() => term()},
    Template :: map(),
    Views :: {map(), map()},
    ItemSnaps :: #{term() => [{arizona_template:az(), term()}]},
    Views1 :: {map(), map()}.
eval_stream_items(Keys, ItemsMap, Tmpl, Views) ->
    eval_stream_items(Keys, ItemsMap, Tmpl, Views, #{}).

-doc """
Renders a single stream item by evaluating its dynamics with view tracking.
Returns `{ItemD, Views1}` where `ItemD` is `[{Az, Value}]`.
""".
-spec render_stream_item(Key, Item, Template, Views) -> {ItemD, Views1} when
    Key :: term(),
    Item :: term(),
    Template :: map(),
    Views :: {map(), map()},
    ItemD :: [{arizona_template:az(), term()}],
    Views1 :: {map(), map()}.
render_stream_item(Key, Item, #{d := DFun}, Views0) ->
    Dynamics = DFun(Item, Key),
    {Triples, Views1} = eval_dynamics_v(Dynamics, Views0),
    D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
    {D, Views1}.

-doc """
Renders stream items without view tracking. Returns `#{Key => ItemD}`.
""".
-spec render_stream_items_simple(Keys, ItemsMap, Template) -> ItemSnaps when
    Keys :: [term()],
    ItemsMap :: #{term() => term()},
    Template :: map(),
    ItemSnaps :: #{term() => [{arizona_template:az(), term()}]}.
render_stream_items_simple(Keys, ItemsMap, Tmpl) ->
    render_stream_items_simple(Keys, ItemsMap, Tmpl, #{}).

-doc """
Renders list items with view tracking. Returns `{[ItemD], Views1}`.
""".
-spec render_list_items(Items, Template, Views) -> {ItemDs, Views1} when
    Items :: [term()],
    Template :: map(),
    Views :: {map(), map()},
    ItemDs :: [[{arizona_template:az(), term()}]],
    Views1 :: {map(), map()}.
render_list_items(Items, #{d := DFun}, Views) ->
    render_list_items1(Items, DFun, Views).

-doc """
Renders list items without view tracking. Returns `[ItemD]`.
""".
-spec render_list_items_simple(Items, Template) -> ItemDs when
    Items :: [term()],
    Template :: map(),
    ItemDs :: [[{arizona_template:az(), term()}]].
render_list_items_simple(Items, #{d := DFun}) ->
    [eval_dynamics(DFun(Item)) || Item <:- Items].

-doc """
Renders map entries with view tracking. Returns `{[ItemD], Views1}`.

Each map entry is passed to the template's `d` callback as `(Key, Value)`.
""".
-spec render_map_items(Map, Template, Views) -> {ItemDs, Views1} when
    Map :: map(),
    Template :: map(),
    Views :: {map(), map()},
    ItemDs :: [[{arizona_template:az(), term()}]],
    Views1 :: {map(), map()}.
render_map_items(Map, #{d := DFun}, Views) ->
    maps:fold(
        fun(K, V, {Acc, V0}) ->
            Dynamics = DFun(K, V),
            {Triples, V1} = eval_dynamics_v(Dynamics, V0),
            D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
            {[D | Acc], V1}
        end,
        {[], Views},
        Map
    ).

-doc """
Renders map entries without view tracking. Returns `[ItemD]`.
""".
-spec render_map_items_simple(Map, Template) -> ItemDs when
    Map :: map(),
    Template :: map(),
    ItemDs :: [[{arizona_template:az(), term()}]].
render_map_items_simple(Map, #{d := DFun}) ->
    maps:fold(
        fun(K, V, Acc) -> [eval_dynamics(DFun(K, V)) | Acc] end,
        [],
        Map
    ).

-doc """
Asserts that `Bindings` did not modify any framework-restricted keys
(currently just `id`) compared to the original `Props`.

Raises `restricted_key_modified` with handler info if a restricted key
was changed by `mount/1`.
""".
-spec check_restricted_keys(Bindings, Props, Handler) -> ok when
    Bindings :: map(),
    Props :: map(),
    Handler :: module().
check_restricted_keys(Bindings, Props, Handler) ->
    lists:foreach(
        fun(Key) ->
            case Props of
                #{Key := Expected} ->
                    case Bindings of
                        #{Key := Expected} ->
                            ok;
                        #{Key := _} ->
                            error(restricted_key_modified, [Key, Handler], [
                                {error_info, #{module => ?MODULE}}
                            ])
                    end;
                #{} ->
                    ok
            end
        end,
        ?RESTRICTED_KEYS
    ).

-doc """
Formats `restricted_key_modified` errors into a human-readable message
naming the offending handler and key.
""".
-spec format_error(Reason, StackTrace) -> ErrorInfo when
    Reason :: term(),
    StackTrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(restricted_key_modified, [{_M, _F, [Key, Handler], _Info} | _]) ->
    #{
        general => io_lib:format(
            "~s:mount/1 modified restricted key '~s'. "
            "This key is owned by the framework and cannot be changed.",
            [Handler, Key]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

eval_one({Az, {attr, Name, Fun}, _Loc}) when is_function(Fun, 0) ->
    {Az, {attr, Name, eval_val(Fun())}};
eval_one({Az, Spec, _Loc}) ->
    {Az, eval_val(Spec)};
eval_one({Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    {Az, {attr, Name, eval_val(Fun())}};
eval_one({Az, Spec}) ->
    {Az, eval_val(Spec)}.

eval_val(Fun) when is_function(Fun, 0) ->
    eval_val(Fun());
eval_val(#{t := ?EACH, source := Items, template := Tmpl}) when is_list(Items) ->
    ItemSnaps = render_list_items_simple(Items, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
eval_val(#{
    t := ?EACH,
    source := #stream{
        items = ItemsMap,
        order = Order,
        limit = Limit
    },
    template := Tmpl
}) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    ItemSnaps = render_stream_items_simple(VKeys, ItemsMap, Tmpl),
    #{t => ?EACH, items => ItemSnaps, order => VKeys, template => Tmpl};
eval_val(#{t := ?EACH, source := Source, template := Tmpl}) when is_map(Source) ->
    ItemSnaps = render_map_items_simple(Source, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
eval_val(#{callback := Callback, props := Props}) ->
    eval_val(Callback(Props));
eval_val(#{s := Statics, d := Dynamics} = Tmpl) ->
    Snap0 = #{s => Statics, d => eval_dynamics(Dynamics)},
    arizona_template:maybe_propagate(Tmpl, Snap0);
eval_val(Val) ->
    Val.

eval_one_v({Az, {attr, Name, Fun}, _Loc}, Views) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    Val = eval_val(Fun()),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec, _Loc}, Views0) ->
    erlang:put('$arizona_deps', #{}),
    {Val, Views1} = eval_val_v(Spec, Views0),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, Val, Deps}, Views1};
eval_one_v({Az, {attr, Name, Fun}}, Views) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    Val = eval_val(Fun()),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec}, Views0) ->
    erlang:put('$arizona_deps', #{}),
    {Val, Views1} = eval_val_v(Spec, Views0),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, Val, Deps}, Views1}.

eval_val_v(Fun, Views0) when is_function(Fun, 0) ->
    eval_val_v(Fun(), Views0);
eval_val_v(#{stateful := H, props := Props}, Views) ->
    eval_stateful(H, Props, Views);
eval_val_v(#{t := ?EACH, source := Items, template := Tmpl}, Views) when is_list(Items) ->
    eval_each_list(Items, Tmpl, Views);
eval_val_v(#{t := ?EACH, source := #stream{} = Source, template := Tmpl}, Views) ->
    eval_each_stream(Source, Tmpl, Views);
eval_val_v(#{t := ?EACH, source := Source, template := Tmpl}, Views) when is_map(Source) ->
    eval_each_map(Source, Tmpl, Views);
eval_val_v(#{callback := Callback, props := Props}, Views) ->
    eval_val_v(Callback(Props), Views);
eval_val_v(#{s := _, d := _} = Tmpl, Views) ->
    eval_template(Tmpl, Views);
eval_val_v(Val, Views) ->
    {Val, Views}.

%% Wrap Fun so any deps captured during its execution are scoped: the
%% caller's existing `$arizona_deps` is restored when Fun returns.
with_saved_deps(Fun) ->
    SavedDeps = erlang:get('$arizona_deps'),
    Result = Fun(),
    erlang:put('$arizona_deps', SavedDeps),
    Result.

eval_stateful(H, Props, {Old, New}) ->
    Id = maps:get(id, Props),
    {B1, Resets} = mount_or_update_stateful(H, Props, Id, Old),
    with_saved_deps(fun() ->
        Tmpl = H:render(B1),
        {ChildTriples, {Old, New1}} = eval_dynamics_v(maps:get(d, Tmpl), {Old, New}),
        {ChildD, ChildDeps} = arizona_template:split_triples(ChildTriples),
        Snap = arizona_template:make_child_snap(Tmpl, ChildD, ChildDeps, Id),
        B2 = maps:merge(B1, Resets),
        ChildEntry = #{handler => H, bindings => B2, snapshot => Snap},
        {Snap, {Old, New1#{Id => ChildEntry}}}
    end).

mount_or_update_stateful(H, Props, Id, Old) ->
    case Old of
        #{Id := #{handler := H, bindings := B}} ->
            arizona_template:call_handle_update(H, Props, B);
        #{} ->
            {B1, Resets} = H:mount(Props),
            ok = check_restricted_keys(B1, Props, H),
            {B1, Resets}
    end.

eval_each_list(Items, Tmpl, Views) ->
    eval_each(fun(Old) -> render_list_items(Items, Tmpl, {Old, #{}}) end, Tmpl, Views, undefined).

eval_each_stream(#stream{items = ItemsMap, order = Order, limit = Limit} = Source, Tmpl, Views) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    eval_each(
        fun(Old) -> eval_stream_items(VKeys, ItemsMap, Tmpl, {Old, #{}}) end,
        Tmpl,
        Views,
        {VKeys, Source}
    ).

eval_each_map(Source, Tmpl, Views) ->
    eval_each(fun(Old) -> render_map_items(Source, Tmpl, {Old, #{}}) end, Tmpl, Views, undefined).

eval_each(RenderFun, Tmpl, {Old, New0}, StreamExtra) ->
    {ItemSnaps, {_, LocalNew}} = with_saved_deps(fun() -> RenderFun(Old) end),
    ChildViews = maps:keys(LocalNew),
    Snap = build_each_snap(ItemSnaps, Tmpl, ChildViews, StreamExtra),
    {Snap, {Old, maps:merge(New0, LocalNew)}}.

build_each_snap(ItemSnaps, Tmpl, ChildViews, undefined) ->
    #{t => ?EACH, items => ItemSnaps, template => Tmpl, child_views => ChildViews};
build_each_snap(ItemSnaps, Tmpl, ChildViews, {VKeys, Source}) ->
    #{
        t => ?EACH,
        items => ItemSnaps,
        order => VKeys,
        source => Source,
        template => Tmpl,
        child_views => ChildViews
    }.

eval_template(#{s := Statics, d := Dynamics} = Tmpl, {Old, New0}) ->
    {Triples, {_, LocalNew}} = with_saved_deps(fun() -> eval_dynamics_v(Dynamics, {Old, #{}}) end),
    {D, DepsList} = arizona_template:split_triples(Triples),
    ChildViews = maps:keys(LocalNew),
    Snap0 = #{s => Statics, d => D, deps => DepsList},
    Snap1 = arizona_template:maybe_propagate(Tmpl, Snap0),
    {Snap1#{child_views => ChildViews}, {Old, maps:merge(New0, LocalNew)}}.

eval_stream_items([], _ItemsMap, _Tmpl, Views, Acc) ->
    {Acc, Views};
eval_stream_items([K | Rest], ItemsMap, Tmpl, Views0, Acc) ->
    Item = maps:get(K, ItemsMap),
    {D, Views1} = render_stream_item(K, Item, Tmpl, Views0),
    eval_stream_items(Rest, ItemsMap, Tmpl, Views1, Acc#{K => D}).

render_stream_items_simple([], _ItemsMap, _Tmpl, Acc) ->
    Acc;
render_stream_items_simple([K | Rest], ItemsMap, Tmpl, Acc) ->
    Item = maps:get(K, ItemsMap),
    D = render_stream_item_simple(K, Item, Tmpl),
    render_stream_items_simple(Rest, ItemsMap, Tmpl, Acc#{K => D}).

render_stream_item_simple(Key, Item, #{d := DFun}) ->
    Dynamics = DFun(Item, Key),
    eval_dynamics(Dynamics).

render_list_items1([], _DFun, Views) ->
    {[], Views};
render_list_items1([Item | Rest], DFun, Views0) ->
    Dynamics = DFun(Item),
    {Triples, Views1} = eval_dynamics_v(Dynamics, Views0),
    D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
    {RestD, Views2} = render_list_items1(Rest, DFun, Views1),
    {[D | RestD], Views2}.

-ifdef(TEST).

eval_one_strips_location_test() ->
    %% eval_one with 3-tuple returns 2-tuple snapshot entry
    Result = eval_one({~"0", fun() -> ~"val" end, {some_mod, 7}}),
    ?assertEqual({~"0", ~"val"}, Result).

eval_one_strips_location_attr_test() ->
    %% eval_one with 3-tuple attr returns 2-tuple snapshot entry
    Result = eval_one({~"0", {attr, ~"class", fun() -> ~"x" end}, {m, 1}}),
    ?assertEqual({~"0", {attr, ~"class", ~"x"}}, Result).

eval_val_stateless_descriptor_test() ->
    Cb = fun(Props) ->
        #{s => [~"<b>", ~"</b>"], d => [{~"0", maps:get(t, Props)}], f => ~"x"}
    end,
    Result = eval_val(#{callback => Cb, props => #{t => ~"hi"}}),
    ?assertMatch(#{s := [~"<b>", ~"</b>"], d := [{~"0", ~"hi"}]}, Result).

eval_one_v_3tuple_test() ->
    %% eval_one_v with 3-tuple returns {Az, Val, Deps} triple
    {Result, _Views} = eval_one_v(
        {~"0", fun() -> ~"val" end, {some_mod, 7}}, {#{}, #{}}
    ),
    ?assertMatch({~"0", ~"val", #{}}, Result).

eval_map_simple_test() ->
    Map = #{~"a" => ~"1", ~"b" => ~"2"},
    Tmpl = #{
        t => 0,
        s => [~"<dt>", ~"</dt><dd>", ~"</dd>"],
        d => fun(K, V) -> [{~"0", fun() -> K end}, {~"1", fun() -> V end}] end,
        f => ~"test"
    },
    Result = render_map_items_simple(Map, Tmpl),
    ?assertEqual(2, length(Result)),
    %% Each item has key and value dynamics
    lists:foreach(
        fun(Item) ->
            [{~"0", Key}, {~"1", Val}] = Item,
            ?assert(is_binary(Key)),
            ?assert(is_binary(Val))
        end,
        Result
    ).

eval_map_empty_test() ->
    Tmpl = #{
        t => 0,
        s => [~"<li>", ~"</li>"],
        d => fun(K, _V) -> [{~"0", fun() -> K end}] end,
        f => ~"test"
    },
    ?assertEqual([], render_map_items_simple(#{}, Tmpl)).

eval_map_single_entry_test() ->
    Tmpl = #{
        t => 0,
        s => [~"<dt>", ~"</dt><dd>", ~"</dd>"],
        d => fun(K, V) -> [{~"0", fun() -> K end}, {~"1", fun() -> V end}] end,
        f => ~"test"
    },
    [[{~"0", ~"x"}, {~"1", ~"1"}]] = render_map_items_simple(#{~"x" => ~"1"}, Tmpl).

eval_map_with_views_test() ->
    Map = #{~"a" => ~"1"},
    Tmpl = #{
        t => 0,
        s => [~"<li>", ~"</li>"],
        d => fun(K, V) -> [{~"0", fun() -> K end}, {~"1", fun() -> V end}] end,
        f => ~"test"
    },
    {Items, {_Old, NewViews}} = render_map_items(Map, Tmpl, {#{}, #{}}),
    ?assertEqual(1, length(Items)),
    ?assertEqual(#{}, NewViews).

eval_map_via_eval_val_test() ->
    %% Test the full eval_val path for maps
    Map = #{~"x" => ~"1"},
    Tmpl = #{
        t => 0,
        s => [~"<li>", ~"</li>"],
        d => fun(K, _V) -> [{~"0", fun() -> K end}] end,
        f => ~"test"
    },
    Result = eval_val(#{t => 0, source => Map, template => Tmpl}),
    ?assertMatch(#{t := 0, items := _, template := _}, Result),
    ?assertEqual(1, length(maps:get(items, Result))).

check_restricted_keys_ok_test() ->
    %% Same id in Props and Bindings -- ok
    ?assertEqual(ok, check_restricted_keys(#{id => ~"v"}, #{id => ~"v"}, my_handler)).

check_restricted_keys_no_id_in_props_test() ->
    %% No id in Props (root handler) -- ok, mount can set default
    ?assertEqual(ok, check_restricted_keys(#{id => ~"v"}, #{}, my_handler)).

check_restricted_keys_modified_test() ->
    %% Mount changed id -- raises with handler info
    ?assertError(
        restricted_key_modified,
        check_restricted_keys(#{id => ~"changed"}, #{id => ~"original"}, my_handler)
    ).

format_error_restricted_key_test() ->
    %% format_error produces readable message with handler and key
    StackFrame = [{arizona_eval, check, [id, my_handler], []}],
    #{general := Msg} = format_error(restricted_key_modified, StackFrame),
    MsgBin = iolist_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(MsgBin, ~"my_handler:mount/1")),
    ?assertNotEqual(nomatch, binary:match(MsgBin, ~"'id'")).

-endif.
