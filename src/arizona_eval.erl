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

Each `eval_one_v` clause opens a dependency scope around its closure call
(`open_deps/0` .. `close_deps/1`), so any `arizona_template:get/2,3` calls
inside the closure populate that scope's map. The captured map becomes the
dynamic's `Deps` and is later compared against the `Changed` set in
`arizona_diff:diff/4`. A closure that raises restores the enclosing scope on
its way out, so a caught render crash leaves no scope open.

Scopes nest: a scope opened while another is open hands its reads to the
enclosing one when it closes. That is how an `?each` item's reads reach the
each slot -- items are evaluated inside the slot's scope, each item dynamic in
its own. A nested template, a `?stateless` body and a `?stateful` child's
lifecycle are evaluated under `with_saved_deps/1` instead, which suspends the
enclosing scope: their reads stay out of it (the parse transform records the
ones that matter on the enclosing slot at compile time).

## Container handling

Evaluation recurses into the following dynamic value shapes:

- **`each` containers** -- lists (`source := [_]`), streams
  (`source := #stream{}`), and maps (`source := #{...}`)
- **Nested templates** -- maps with `s` and `d`
- **Stateful descriptors** -- `#{stateful := H, props := P}` (mounts a
  child or reuses an existing one from `Old` views)
- **Stateless descriptors** -- `#{callback := F, props := P}`

## Restricted keys

`check_restricted_keys/3,4` enforces that a handler's callbacks (mount and
every later one that returns bindings) cannot modify or drop framework-owned
bindings like `id`. Violations raise
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
-export([eval_unless_same_nodiff/3]).
-export([eval_each_def/2]).
-export([eval_items/2]).
-export([eval_stream_items/4]).
-export([render_stream_item/4]).
-export([render_stream_item_skipping/6]).
-export([render_list_items/3]).
-export([render_map_items/3]).
-export([check_restricted_keys/3]).
-export([check_restricted_keys/4]).
-export([restricted_keys/0]).
-export([child_view_set/1]).
-export([set_update_effects/1]).
-export([drain_update_effects/0]).
-export([relocate/4]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).
-ignore_xref([restricted_keys/0]).

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
    [eval_one(D) || D <- Dynamics].

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
Evaluates a dynamic whose snapshot `Old` holds an az-nodiff element, unless the
dynamic still produces that az-nodiff element.

The slot's closure runs under dep tracking as in `eval_one_v_flat/2`, but its
result is inspected before anything inside it is evaluated (a `?stateless`
descriptor is rendered to its template first): a template that is the same az-nodiff element
(`arizona_template:same_nodiff/3`) answers `{same, Deps}` and its dynamics never
run -- the az-nodiff element renders once, so evaluating them would only cost the whole
az-nodiff element per change. Anything else is evaluated from the value already produced
(the closure is not called twice) and answered `{changed, Az, Value, Deps, Views1}`.
""".
-spec eval_unless_same_nodiff(Dynamic, Old, Views) -> Result when
    Dynamic :: arizona_template:dynamic(),
    Old :: arizona_template:snapshot(),
    Views :: {map(), map()},
    Result ::
        {same, map()}
        | {changed, arizona_template:az(), term(), map(), {map(), map()}}.
eval_unless_same_nodiff({Az, Spec, Loc}, Old, Views0) ->
    Outer = open_deps(),
    try
        unless_same_nodiff(Az, Spec, Old, Outer, Views0)
    catch
        Class:Reason:ST -> relocate(Class, Reason, ST, Loc, Outer)
    end;
eval_unless_same_nodiff({Az, Spec}, Old, Views0) ->
    Outer = open_deps(),
    try
        unless_same_nodiff(Az, Spec, Old, Outer, Views0)
    catch
        Class:Reason:ST -> abort_deps(Class, Reason, ST, Outer)
    end.

unless_same_nodiff(Az, Spec, Old, Outer, Views0) ->
    {Esc, Raw} = peek_spec(Spec),
    Val = peek_nodiff(Raw),
    case arizona_template:same_nodiff(Az, Val, Old) of
        true -> {same, close_deps(Outer)};
        false -> eval_peeked(Az, Esc, Val, Outer, Views0)
    end.

%% The first step of eval_val_v/2, taken alone so the result can be looked at
%% before it is evaluated.
peek_spec({esc, Fun}) when is_function(Fun, 0) -> {true, Fun()};
peek_spec(Fun) when is_function(Fun, 0) -> {false, Fun()};
peek_spec(Spec) -> {false, Spec}.

%% A `?stateless` descriptor is rendered here as eval_val_v/2 would, its body's
%% own reads kept out of the slot's deps.
peek_nodiff(#{callback := _, props := _} = Desc) ->
    with_saved_deps(fun() -> peek_nodiff(arizona_template:stateless_template(Desc)) end);
peek_nodiff(Val) ->
    Val.

eval_peeked(Az, Esc, Raw, Outer, Views0) ->
    {Val0, Views1} = eval_val_v(Raw, Views0),
    Val =
        case Esc of
            true -> arizona_template:mark_esc(Val0);
            false -> Val0
        end,
    Deps = close_deps(Outer),
    {changed, Az, arizona_template:scope_slot(Az, Val), Deps, Views1}.

-doc """
Re-evaluates a slot that last held an each container, under dep tracking,
without rendering its items.

Answers `{each, Az, EachValue, Deps}` while the slot still produces an each
container (`#{t := ?EACH, source, template}`), for the each-specific diff to
render the items against the old snapshot; `Deps` are then only the reads of the
slot's own closure (the source, a branch condition), and the diff adds the
items' through `eval_items/2`. The slot need not still hold an each -- a
conditional whose branch was one can land on any value -- and anything else is
evaluated from the value already produced, answered
`{changed, Az, Value, Deps, Views1}` as `eval_unless_same_nodiff/3` does.
""".
-spec eval_each_def(Dynamic, Views) -> Result when
    Dynamic :: arizona_template:dynamic(),
    Views :: {map(), map()},
    Result ::
        {each, arizona_template:az(), map(), map()}
        | {changed, arizona_template:az(), term(), map(), {map(), map()}}.
eval_each_def({Az, Spec, Loc}, Views0) ->
    Outer = open_deps(),
    try
        each_def(Az, Spec, Outer, Views0)
    catch
        Class:Reason:ST -> relocate(Class, Reason, ST, Loc, Outer)
    end;
eval_each_def({Az, Spec}, Views0) ->
    Outer = open_deps(),
    try
        each_def(Az, Spec, Outer, Views0)
    catch
        Class:Reason:ST -> abort_deps(Class, Reason, ST, Outer)
    end.

each_def(Az, Spec, Outer, Views0) ->
    case peek_spec(Spec) of
        {_Esc, #{t := ?EACH, source := _, template := _} = Each} ->
            {each, Az, Each, close_deps(Outer)};
        {Esc, Raw} ->
            eval_peeked(Az, Esc, Raw, Outer, Views0)
    end.

-doc """
Runs `Fun`, which renders items of an `?each` whose per-item template is
`Template`, in a dependency scope of its own, and returns `{Result, ItemDeps}`:
the binding keys the items read, at any depth (an inner each's items included).

They also reach the enclosing scope, if one is open, so an each slot evaluated
with its items depends on whatever its items read -- a change to an outer
binding read inside the callback re-evaluates the each, and the per-item diff
then patches only the items it changed. An az-nodiff item template renders once,
so its reads are suspended rather than collected (`#{}`).
""".
-spec eval_items(Template, Fun) -> {Result, ItemDeps} when
    Template :: map(),
    Fun :: fun(() -> Result),
    Result :: term(),
    ItemDeps :: map().
eval_items(#{diff := false}, Fun) ->
    {with_saved_deps(Fun), #{}};
eval_items(_Tmpl, Fun) ->
    Outer = open_deps(),
    try Fun() of
        Result -> {Result, close_deps(Outer)}
    catch
        Class:Reason:ST -> abort_deps(Class, Reason, ST, Outer)
    end.

-doc """
Evaluates a list of stream item keys against the items map and template,
threading view state. Returns a `#{Key => ItemD}` map and updated views.
""".
-spec eval_stream_items(Keys, ItemsMap, Template, Views) -> {ItemSnaps, Views1} when
    Keys :: [term()],
    ItemsMap :: #{term() => term()},
    Template :: map(),
    Views :: {map(), map()},
    ItemSnaps :: #{term() => [{arizona_template:az(), term(), map()}]},
    Views1 :: {map(), map()}.
eval_stream_items(Keys, ItemsMap, Tmpl, Views) ->
    eval_stream_items(Keys, ItemsMap, Tmpl, Views, #{}).

-doc """
Renders a single stream item by evaluating its dynamics with view tracking.
Returns `{ItemD, Views1}` where `ItemD` is `[{Az, Value, Deps}]`.
""".
-spec render_stream_item(Key, Item, Template, Views) -> {ItemD, Views1} when
    Key :: term(),
    Item :: term(),
    Template :: map(),
    Views :: {map(), map()},
    ItemD :: [{arizona_template:az(), term(), map()}],
    Views1 :: {map(), map()}.
render_stream_item(Key, Item, #{d := DFun}, Views0) ->
    Dynamics = DFun(Item, Key),
    eval_dynamics_v(Dynamics, Views0).

-doc """
Re-renders a stream item using `Changed` (set of fields differing between
old and new item) and the previous `OldItemD`, keeping the old
`{Az, Val, Deps}` triple of each per-item dynamic that cannot have changed, so
the downstream value comparison emits no op for it.

A dynamic qualifies only when the parse transform flagged it in the template's
`reuse` list (every item read it makes is tracked in its own closure, so its
deps name every field it read) and its deps miss `Changed`. Any other dynamic
re-evaluates, as does every dynamic of a template without the list: a read the
deps never recorded is an input the item change may have touched.
""".
-spec render_stream_item_skipping(Key, NewItem, OldItemD, Changed, Template, Views) ->
    {ItemD, Views1}
when
    Key :: term(),
    NewItem :: term(),
    OldItemD :: [{arizona_template:az(), term(), map()}],
    Changed :: map(),
    Template :: map(),
    Views :: {map(), map()},
    ItemD :: [{arizona_template:az(), term(), map()}],
    Views1 :: {map(), map()}.
render_stream_item_skipping(Key, NewItem, OldItemD, Changed, #{d := DFun} = Tmpl, Views0) ->
    case {map_size(Changed), Tmpl} of
        {0, _} ->
            %% No fields differ -- old item triples are reusable as-is.
            {OldItemD, Views0};
        {_, #{reuse := Reuse}} ->
            %% The changed keys are fixed across the item's dynamics --
            %% materialize once, probe per dynamic (see deps_changed/2).
            eval_or_reuse_per_item(
                DFun(NewItem, Key), OldItemD, Reuse, maps:keys(Changed), Views0
            );
        {_, #{}} ->
            eval_dynamics_v(DFun(NewItem, Key), Views0)
    end.

eval_or_reuse_per_item([], [], [], _ChangedKeys, Views) ->
    {[], Views};
eval_or_reuse_per_item(
    [Def | DR], [{_Az, _OldVal, OldDeps} = Old | OR], [Reuse | RR], ChangedKeys, Views0
) ->
    case Reuse andalso not arizona_diff:deps_changed(OldDeps, ChangedKeys) of
        true ->
            {Rest, Views1} = eval_or_reuse_per_item(DR, OR, RR, ChangedKeys, Views0),
            {[Old | Rest], Views1};
        false ->
            {NewAz, NewVal, NewDeps, Views1} = eval_one_v_flat(Def, Views0),
            {Rest, Views2} = eval_or_reuse_per_item(DR, OR, RR, ChangedKeys, Views1),
            {[{NewAz, NewVal, NewDeps} | Rest], Views2}
    end.

%% Renders stream items without view tracking. Returns `#{Key => ItemD}`.
%%
%% Not exported: SSR renders a stream each straight to output, like a plain list.
-spec render_stream_items_simple(Keys, ItemsMap, Template) -> ItemSnaps when
    Keys :: [term()],
    ItemsMap :: #{term() => term()},
    Template :: map(),
    ItemSnaps :: #{term() => [{arizona_template:az(), term(), map()}]}.
render_stream_items_simple(Keys, ItemsMap, Tmpl) ->
    render_stream_items_simple(Keys, ItemsMap, Tmpl, #{}).

-doc """
Renders list items with view tracking. Returns `{[ItemD], Views1}`.
""".
-spec render_list_items(Items, Template, Views) -> {ItemDs, Views1} when
    Items :: [term()],
    Template :: map(),
    Views :: {map(), map()},
    ItemDs :: [[{arizona_template:az(), term(), map()}]],
    Views1 :: {map(), map()}.
render_list_items(Items, #{d := DFun}, Views) ->
    render_list_items1(Items, DFun, Views).

%% Renders list items without view tracking. Returns `[ItemD]`.
%%
%% Not exported: the SSR path renders a plain-list each straight to output and keeps no
%% snapshot, so `eval_val/1` -- the live evaluator, whose snapshot the next diff reads
%% -- is the only caller left.
-spec render_list_items_simple(Items, Template) -> ItemDs when
    Items :: [term()],
    Template :: map(),
    ItemDs :: [[{arizona_template:az(), term(), map()}]].
render_list_items_simple(Items, #{d := DFun}) ->
    [[eval_one_triple(D) || D <- DFun(Item)] || Item <- Items].

%% Renders map entries without view tracking. Returns `[ItemD]`.
-spec render_map_items_simple(Map, Template) -> ItemDs when
    Map :: map(),
    Template :: map(),
    ItemDs :: [[{arizona_template:az(), term(), map()}]].
render_map_items_simple(Map, #{d := DFun}) ->
    [[eval_one_triple(D) || D <- DFun(K, V)] || K := V <- Map].

-doc """
Asserts that `Bindings` did not modify any framework-restricted keys
(currently just `id`) compared to the original `Props`.

Raises `restricted_key_modified` with handler info if a restricted key
was changed or dropped by the handler's mount callback.
""".
-spec check_restricted_keys(Bindings, Props, Handler) -> ok when
    Bindings :: map(),
    Props :: map(),
    Handler :: module().
check_restricted_keys(Bindings, Props, Handler) ->
    check_restricted_keys(Bindings, Props, Handler, mount).

-doc """
`check_restricted_keys/3` for bindings returned by `Callback` rather than by
`mount/1`: `Props` is then what the callback was handed (a child's props, or
the bindings the view held before the callback ran), and the error names it.
""".
-spec check_restricted_keys(Bindings, Props, Handler, Callback) -> ok when
    Bindings :: map(),
    Props :: map(),
    Handler :: module(),
    Callback ::
        mount | handle_update | handle_event | handle_info | handle_drain | default_patch_merge.
check_restricted_keys(Bindings, Props, Handler, Callback) ->
    [check_restricted_key(Key, Bindings, Props, Handler, Callback) || Key <- ?RESTRICTED_KEYS],
    ok.

-doc """
Returns the framework-owned binding keys. Mounted handlers cannot
change them (see `check_restricted_keys/3`); navigate-carry strips
them from the previous-route bindings before the new mount sees them,
since they're route-bound rather than session-level state.
""".
-spec restricted_keys() -> [atom()].
restricted_keys() ->
    ?RESTRICTED_KEYS.

-doc """
Projects a views accumulator down to the `child_views` annotation stored on a
snapshot: the set of the view ids it rendered, with the view entries themselves
dropped (keeping them would pin every descendant's bindings and snapshot alive
inside its ancestors').

A SET rather than a list, because every consumer asks only "is this id in
there?" -- so the annotation is on the small side of the membership test and the
prune in `arizona_live`'s root-snapshot settle iterates the handful of ids on a
path to a change instead of scanning a page-sized `child_views` per container.
Nothing reads it in order.
""".
-spec child_view_set(Views) -> ChildViews when
    Views :: #{binary() => term()},
    ChildViews :: #{binary() => true}.
child_view_set(Views) ->
    #{Id => true || Id := _ <- Views}.

-doc """
Seeds the update-effects accumulator before a diff with the originating
callback's effects (e.g. the root event's). Child `handle_update/3`
callbacks fold onto this during the diff; `drain_update_effects/0` reads
the final list. Threading the accumulator this way avoids concatenating
child effects onto the originating ones at the call site.
""".
-spec set_update_effects(Effects) -> ok when Effects :: [arizona_stateful:effect()].
set_update_effects(Effects) ->
    erlang:put('$arizona_update_effects', Effects),
    ok.

-doc """
Drains the update-effects accumulator after the render/diff just performed
(the seed plus every child `handle_update/3`'s additions). The live process
calls this to ship them to the client. Returns `[]` if none accumulated.
""".
-spec drain_update_effects() -> [arizona_stateful:effect()].
drain_update_effects() ->
    case erlang:erase('$arizona_update_effects') of
        undefined -> [];
        Effects -> Effects
    end.

-doc """
Re-raises an exception from a template dynamic tagged with the dynamic's source
location, as `{arizona_loc, Loc, Reason}`, keeping its class and stacktrace.

Every evaluator of a located dynamic catches here -- the SSR walk, the live
mount and the diff alike -- so a crash names the template line whichever path
ran it. A reason already tagged by a nested dynamic is re-raised unchanged:
the deepest location is the one closest to the failure.
""".
-spec relocate(Class, Reason, Stacktrace, Loc) -> no_return() when
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace(),
    Loc :: {module(), pos_integer()}.
relocate(Class, {arizona_loc, _, _} = Reason, ST, _Loc) ->
    erlang:raise(Class, Reason, ST);
relocate(Class, Reason, ST, Loc) ->
    erlang:raise(Class, {arizona_loc, Loc, Reason}, ST).

check_restricted_key(Key, Bindings, Props, Handler, Callback) ->
    case Props of
        #{Key := Expected} ->
            case Bindings of
                #{Key := Expected} ->
                    ok;
                #{} ->
                    %% Dropping the key is a modification too.
                    error(restricted_key_modified, [Key, Handler, Callback], [
                        {error_info, #{module => ?MODULE}}
                    ])
            end;
        #{} ->
            ok
    end.

-doc """
Formats `restricted_key_modified` errors into a human-readable message
naming the offending handler, callback and key.
""".
-spec format_error(Reason, StackTrace) -> ErrorInfo when
    Reason :: term(),
    StackTrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(restricted_key_modified, [{_M, _F, [Key, Handler, default_patch_merge], _Info} | _]) ->
    #{
        general => io_lib:format(
            "~s has no handle_update/3, so the patch params were merged into its "
            "bindings, changing or dropping restricted key '~s'. This key is owned by "
            "the framework: a patch must not carry another value for it.",
            [Handler, Key]
        )
    };
format_error(restricted_key_modified, [{_M, _F, [Key, Handler, Callback], _Info} | _]) ->
    #{
        general => io_lib:format(
            "~s's ~s callback changed or dropped restricted key '~s'. "
            "This key is owned by the framework: every callback must return it unchanged.",
            [Handler, Callback, Key]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec eval_one(Dynamic) -> {arizona_template:az(), term()} when
    Dynamic :: arizona_template:dynamic().
eval_one({Az, _Spec, _Loc} = Dyn) ->
    {Az, dyn_value(Dyn)};
eval_one({Az, _Spec} = Dyn) ->
    {Az, dyn_value(Dyn)}.

%% Like eval_one/1 but builds the 3-tuple `{Az, Val, #{}}` directly, for the
%% per-item snapshot paths (render_list_items_simple, render_map_items_simple,
%% render_stream_item_simple), which track no deps, so the empty map is shared.
-spec eval_one_triple(Dynamic) -> {arizona_template:az(), term(), #{}} when
    Dynamic :: arizona_template:dynamic().
eval_one_triple({Az, _Spec, _Loc} = Dyn) ->
    {Az, dyn_value(Dyn), #{}};
eval_one_triple({Az, _Spec} = Dyn) ->
    {Az, dyn_value(Dyn), #{}}.

%% The value a dynamic evaluates to; eval_one/1 and eval_one_triple/1 add the address.
-spec dyn_value(Dynamic) -> term() when
    Dynamic :: arizona_template:dynamic().
dyn_value({_Az, {attr, Name, Fun}, Loc}) when is_function(Fun, 0) ->
    try
        {attr, Name, attr_val(Name, eval_val(Fun()))}
    catch
        Class:Reason:ST -> relocate(Class, Reason, ST, Loc)
    end;
dyn_value({Az, Spec, Loc}) ->
    try
        arizona_template:scope_slot(Az, eval_val(Spec))
    catch
        Class:Reason:ST -> relocate(Class, Reason, ST, Loc)
    end;
dyn_value({_Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    {attr, Name, attr_val(Name, eval_val(Fun()))};
dyn_value({Az, Spec}) ->
    arizona_template:scope_slot(Az, eval_val(Spec)).

eval_val({esc, Fun}) when is_function(Fun, 0) ->
    arizona_template:mark_esc(eval_val(Fun()));
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
eval_val(#{callback := _, props := _} = Desc) ->
    %% A stateless child evaluates to a plain snapshot map, structurally like an
    %% inline nested template. The enclosing eval_one (which knows the slot az)
    %% namespaces its inner az ids via scope_slot -- no wrapper tag needed.
    eval_val(arizona_template:stateless_template(Desc));
eval_val(#{s := Statics, d := Dynamics} = Tmpl) ->
    Snap0 = #{s => Statics, d => eval_dynamics(Dynamics)},
    arizona_template:maybe_propagate(Tmpl, Snap0);
eval_val(#{az_local := _, target := {attr, Name}, v := V} = Local) ->
    ok = observe_command(Name, V),
    Local;
eval_val(Val) ->
    Val.

%% A dynamic attribute whose value evaluates to a command is the render-time
%% proof `arizona_event_attrs` needs for a name compile time could not see.
%% Observing it here, where every live render, mount and diff evaluates it,
%% declares exactly what the client will receive without building the document;
%% a stream item outside the visible window is never evaluated, so never observed.
%% A `?local` attribute's init is observed the same way (eval_val/1, eval_val_v/2).
attr_val(Name, Val) ->
    ok = observe_command(Name, Val),
    Val.

observe_command(Name, Val) ->
    case arizona_template:classify_trusted(Val) of
        {effect, _Cmds} -> arizona_event_attrs:observe_attr(Name);
        _NotACommand -> ok
    end.

eval_one_v({Az, {attr, Name, Fun}, Loc}, Views) when is_function(Fun, 0) ->
    Outer = open_deps(),
    Val =
        try
            attr_val(Name, eval_val(Fun()))
        catch
            Class:Reason:ST -> relocate(Class, Reason, ST, Loc, Outer)
        end,
    Deps = close_deps(Outer),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec, Loc}, Views0) ->
    Outer = open_deps(),
    {Val, Views1} =
        try
            eval_val_v(Spec, Views0)
        catch
            Class:Reason:ST -> relocate(Class, Reason, ST, Loc, Outer)
        end,
    Deps = close_deps(Outer),
    {{Az, arizona_template:scope_slot(Az, Val), Deps}, Views1};
eval_one_v({Az, {attr, Name, Fun}}, Views) when is_function(Fun, 0) ->
    Outer = open_deps(),
    Val =
        try
            attr_val(Name, eval_val(Fun()))
        catch
            Class:Reason:ST -> abort_deps(Class, Reason, ST, Outer)
        end,
    Deps = close_deps(Outer),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec}, Views0) ->
    Outer = open_deps(),
    {Val, Views1} =
        try
            eval_val_v(Spec, Views0)
        catch
            Class:Reason:ST -> abort_deps(Class, Reason, ST, Outer)
        end,
    Deps = close_deps(Outer),
    {{Az, arizona_template:scope_slot(Az, Val), Deps}, Views1}.

%% Opens a dependency scope for `arizona_template:track/1` to fill, answering the
%% enclosing one (`undefined` when none is open) for close_deps/1 to restore.
%% Every opener restores the enclosing scope on an exception too (abort_deps/4):
%% a process that catches a render crash and carries on (a keep-alive connection,
%% a static-generation batch) would otherwise keep a scope open that every later
%% read outside a slot records into.
open_deps() ->
    erlang:put('$arizona_deps', #{}).

%% Restores the scope enclosing a failed one, dropping its reads, and re-raises.
abort_deps(Class, Reason, ST, Outer) ->
    _FailedDeps = restore_deps(Outer),
    erlang:raise(Class, Reason, ST).

%% relocate/4 for a dynamic that opened a dependency scope.
relocate(Class, Reason, ST, Loc, Outer) ->
    _FailedDeps = restore_deps(Outer),
    relocate(Class, Reason, ST, Loc).

restore_deps(undefined) ->
    erlang:erase('$arizona_deps');
restore_deps(Outer) ->
    erlang:put('$arizona_deps', Outer).

%% Closes the scope open_deps/0 opened and answers its reads, handing them to the
%% enclosing scope as well. Only an each's items evaluate inside another dynamic's
%% scope (with_saved_deps/1 suspends it everywhere else), and most item slots read
%% nothing tracked, hence the empty case.
close_deps(undefined) ->
    erlang:erase('$arizona_deps');
close_deps(Outer) ->
    case erlang:put('$arizona_deps', Outer) of
        Deps when map_size(Deps) =:= 0 ->
            Deps;
        Deps ->
            erlang:put('$arizona_deps', maps:merge(Outer, Deps)),
            Deps
    end.

eval_val_v({esc, Fun}, Views0) when is_function(Fun, 0) ->
    {V, Views1} = eval_val_v(Fun(), Views0),
    {arizona_template:mark_esc(V), Views1};
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
eval_val_v(#{callback := _, props := _} = Desc, Views) ->
    %% Bracket the whole stateless recursion so eager ?get calls inside the
    %% callback body don't leak into the outer dynamic's tracked deps. The child
    %% evaluates to a plain snapshot map; the enclosing dynamic (which knows the
    %% slot az) namespaces its inner az ids via scope_slot -- otherwise two
    %% renders of the same function share one diff target.
    with_saved_deps(fun() ->
        eval_val_v(arizona_template:stateless_template(Desc), Views)
    end);
eval_val_v(#{s := _, d := _} = Tmpl, Views) ->
    eval_template(Tmpl, Views);
eval_val_v(#{az_local := _, target := {attr, Name}, v := V} = Local, Views) ->
    ok = observe_command(Name, V),
    {Local, Views};
eval_val_v(Val, Views) ->
    {Val, Views}.

%% Run Fun with the enclosing dependency scope suspended: nothing Fun reads
%% reaches it, and it is restored when Fun returns. An exception needs no restore
%% here: a suspended scope was opened by a frame still on the stack, which puts
%% back its own enclosing scope as the exception passes (abort_deps/4).
with_saved_deps(Fun) ->
    case erlang:erase('$arizona_deps') of
        undefined ->
            Fun();
        SavedDeps ->
            Result = Fun(),
            erlang:put('$arizona_deps', SavedDeps),
            Result
    end.

%% A child view whose own root is az-nodiff, or that sits inside an az-nodiff
%% az-nodiff element, takes its props at mount only: re-running it would call
%% `handle_update/3` and move its state past a DOM no op will ever touch, wherever
%% the descriptor is evaluated from (a slot, a branch, a `?stateless` body, a list
%% item). The same handler at the same id keeps its view as it is.
eval_stateful(H, Props, {Old, _New} = Views) ->
    Id = maps:get(id, Props),
    case Old of
        #{Id := #{handler := H, snapshot := #{diff := false} = Snap}} ->
            {Snap, arizona_diff:carry_skipped_view(Snap, Views)};
        #{Id := #{handler := H, snapshot := Snap}} ->
            case erlang:get('$arizona_in_nodiff') of
                true -> {Snap, arizona_diff:carry_skipped_view(Snap, Views)};
                undefined -> eval_stateful_lifecycle(H, Props, Id, Views)
            end;
        #{} ->
            eval_stateful_lifecycle(H, Props, Id, Views)
    end.

eval_stateful_lifecycle(H, Props, Id, {Old, New}) ->
    %% Bracket the whole lifecycle. mount/1 and handle_update/3 callbacks
    %% are user code; their ?get calls would otherwise land in the outer
    %% dynamic's tracked deps.
    with_saved_deps(fun() ->
        %% Thread the update-effects accumulator through this child's
        %% handle_update (if it has one) and store it back for grandchildren
        %% and the post-diff drain. with_saved_deps/1 restores only
        %% $arizona_deps, so this key accumulates uninterrupted.
        Effects0 = current_update_effects(),
        {B0, Resets, Effects1} = mount_or_update_stateful(H, Props, Id, Old, Effects0),
        ok = set_update_effects(Effects1),
        %% Clear the child's queued stream ops BEFORE rendering, so the snapshot
        %% built below and the bindings stored beside it hold the SAME stream.
        %% (The eval path renders the whole current state rather than draining
        %% the queue, so the ops are consumed either way -- this mirrors what
        %% arizona_live does on the root and child-event paths, without which a
        %% child whose stream is fed by parent props accumulates one queue entry
        %% per ROOT update for the lifetime of the process.) Clearing AFTER the
        %% render instead left the two disagreeing purely about the queue, so the
        %% next eval's source comparison differed and re-rendered the whole
        %% container on ticks where nothing had changed.
        B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
        Tmpl = arizona_stateful:call_render(H, B1),
        %% Evaluate this child's dynamics against a FRESH accumulator (mirroring
        %% eval_template/eval_each): whatever lands in LocalNew is exactly its
        %% transitive descendant subtree (grandchildren and deeper), read in
        %% O(subtree) -- scanning the whole accumulated views map per child would
        %% make N sibling children O(N^2). Record it on the snapshot so a skipped
        %% root diff (carry_skipped_view) and a child's own event
        %% (arizona_live:process_child_change) can carry/reconcile the WHOLE
        %% subtree, not just this child -- otherwise a depth-2 grandchild is
        %% dropped from the views map and unmounted/reset.
        {ChildTriples, {Old, LocalNew}} = eval_dynamics_v(maps:get(d, Tmpl), {Old, #{}}),
        {ChildD, ChildDeps} = arizona_template:split_triples(ChildTriples),
        Descendants = child_view_set(LocalNew),
        Snap0 = arizona_template:make_child_snap(Tmpl, ChildD, ChildDeps, Id),
        Snap = Snap0#{child_views => Descendants},
        %% The eval above rendered the child's whole current state, so its queued
        %% stream ops are consumed -- clear them, mirroring what arizona_live
        %% does on the root and child-event paths. Without this a child whose
        %% stream is fed by parent props accumulates one queue entry per ROOT
        %% update for the lifetime of the process.
        B2 = maps:merge(B1, Resets),
        ChildEntry = #{handler => H, bindings => B2, snapshot => Snap},
        New1 = maps:merge(New, LocalNew),
        {Snap, {Old, New1#{Id => ChildEntry}}}
    end).

%% Returns {Bindings, Resets, Effects1}: the update path threads Effects0 through
%% the child's handle_update; the fresh-mount paths (mount/1 has no effects
%% channel) pass Effects0 through unchanged.
mount_or_update_stateful(H, Props, Id, Old, Effects0) ->
    case Old of
        #{Id := #{handler := H, bindings := B}} ->
            {B1, Resets, Effects1} = arizona_stateful:call_handle_update(H, Props, B, Effects0),
            ok = check_restricted_keys(B1, Props, H, handle_update),
            {B1, Resets, Effects1};
        #{Id := #{handler := OldH, bindings := OldB}} ->
            %% Same id, different handler: tear the old instance down before
            %% mounting the new one so it can release resources. Its timers and
            %% queued messages carry the id, not the handler, so they must die
            %% here too or they reach the new handler's handle_info/2.
            ok = arizona_live:swap_out_view(Id, OldH, OldB),
            {B1, Resets} = fresh_mount_stateful(H, Props),
            {B1, Resets, Effects0};
        #{} ->
            {B1, Resets} = fresh_mount_stateful(H, Props),
            {B1, Resets, Effects0}
    end.

fresh_mount_stateful(H, Props) ->
    {B1, Resets} = arizona_stateful:call_mount(H, Props),
    ok = check_restricted_keys(B1, Props, H),
    {B1, Resets}.

%% Reads the update-effects accumulator threaded through the current
%% render/diff (default [] when unset, e.g. an initial mount).
current_update_effects() ->
    case erlang:get('$arizona_update_effects') of
        undefined -> [];
        Effects -> Effects
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

%% Renders map entries with view tracking. Returns `{[ItemD], Views1}`.
%% Each map entry is passed to the template's `d` callback as `(Key, Value)`.
render_map_items(Map, #{d := DFun}, Views) ->
    %% Fold prepends, so the accumulator ends up reversed relative to the map's
    %% iteration order -- reverse it back so the tracked/diff path renders entries
    %% in the SAME order as the untracked path (`render_map_items_simple`) and SSR
    %% (`arizona_render`), both a `K := V <-` comprehension over the same iteration
    %% order. Without this the live snapshot disagrees with the SSR DOM item-for-item.
    {Acc, Views1} = maps:fold(
        fun(K, V, {Acc0, V0}) ->
            Dynamics = DFun(K, V),
            {Triples, V1} = eval_dynamics_v(Dynamics, V0),
            {[Triples | Acc0], V1}
        end,
        {[], Views},
        Map
    ),
    {lists:reverse(Acc), Views1}.

eval_each(RenderFun, Tmpl, {Old, New0}, StreamExtra) ->
    {{ItemSnaps, {_, LocalNew}}, ItemDeps} = eval_items(Tmpl, fun() -> RenderFun(Old) end),
    ChildViews = child_view_set(LocalNew),
    Snap = build_each_snap(ItemSnaps, Tmpl, ChildViews, ItemDeps, StreamExtra),
    {Snap, {Old, maps:merge(New0, LocalNew)}}.

build_each_snap(ItemSnaps, Tmpl, ChildViews, _ItemDeps, undefined) ->
    #{t => ?EACH, items => ItemSnaps, template => Tmpl, child_views => ChildViews};
build_each_snap(ItemSnaps, Tmpl, ChildViews, ItemDeps, {VKeys, Source}) ->
    #{
        %% A stream diff renders only the items its queued ops touch, so it cannot
        %% learn from the render what the others read; it keeps their reads here to
        %% tell when an outer binding change must reconcile every visible item.
        item_deps => ItemDeps,
        t => ?EACH,
        items => ItemSnaps,
        order => VKeys,
        source => Source,
        template => Tmpl,
        child_views => ChildViews,
        %% This eval rendered the stream's whole post-op state, so every op
        %% queued so far counts as consumed. A nested stream's snapshot comes
        %% from here (the diff discards `diff_stream`'s own), so this is what
        %% stops the next re-drain from replaying the history.
        drained => arizona_stream:drain_mark(Source)
    }.

eval_template(#{d := Dynamics, diff := false} = Tmpl, {Old, _New0} = Views) ->
    Evaluated = with_saved_deps(fun() -> eval_nodiff_dynamics(Dynamics, {Old, #{}}) end),
    template_snapshot(Tmpl, Evaluated, Views);
eval_template(#{d := Dynamics} = Tmpl, {Old, _New0} = Views) ->
    Evaluated = with_saved_deps(fun() -> eval_dynamics_v(Dynamics, {Old, #{}}) end),
    template_snapshot(Tmpl, Evaluated, Views).

template_snapshot(#{s := Statics} = Tmpl, {Triples, {_, LocalNew}}, {Old, New0}) ->
    {D, DepsList} = arizona_template:split_triples(Triples),
    ChildViews = child_view_set(LocalNew),
    Snap0 = #{s => Statics, d => D, deps => DepsList},
    Snap1 = arizona_template:maybe_propagate(Tmpl, Snap0),
    {Snap1#{child_views => ChildViews}, {Old, maps:merge(New0, LocalNew)}}.

%% Mark the evaluation of an az-nodiff template's dynamics for eval_stateful/3.
eval_nodiff_dynamics(Dynamics, Views) ->
    case erlang:put('$arizona_in_nodiff', true) of
        true ->
            eval_dynamics_v(Dynamics, Views);
        undefined ->
            try
                eval_dynamics_v(Dynamics, Views)
            after
                erlang:erase('$arizona_in_nodiff')
            end
    end.

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
    [eval_one_triple(D) || D <- DFun(Item, Key)].

render_list_items1([], _DFun, Views) ->
    {[], Views};
render_list_items1([Item | Rest], DFun, Views0) ->
    Dynamics = DFun(Item),
    {Triples, Views1} = eval_dynamics_v(Dynamics, Views0),
    {RestD, Views2} = render_list_items1(Rest, DFun, Views1),
    {[Triples | RestD], Views2}.

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
    %% eval_val reduces the descriptor to a plain snapshot map (no wrapper tag);
    %% the enclosing eval_one/eval_one_v then namespaces its az ids by the slot
    %% via scope_slot, exactly like an inline nested template.
    Result = eval_val(#{callback => Cb, props => #{t => ~"hi"}}),
    ?assertMatch(
        #{s := [~"<b>", ~"</b>"], d := [{~"0", ~"hi"}], f := ~"x"}, Result
    ).

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
            [{~"0", Key, _}, {~"1", Val, _}] = Item,
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
    [[{~"0", ~"x", _}, {~"1", ~"1", _}]] = render_map_items_simple(#{~"x" => ~"1"}, Tmpl).

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
    StackFrame = [{arizona_eval, check, [id, my_handler, mount], []}],
    #{general := Msg} = format_error(restricted_key_modified, StackFrame),
    MsgBin = iolist_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(MsgBin, ~"my_handler's mount callback")),
    ?assertNotEqual(nomatch, binary:match(MsgBin, ~"'id'")).

restricted_keys_returns_id_test() ->
    %% Pins the public contract so future macro changes are deliberate.
    %% The list is consumed by `arizona_live`'s navigate handler to strip
    %% route-bound keys from the carried bindings.
    ?assertEqual([id], restricted_keys()).

-endif.
