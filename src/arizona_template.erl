-module(arizona_template).
-moduledoc """
Runtime template API: bindings, descriptors, value rendering, and snapshots.

Most of this module's surface is exposed through the macros in
`include/arizona_common.hrl` (`?get`, `?html`, `?each`, `?stateful`,
`?stateless`). Templates themselves are produced by `arizona_parse_transform`
at compile time -- `html/1` is only a runtime stub that errors if the parse
transform was not applied.

## Concepts

- **Bindings** -- the map passed to `render/1`. `get/2` and `get/3` access
  bindings while tracking which keys were read, so the differ knows which
  dynamics depend on which bindings.
- **Templates** -- compile-time maps `#{s := Statics, d := Dynamics, f := Fp}`
  emitted by the parse transform.
- **Descriptors** -- lightweight tuples returned by `stateful/2` and
  `stateless/2,3` that tell the renderer how to mount a child component.
- **Snapshots** -- cached `#{s, d, deps, ...}` maps used by the differ to
  detect what changed between renders.

## Example

```erlang
render(Bindings) ->
    ?html({'div', [{class, ?get(theme)}], [?get(name)]}).
```
""".

-compile({nowarn_redefined_builtin_type, [{dynamic, 0}]}).

-include("arizona.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get/2]).
-export([get/3]).
-export([get_lazy/3]).
-export([track/1]).
-export([html/1]).
-export([stateful/2]).
-export([stateless/2]).
-export([stateless/3]).
-export([each/2]).
-export([to_bin/1]).
-export([dyn_az/1]).
-export([format_error/1]).
-export([format_error/2]).
-export([unwrap_val/1]).
-export([render_attr/2]).
-export([maybe_propagate/2]).
-export([maybe_put_fingerprint/2]).
-export([make_child_snap/4]).
-export([unzip_triples/1]).
-export([split_triples/1]).
-export([visible_keys/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/1]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([az/0]).
-export_type([bindings/0]).
-export_type([deps/0]).
-export_type([loc/0]).
-export_type([dynamic/0]).
-export_type([template/0]).
-export_type([each_template/0]).
-export_type([each_container/0]).
-export_type([snapshot/0]).
-export_type([stateful_descriptor/0]).
-export_type([stateless_descriptor/0]).
-export_type([render_fun/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal az() :: binary() | undefined.
-type bindings() :: map().
-nominal deps() :: #{term() => true}.
-nominal loc() :: {module(), pos_integer()}.

-nominal dynamic() ::
    {az(), fun(() -> term()), loc()}
    | {az(), {attr, binary(), fun(() -> term())}, loc()}
    | {az(), template(), loc()}
    | {az(), term(), loc()}
    | {az(), fun(() -> term())}
    | {az(), {attr, binary(), fun(() -> term())}}
    | {az(), template()}
    | {az(), term()}.

-nominal template() :: #{s := [binary()], d := [dynamic()], f := binary(), diff => false}.

-nominal each_template() :: #{
    t := 0,
    s := [binary()],
    d := fun((term()) -> [dynamic()]) | fun((term(), term()) -> [dynamic()]),
    f := binary()
}.

-nominal each_container() :: #{
    t := 0,
    source := term(),
    template := each_template()
}.

-nominal snapshot() :: #{
    s := [binary()],
    d := [{az(), term()}],
    f => binary(),
    deps => [deps()],
    diff => false,
    view_id => binary()
}.

-nominal stateful_descriptor() :: #{stateful := module(), props := map()}.
-nominal stateless_descriptor() :: #{callback := fun((map()) -> template()), props := map()}.

-nominal render_fun() :: fun((bindings()) -> template()).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Reads `Key` from `Bindings` and tracks the access for diff dependency analysis.

Errors with `missing_binding` (carrying an `error_info` annotation that
routes through `format_error/2`) if not present. Use `get/3` for a default.
""".
-spec get(Key, Bindings) -> Value when
    Key :: term(),
    Bindings :: map(),
    Value :: term().
get(Key, Bindings) ->
    track(Key),
    case Bindings of
        #{Key := Val} ->
            Val;
        #{} ->
            erlang:error(missing_binding, [Key, Bindings], [
                {error_info, #{module => ?MODULE}}
            ])
    end.

-doc """
Reads `Key` from `Bindings`, returning `Default` if absent. Tracks the access.
""".
-spec get(Key, Bindings, Default) -> Value when
    Key :: term(),
    Bindings :: map(),
    Default :: term(),
    Value :: term().
get(Key, Bindings, Default) ->
    track(Key),
    maps:get(Key, Bindings, Default).

-doc """
Like `get/3` but the default is computed lazily by a 0-arity fun.
""".
-spec get_lazy(Key, Bindings, DefaultFun) -> Value when
    Key :: term(),
    Bindings :: map(),
    DefaultFun :: fun(() -> term()),
    Value :: term().
get_lazy(Key, Bindings, DefaultFun) when is_function(DefaultFun, 0) ->
    track(Key),
    case Bindings of
        #{Key := Val} -> Val;
        #{} -> DefaultFun()
    end.

-doc """
Records `Key` as a dependency of the dynamic element currently being rendered.

Called automatically by `get/2,3` and `get_lazy/3`. Public so render code
can attribute dependencies that bypass binding access.
""".
-spec track(Key) -> ok when
    Key :: term().
track(Key) ->
    case erlang:get('$arizona_deps') of
        undefined ->
            ok;
        Deps ->
            erlang:put('$arizona_deps', Deps#{Key => true}),
            ok
    end.

-doc """
Compile-time stub. The parse transform replaces every `?html(...)` (and
`arizona_template:html/1`) call with a precomputed `t:template/0` map. If
this function runs, the parse transform was not applied -- include
`arizona_stateful.hrl` or `arizona_stateless.hrl`.
""".
-spec html(term()) -> no_return().
html(_Elems) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Builds a stateful child descriptor. The renderer mounts `Handler` with `Props`.
""".
-spec stateful(Handler, Props) -> stateful_descriptor() when
    Handler :: module(),
    Props :: map().
stateful(Handler, Props) when is_atom(Handler), is_map(Props) ->
    #{stateful => Handler, props => Props}.

-doc """
Builds a stateless child descriptor from a 1-arity render fun.
""".
-spec stateless(Callback, Props) -> stateless_descriptor() when
    Callback :: fun((map()) -> template()),
    Props :: map().
stateless(Callback, Props) when is_function(Callback, 1), is_map(Props) ->
    #{callback => Callback, props => Props}.

-doc """
Builds a stateless child descriptor from a `Handler:Fun/1` reference.
""".
-spec stateless(Handler, Fun, Props) -> stateless_descriptor() when
    Handler :: module(),
    Fun :: atom(),
    Props :: map().
stateless(Handler, Fun, Props) when is_atom(Handler), is_atom(Fun), is_map(Props) ->
    #{callback => fun Handler:Fun/1, props => Props}.

-doc """
Wraps a `t:template/0` produced by the parse transform with an `each` source.

Called by the parse transform to compile `?each(Fun, Source)` -- pairs
the per-item template with its source list, stream, or map.
""".
-spec each(Source, Template) -> each_container() when
    Source :: term(),
    Template :: each_template().
each(Source, #{t := ?EACH, d := DFun} = Tmpl) when is_function(DFun, 1); is_function(DFun, 2) ->
    #{t => ?EACH, source => Source, template => Tmpl}.

-doc """
Converts a template value to its binary HTML representation.

Errors with `{bad_template_value, V}` for unsupported types.
""".
-spec to_bin(Value) -> binary() when
    Value :: term().
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
to_bin(V) when is_atom(V) -> atom_to_binary(V);
to_bin({arizona_js, _} = Cmd) ->
    arizona_js:encode(Cmd);
to_bin([{arizona_js, _} | _] = Cmds) ->
    arizona_js:encode(Cmds);
to_bin(V) when is_list(V) -> iolist_to_binary(V);
to_bin(V) ->
    erlang:error({bad_template_value, V}, [V], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Extracts the `t:az/0` index from a 2- or 3-tuple `t:dynamic/0`.
""".
-spec dyn_az(Dynamic) -> az() when
    Dynamic :: dynamic().
dyn_az({Az, _}) -> Az;
dyn_az({Az, _, _}) -> Az.

-doc """
Formats compile/runtime error reasons emitted by this module.
""".
-spec format_error(Reason) -> string() when
    Reason :: term().
format_error(parse_transform_not_applied) ->
    "parse transform not applied -- include arizona_stateful.hrl or arizona_stateless.hrl";
format_error({bad_template_value, V}) ->
    lists:flatten(io_lib:format("cannot convert ~0tp to binary in template", [V])).

-doc """
Formats runtime errors raised with an `error_info` annotation pointing at
this module. Picked up by `erl_error:format_exception/3`.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(missing_binding, [{_M, _F, [Key, Bindings], _Info} | _]) ->
    Available = lists:sort(maps:keys(Bindings)),
    Suggestion =
        case arizona_error:closest(Key, Available) of
            undefined -> "";
            Match -> io_lib:format(" Did you mean ~0tp?", [Match])
        end,
    #{
        general => io_lib:format(
            "binding ~0tp not found.~s Available bindings: ~0tp. "
            "Use arizona_template:get/3 (or ?get/2) with a default "
            "to make the binding optional.",
            [Key, Suggestion, Available]
        )
    };
format_error({bad_template_value, _V} = Reason, _ST) ->
    #{general => format_error(Reason)};
format_error(parse_transform_not_applied = Reason, _ST) ->
    #{general => format_error(Reason)}.

-doc """
Materializes an attribute dynamic into its rendered binary, leaving plain
values untouched.
""".
-spec unwrap_val(Value) -> term() when
    Value :: term().
unwrap_val({attr, Name, V}) -> render_attr(Name, V);
unwrap_val(V) -> V.

-doc """
Renders one HTML attribute, handling boolean true/false specially.

`false` strips the attribute, `true` emits a bare `name`, anything else
becomes `name="value"`.
""".
-spec render_attr(Name, Value) -> binary() when
    Name :: binary(),
    Value :: term().
render_attr(_Name, false) -> <<>>;
render_attr(Name, true) -> <<" ", Name/binary>>;
render_attr(Name, V) -> <<" ", Name/binary, "=\"", (to_bin(V))/binary, "\"">>.

-doc """
Propagates `f` (fingerprint) and the optional `diff => false` flag from a
template into a snapshot.
""".
-spec maybe_propagate(Template, Snapshot) -> Snapshot1 when
    Template :: template(),
    Snapshot :: snapshot(),
    Snapshot1 :: snapshot().
maybe_propagate(Tmpl, Snap) ->
    Snap1 =
        case Tmpl of
            #{diff := false} -> Snap#{diff => false};
            #{} -> Snap
        end,
    maybe_put_fingerprint(Tmpl, Snap1).

-doc """
Copies the `f` field from a template to a snapshot if present.
""".
-spec maybe_put_fingerprint(Template, Snapshot) -> Snapshot1 when
    Template :: template(),
    Snapshot :: snapshot(),
    Snapshot1 :: snapshot().
maybe_put_fingerprint(#{f := F}, Snap) -> Snap#{f => F};
maybe_put_fingerprint(#{}, Snap) -> Snap.

-doc """
Builds a stateful child snapshot, propagating `diff => false` from the template.
""".
-spec make_child_snap(Template, ChildD, ChildDeps, Id) -> snapshot() when
    Template :: template(),
    ChildD :: [{az(), term()}],
    ChildDeps :: [deps()],
    Id :: binary().
make_child_snap(Tmpl, ChildD, ChildDeps, Id) ->
    #{s := S} = Tmpl,
    Snap = #{s => S, d => ChildD, deps => ChildDeps, view_id => Id},
    maybe_propagate(Tmpl, Snap).

-doc """
Splits a list of `{Az, Val, Deps}` triples into the snapshot d-list, deps list,
and rendered values list.
""".
-spec unzip_triples(Triples) -> {DList, DepsList, Vals} when
    Triples :: [{az(), term(), deps()}],
    DList :: [{az(), term()}],
    DepsList :: [deps()],
    Vals :: [term()].
unzip_triples([]) ->
    {[], [], []};
unzip_triples([{Az, Val, Deps} | Rest]) ->
    {RestD, RestDeps, RestVals} = unzip_triples(Rest),
    {[{Az, Val} | RestD], [Deps | RestDeps], [unwrap_val(Val) | RestVals]}.

-doc """
Like `unzip_triples/1` but discards the rendered values list.
""".
-spec split_triples(Triples) -> {DList, DepsList} when
    Triples :: [{az(), term(), deps()}],
    DList :: [{az(), term()}],
    DepsList :: [deps()].
split_triples([]) ->
    {[], []};
split_triples([{Az, Val, Deps} | Rest]) ->
    {RestD, RestDeps} = split_triples(Rest),
    {[{Az, Val} | RestD], [Deps | RestDeps]}.

-doc """
Returns the visible portion of a stream order buffer given a `Limit`.

The order is the `arizona_stream` record's `{Front, BackRev}` 2-list
buffer (Back holds recently-appended keys NEWEST-first). This function
flushes the buffer to a flat oldest-first list and applies the limit.
`infinity` returns the full order; an integer truncates via
`lists:sublist/2`.
""".
-spec visible_keys(Order, Limit) -> Order1 when
    Order :: {[term()], [term()]},
    Limit :: pos_integer() | infinity,
    Order1 :: [term()].
visible_keys({Front, []}, infinity) -> Front;
visible_keys({Front, Back}, infinity) -> Front ++ lists:reverse(Back);
visible_keys({Front, []}, Limit) -> lists:sublist(Front, Limit);
visible_keys({Front, Back}, Limit) -> lists:sublist(Front ++ lists:reverse(Back), Limit).
