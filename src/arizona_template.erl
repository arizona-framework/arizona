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

%% The html/native/terminal (+ _each) stubs are intentionally identical: each
%% raises parse_transform_not_applied inline so the raising stub is the top stack
%% frame, pointing the user at the exact un-transformed call. A shared helper would
%% move that frame and obscure which call was made, so dedup does not apply here.
%% This module is the template API surface (construction stubs for every target
%% plus binding/descriptor helpers), so it legitimately exceeds the god-module
%% function count.
-elvis([
    {elvis_style, dont_repeat_yourself, disable},
    {elvis_style, no_god_modules, disable}
]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get/2]).
-export([get/3]).
-export([get_lazy/3]).
-export([with/2]).
-export([track/1]).
-export([html/1]).
-export([native/1]).
-export([terminal/1]).
-export([stateful/2]).
-export([stateless/2]).
-export([stateless/3]).
-export([local/2]).
-export([each/2]).
-export([native_each/2]).
-export([terminal_each/2]).
-export([foreign_each/2]).
-export([to_bin/1]).
-export([raw/1]).
-export([classify_trusted/1]).
-export([escape_value/2]).
-export([escape_marked/2]).
-export([mark_esc/1]).
-export([dyn_az/1]).
-export([format_error/1]).
-export([format_error/2]).
-export([unwrap_val/2]).
-export([maybe_propagate/2]).
-export([make_child_snap/4]).
-export([scope_slot/2]).
-export([unzip_triples/2]).
-export([split_triples/1]).
-export([visible_keys/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/1]).
-ignore_xref([format_error/2]).
%% Internal each stubs: emitted by the parse transform's native/terminal pre-pass,
%% never called directly (no az alias, unlike each/2).
-ignore_xref([native_each/2]).
-ignore_xref([terminal_each/2]).
-ignore_xref([foreign_each/2]).
%% Public escape opt-out for template authors; no internal callers by design.
-ignore_xref([raw/1]).

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

-nominal template() :: #{
    s := [binary()],
    d := [dynamic()],
    f := binary(),
    diff => false,
    backend => module(),
    %% The `az-*` attribute names this template declares, recorded by the parse
    %% transform so the client can delegate exactly those DOM event types.
    az_names => [binary()]
}.

-nominal each_template() :: #{
    t := 0,
    s := [binary()],
    d := fun((term()) -> [dynamic()]) | fun((term(), term()) -> [dynamic()]),
    f := binary(),
    single_root => true,
    backend => module()
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
    view_id => binary(),
    backend => module(),
    az_names => [binary()]
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
Projects `Bindings` to a sub-map of only `Keys`, tracking each key as a
dependency.

Use it to hand a bindings subset to a sub-context (a helper, a child, a
passed-through map) while declaring exactly which keys it depends on: the
projection means the sub-context cannot silently read an untracked key (an
omitted key fails loudly with `missing_binding` rather than freezing), and the
tracking makes the enclosing slot re-render when any listed key changes.

Equivalent to `track/1` on each key followed by `maps:with(Keys, Bindings)`.
""".
-spec with(Keys, Bindings) -> Bindings1 when
    Keys :: [term()],
    Bindings :: map(),
    Bindings1 :: map().
with(Keys, Bindings) ->
    ok = lists:foreach(fun track/1, Keys),
    maps:with(Keys, Bindings).

-doc """
Records `Key` as a dependency of the dynamic element currently being rendered.

Called automatically by `get/2,3` and `get_lazy/3`. Public so render code
can attribute dependencies that bypass binding access.

It is a no-op outside a dynamic's evaluation, since that is where the
dependency scope is opened. **Do not read that as "a `?get` in the body of
`render/1` records nothing"** -- what a slot depends on is decided at *compile
time*, not here. The parse transform inlines a fresh read into the slot's own
closure, so the body-level call is the one that no-ops while the inlined one
records. `erlc -P` shows which reads ended up inside which closure, and is the
only reliable way to answer "why does this slot not update". See "The
invariant" in `docs/architecture.md`.
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
Compile-time stub for the native (JSON) render target. The parse transform
replaces every `?native(...)` (and `arizona_template:native/1`) call with a
precomputed `t:template/0` map whose statics are JSON fragments. If this
function runs, the parse transform was not applied.
""".
-spec native(term()) -> no_return().
native(_Elems) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Compile-time stub for the terminal (ANSI) render target. The parse transform
replaces every `?terminal(...)` (and `arizona_template:terminal/1`) call with a
precomputed `t:template/0` map whose statics are ANSI-decorated text. If this
function runs, the parse transform was not applied.
""".
-spec terminal(term()) -> no_return().
terminal(_Elems) ->
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
Builds a client-owned slot value. The server renders `Init` once and never
diffs the slot (`diff => false`); the browser owns the value via `Key`,
updating it client-side with no round-trip. Used via the `?local` macro in
content or attribute-value position. `Key` may be a binary or an atom (an atom
is normalized to a binary, so it reaches the client as the same string).
""".
-spec local(Key, Init) -> map() when
    Key :: binary() | atom(),
    Init :: term().
local(Key, Init) when is_atom(Key) ->
    local(atom_to_binary(Key), Init);
local(Key, Init) when is_binary(Key) ->
    #{diff => false, az_local => Key, v => Init}.

-doc """
Wraps a `t:template/0` produced by the parse transform with an `each` source.

Called by the parse transform to compile `?each(Fun, Source)` -- pairs
the per-item template with its source list, stream, or map.

`?each(Fun, Source)` expands to a call on this same name, which the parse
transform replaces with the compiled pairing above. Reached with the macro's
own `(Fun, Source)` argument order, the transform did not run: say so with
`parse_transform_not_applied` rather than a bare `function_clause`.
""".
-spec each(Source, Template) -> each_container() when
    Source :: term(),
    Template :: each_template().
each(Source, #{t := ?EACH, d := DFun} = Tmpl) when is_function(DFun, 1); is_function(DFun, 2) ->
    #{t => ?EACH, source => Source, template => Tmpl};
each(Fun, _Source) when is_function(Fun, 1); is_function(Fun, 2) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Compile-time stub for the native each. The parse transform replaces every
`?native_each(Fun, Source)` call with a compiled each-container whose per-item
template carries JSON statics. If this function runs, the parse transform was
not applied.
""".
-spec native_each(term(), term()) -> no_return().
native_each(_Fun, _Source) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Compile-time stub for an each in a non-default content context (an `?each`
inside `<svg>`, whose per-item template must be compiled as foreign content).
The parse transform replaces every such call with a compiled each-container. If
this function runs, the parse transform was not applied.
""".
-spec foreign_each(term(), term()) -> no_return().
foreign_each(_Fun, _Source) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Compile-time stub for the terminal each. The parse transform replaces every
`?terminal_each(Fun, Source)` call with a compiled each-container whose per-item
template carries ANSI statics. If this function runs, the parse transform was
not applied.
""".
-spec terminal_each(term(), term()) -> no_return().
terminal_each(_Fun, _Source) ->
    erlang:error(parse_transform_not_applied, [], [
        {error_info, #{module => ?MODULE}}
    ]).

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
%% Escape markers are unwrapped to their raw bytes here. to_bin never escapes;
%% escaping happens at the render-output boundary. At SSR that boundary is
%% `arizona_render:render_dyn/2` (it calls `escape_value/2` through the backend);
%% on the live diff the boundary is the JS client, which text-nodes a scalar
%% `?OP_TEXT` value (so `<` is literal text, never parsed) -- so the diff path
%% stays raw end to end.
to_bin({arizona_esc, V}) ->
    to_bin(V);
to_bin(V0) ->
    case classify_trusted(V0) of
        {raw, V} ->
            to_bin(V);
        {effect, Cmd} ->
            arizona_effect:encode(Cmd);
        value when is_list(V0) ->
            %% A charlist/iodata is text: decode it as Unicode codepoints so the
            %% output is valid UTF-8. `iolist_to_binary/1` truncates a 128..255
            %% codepoint to an invalid latin-1 byte (`"café"` -> `<<...,233>>`
            %% instead of `<<...,195,169>>`) and crashes a codepoint > 255 with a
            %% bare `badarg`. A term that is not valid chardata (e.g. a list
            %% wrapping a binary of non-UTF-8 bytes) yields the same
            %% `bad_template_value` as any other unsupported value.
            case unicode:characters_to_binary(V0) of
                Bin when is_binary(Bin) ->
                    Bin;
                _ ->
                    erlang:error({bad_template_value, V0}, [V0], [
                        {error_info, #{module => ?MODULE}}
                    ])
            end;
        value ->
            erlang:error({bad_template_value, V0}, [V0], [
                {error_info, #{module => ?MODULE}}
            ])
    end.

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
        case arizona_error:did_you_mean(Key, Available) of
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
Materializes an attribute dynamic into its rendered binary through `Backend`
(an `arizona_renderer`), leaving plain values untouched.
""".
-spec unwrap_val(Backend, Value) -> term() when
    Backend :: module(),
    Value :: term().
unwrap_val(Backend, {attr, Name, V}) -> Backend:render_attr(Name, V);
unwrap_val(_Backend, V) -> V.

-doc """
Wraps a value to opt out of HTML auto-escaping. Use only for trusted,
already-safe HTML fragments -- never for user-controlled data.

This is an **HTML-target** feature: on the live diff a `?raw` value is tagged so the
client `innerHTML`s it (vs text-noding a plain `?get` value -- the escaping boundary).
The `?native` and `?terminal` targets do not HTML-escape, so `?raw` there is a no-op
with no defined meaning; using it in a `?native`/`?terminal` template is unsupported.
""".
-spec raw(Value) -> {arizona_raw, term()} when
    Value :: term().
raw(Value) -> {arizona_raw, Value}.

-doc """
Classifies the escape-exempt content shapes: a `raw/1` opt-out
(`{arizona_raw, V}`, returned unwrapped) and an already-encoded effect command
or command list (`{arizona_effect, _}`). Every other value is `value`.

The single definition of the shape vocabulary -- `to_bin/1`, `escape_value/2`,
`mark_esc/1`, and `arizona_html:render_attr/2` all dispatch on it, so growing
the vocabulary is a one-place change. Each call site decides the policy for a
shape (`mark_esc/1` deliberately unwraps and re-escapes a helper-produced raw).
""".
-spec classify_trusted(Value) -> {raw, term()} | {effect, Effect} | value when
    Value :: term(),
    Effect :: term().
classify_trusted({arizona_raw, V}) -> {raw, V};
classify_trusted({arizona_effect, _} = Cmd) -> {effect, Cmd};
classify_trusted([{arizona_effect, _} | _] = Cmds) -> {effect, Cmds};
classify_trusted(_Value) -> value.

-doc """
Renders a value to its output binary, escaping via `Backend` (an
`arizona_renderer`): HTML entity-escaping, terminal control-char sanitizing, or a
plain-text/JSON identity. `raw/1`-wrapped values and already-encoded
`arizona_effect` commands are classified out and pass through unescaped (the
`?raw` opt-out and effects are trusted, backend-independent).
""".
-spec escape_value(Backend, Value) -> binary() when
    Backend :: module(),
    Value :: term().
escape_value(Backend, V0) ->
    case classify_trusted(V0) of
        {raw, V} -> to_bin(V);
        {effect, Cmd} -> to_bin(Cmd);
        value -> Backend:escape(to_bin(V0))
    end.

-doc """
Escape the inner value of an `{arizona_esc, _}` marker.

The marker itself is the classification: `mark_esc/1` is its only producer and it
wraps only what already came back as a plain `value`, so re-asking
`classify_trusted/1` here can only ever get the same answer. Escapable content is
most of what a template renders, so that second ask is paid per value per render.
""".
-spec escape_marked(Backend, Value) -> binary() when
    Backend :: module(),
    Value :: term().
escape_marked(Backend, V) ->
    Backend:escape(to_bin(V)).

-doc """
Marks an evaluated value for HTML escaping at output. Only scalars are marked --
nested templates/descriptors and stateless-child snapshots (all maps) and
effects pass through untouched so they render structurally (their own inner
dynamics carry their own marks). A stateless descriptor returned from a
conditional content slot reduces to a bare snapshot map here, so the `is_map`
clause passes it through and `scope_slot/2` later namespaces it -- the same
treatment a directly-slotted stateless child gets.

`mark_esc/1` is reached only for an **escape-tagged** content slot (`{esc, Fun}`),
which the parse transform emits for every content interpolation that is not a
literal block -- a literal `raw/1` at the template site compiles to a bare
(un-esc-tagged) slot and never reaches here. So a `{arizona_raw, V}` arriving
here was produced by a **helper** returning `raw/1` into an escaping slot, which
the documented rule forbids ("the parse transform only recognizes the opt-out
when the `raw` call is literal at the template site"). Honor that rule: unwrap
the raw and escape its inner value like any plain scalar, so a helper wrapping
user data in `raw/1` cannot smuggle trusted markup past the escaper (on SSR or
across the live diff).
""".
-spec mark_esc(Value) -> term() when
    Value :: term().
mark_esc(V) when is_map(V) ->
    V;
mark_esc(V0) ->
    case classify_trusted(V0) of
        {raw, V} -> mark_esc(V);
        {effect, E} -> E;
        value -> {arizona_esc, V0}
    end.

-doc """
Propagates the template-level fields that must survive onto a snapshot: `f`
(fingerprint), the optional `diff => false` flag, the render `backend`, and the
`az_names` this template declares.

Used by both the render path (`arizona_render`) and the diff path
(`arizona_diff`), so a diffed snapshot carries the same backend as a freshly
rendered one -- the field is a uniform invariant on every snapshot.
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
    Snap2 = maybe_put_fingerprint(Tmpl, Snap1),
    Snap3 =
        case Tmpl of
            #{backend := Backend} -> Snap2#{backend => Backend};
            #{} -> Snap2
        end,
    %% The client binds a DOM event type only for a name it has been told about, so
    %% the names have to reach the payload that renders the markup declaring them.
    case Tmpl of
        #{az_names := Names} -> Snap3#{az_names => Names};
        #{} -> Snap3
    end.

%% Copies the `f` field from a template to a snapshot if present. Internal to
%% maybe_propagate/2.
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
Namespaces a slot's rendered value by the enclosing slot `Az`, so a repeated
stateless child (or any inline nested template) is an independent diff target.
Prefixes the value's fingerprint, its statics' baked `az`/marker ids, and every
inner dynamic `az` (recursing through inline nested templates) with the slot `az`.

A stateless component has no `view_id` boundary of its own, and its inner `az`
ids derive only from its own template fingerprint (`<Fp>-<n>`). So the *same*
render function rendered more than once in a view produces byte-identical `az`
targets; on the client every diff op resolves with `querySelector` (first match)
and lands on the first instance. Namespacing by the slot `az` -- applied here,
where the slot is in scope -- keeps each instance distinct. The same prefixing
makes two structurally identical inline nested templates (e.g. same-shaped
conditional branches) in different slots distinct too.

The prefix is made colon-free (`:` -> `-`): the client treats `:` as the
content-slot-index separator (a base element `az` never contains one) and
recovers a base element by stripping at the *first* colon, so an internal colon
in the prefix would misroute multi-slot and `?each`-among-siblings ops. A
stateful child (`view_id` boundary), an `?each` container (items are addressed
relative to the container, not by global `az`), and a client-owned `?local`
slot are left untouched; a scalar (escaped or bare) has no `az` to prefix and
passes through. This is exactly the structural discrimination `scope_val/2`
applies inside an already-scoped snapshot, reused here for the top-level slot
value -- so a stateless child evaluates to a plain snapshot map (no wrapper tag)
and is scoped by the same `#{s, d}` path as an inline nested template.
""".
-spec scope_slot(Az, Value) -> term() when
    Az :: az(),
    Value :: term().
scope_slot(Az, Value) when is_binary(Az) ->
    %% Ask the VALUE first. Only a nested snapshot is ever prefixed -- every other
    %% shape `scope_val/2` can be handed comes back untouched -- and a scalar is what
    %% nearly every content slot holds. Building the prefix regardless meant
    %% `binary:replace/4` on every one of them: a pure-Erlang wrapper that compiles
    %% the pattern, allocates a closure and copies the `az` into a fresh binary, all
    %% to hand back a namespace nothing would read. That is ~125ns a slot, per
    %% evaluation, on every render and every re-evaluated dynamic.
    case scopable(Value) of
        true -> scope_val(colon_free(Az), Value);
        false -> Value
    end;
scope_slot(_Az, Value) ->
    %% Undefined slot az (an az-nodiff slot): no target to namespace against.
    Value.

%% Mirrors `scope_val/2` clause for clause: `true` exactly where that function does
%% something other than hand its argument back. Keep the two in step -- a shape that
%% answers `false` here can never be prefixed, however `scope_val/2` treats it.
scopable({arizona_esc, Val}) -> scopable(Val);
scopable(#{view_id := _}) -> false;
scopable(#{t := ?EACH}) -> false;
scopable(#{az_local := _}) -> false;
scopable(#{s := _, d := _}) -> true;
scopable(_Val) -> false.

colon_free(Az) ->
    binary:replace(Az, ~":", ~"-", [global]).

%% Only ever reached via scope_val/2's guarded `#{s, d}` clause, so the argument
%% is always a snapshot map -- a non-snapshot (a scalar, a stateless child that
%% rendered to a bare binary) is filtered out there and never arrives here.
scope_snapshot(Prefix, #{s := S, d := D} = Snap) ->
    Backend = maps:get(backend, Snap, undefined),
    Snap1 = Snap#{
        s => scoped_statics(Backend, Prefix, Snap, S),
        d => [scope_dyn(Prefix, Dyn) || Dyn <- D]
    },
    scope_fp(Prefix, Snap1).

%% A snapshot with no backend (a hand-constructed map; templates from the parse
%% transform always carry one) has no known az-marker syntax to rewrite, so leave
%% its statics untouched -- the dynamic `az`/fingerprint prefixing below is
%% backend-independent and still applies.
scoped_statics(undefined, _Prefix, _Snap, S) ->
    S;
%% The fingerprint is both the memo key and the marker anchor. Anchor: the parse
%% transform builds every marker from the id it allocated, so a framework `az` in
%% a compiled static is always `<F>-<id>` -- matching the bare marker opener
%% instead would rewrite user-authored bytes (a static text child is spliced
%% verbatim, so a page showing markup carries ` az="` / `<!--az:` as content).
%% After a scope pass the statics read `<Prefix>-<F>-<id>` and `f` becomes
%% `<Prefix>-<F>`, so a re-scope of an already-scoped snapshot anchors correctly
%% too. Memo: prefixing a static list is a `binary:replace` per element -- pure
%% and deterministic in (Prefix, statics) -- so cache it per (Prefix, fingerprint)
%% in the process dictionary; a re-render or diff of the same slot (and every
%% level of a deep stateless nest, whose inner statics would otherwise be
%% re-walked once per ancestor) reuses the cached result.
scoped_statics(Backend, Prefix, #{f := F}, S) ->
    Key = {'$arizona_scoped_statics', Prefix, F},
    case erlang:get(Key) of
        undefined ->
            Scoped = [Backend:scope_static(F, Prefix, X) || X <- S],
            erlang:put(Key, Scoped),
            Scoped;
        Scoped ->
            Scoped
    end;
%% No fingerprint means no anchor, so no marker can be told apart from content.
%% Every template the parse transform emits carries one, so this is only reached
%% by a hand-built snapshot -- which has no framework markers to scope either.
scoped_statics(_Backend, _Prefix, #{}, S) ->
    S.

scope_fp(Prefix, #{f := F} = Snap) -> Snap#{f => <<Prefix/binary, "-", F/binary>>};
scope_fp(_Prefix, Snap) -> Snap.

scope_dyn(Prefix, {Az, Val}) ->
    {scope_az(Prefix, Az), scope_val(Prefix, Val)}.

scope_az(_Prefix, undefined) -> undefined;
scope_az(Prefix, Az) -> <<Prefix/binary, "-", Az/binary>>.

%% Recurse only into an inline nested template (a conditional branch): its inner
%% `az` ids are globally addressed, so they must carry the slot prefix. An
%% `?each` container, a stateful child view, and a `?local` slot own their inner
%% addressing (item-relative, a `view_id` boundary, and a client-owned key), so
%% their bodies are left alone -- only the container/element `az` in the enclosing
%% statics (already scoped above) and this dynamic's own `az` are prefixed.
scope_val(Prefix, {arizona_esc, V}) -> {arizona_esc, scope_val(Prefix, V)};
scope_val(_Prefix, #{view_id := _} = Child) -> Child;
scope_val(_Prefix, #{t := ?EACH} = Each) -> Each;
scope_val(_Prefix, #{az_local := _} = Local) -> Local;
scope_val(Prefix, #{s := _, d := _} = Nested) -> scope_snapshot(Prefix, Nested);
scope_val(_Prefix, Val) -> Val.

-doc """
Splits a list of `{Az, Val, Deps}` triples into the snapshot d-list, deps list,
and rendered values list (each rendered through `Backend`, an `arizona_renderer`).
""".
-spec unzip_triples(Backend, Triples) -> {DList, DepsList, Vals} when
    Backend :: module(),
    Triples :: [{az(), term(), deps()}],
    DList :: [{az(), term()}],
    DepsList :: [deps()],
    Vals :: [term()].
unzip_triples(_Backend, []) ->
    {[], [], []};
unzip_triples(Backend, [{Az, Val, Deps} | Rest]) ->
    {RestD, RestDeps, RestVals} = unzip_triples(Backend, Rest),
    {[{Az, Val} | RestD], [Deps | RestDeps], [unwrap_val(Backend, Val) | RestVals]}.

-doc """
Like `unzip_triples/2` but discards the rendered values list.
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
visible_keys({Front, Back}, Limit) -> take_visible(Front, Limit, Back).

%% The window is the first `Limit` keys, so flattening the WHOLE order to then
%% sublist it copies the hidden tail for nothing -- and on a `halt` stream that tail
%% is every key ever appended past the limit, so the copy grew with the session while
%% the answer stayed `Limit` long. Take from the front and stop; the append buffer is
%% touched only if the front runs out first, and then only for the keys still wanted
%% (`nthtail/2` walks cons cells without allocating any).
take_visible(_Front, 0, _Back) ->
    [];
take_visible([Key | Rest], Limit, Back) ->
    [Key | take_visible(Rest, Limit - 1, Back)];
take_visible([], Limit, Back) ->
    %% The first `Limit` of `reverse(Back)` is the last `Limit` OF `Back`, reversed.
    case length(Back) - Limit of
        Drop when Drop > 0 -> lists:reverse(lists:nthtail(Drop, Back));
        _ -> lists:reverse(Back)
    end.
