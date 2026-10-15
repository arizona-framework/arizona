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

%% The html/native/terminal (+ each) stubs are intentionally identical: each
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
-export([to_bin/1]).
-export([raw/1]).
-export([raw_attr/2]).
-export([bool_attr/1]).
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
-export([branch/2]).
-export([stateless_template/1]).
-export([same_nodiff/3]).
-export([same_nodiff/2]).
-export([nodiff_token/3]).
-export([split_triples/1]).
-export([visible_keys/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/1]).
-ignore_xref([format_error/2]).
%% Emitted by the parse transform for a conditional branch value it cannot see.
-ignore_xref([branch/2]).
%% Emitted by the parse transform as an az-nodiff element's `az-nodiff` value.
-ignore_xref([nodiff_token/3]).
%% Public escape opt-out for template authors; no internal callers by design.
-ignore_xref([raw/1]).
%% Emitted by the parse transform around an interpolated attribute with a `?raw` part.
-ignore_xref([raw_attr/2]).
%% Emitted by the parse transform around a `value` backend's dynamic attribute.
-ignore_xref([bool_attr/1]).

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
-export_type([nodiff_identity/0]).
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
    %% An az-nodiff template's identity beside its fingerprint: the conditional
    %% branch that produced it and the value of its root's `az_key`.
    branch => pos_integer(),
    az_key => term(),
    %% Stamped when the backend's document must be one node (see
    %% arizona_renderer:single_root_document/0) and the body is several.
    multi_root => true,
    backend => module()
}.

-nominal each_template() :: #{
    t := 0,
    s := [binary()],
    d := fun((term()) -> [dynamic()]) | fun((term(), term()) -> [dynamic()]),
    f := binary(),
    %% An item element carrying az-nodiff compiles its per-item template nodiff.
    diff => false,
    single_root => true,
    %% Per dynamic: may a changed stream item keep its old render when the deps
    %% miss the changed fields (every item read tracked in its own closure)?
    reuse => [boolean()],
    %% A 2-arg callback whose body is not exactly one element: legal over a map, refused
    %% over a stream by each/2.
    multi_root => true,
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
    nodiff => nodiff_identity(),
    view_id => binary(),
    backend => module()
}.

-nominal nodiff_identity() :: {pos_integer() | undefined, term()}.

-nominal stateful_descriptor() :: #{stateful := module(), props := map()}.
-nominal stateless_descriptor() :: #{
    callback := fun((map()) -> template()),
    props := map(),
    branch => pos_integer()
}.

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
    %% Instantiation is the one moment a runtime-bound module
    %% (`?stateful(?get(page), ...)`) is known, so the socket can walk its
    %% compile-time attributes and deliver them on the frame that mounts it.
    ok = arizona_event_attrs:observe_mod(Handler),
    #{stateful => Handler, props => Props}.

-doc """
Builds a stateless child descriptor from a 1-arity render fun.
""".
-spec stateless(Callback, Props) -> stateless_descriptor() when
    Callback :: fun((map()) -> template()),
    Props :: map().
stateless(Callback, Props) when is_function(Callback, 1), is_map(Props) ->
    ok = arizona_event_attrs:observe_callback(Callback),
    #{callback => Callback, props => Props}.

-doc """
Builds a stateless child descriptor from a `Handler:Fun/1` reference.
""".
-spec stateless(Handler, Fun, Props) -> stateless_descriptor() when
    Handler :: module(),
    Fun :: atom(),
    Props :: map().
stateless(Handler, Fun, Props) when is_atom(Handler), is_atom(Fun), is_map(Props) ->
    ok = arizona_event_attrs:observe_mod(Handler),
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
each(#stream{}, #{t := ?EACH, multi_root := true} = Tmpl) when not is_map_key(diff, Tmpl) ->
    %% The callback serves maps too, so this pairing cannot be refused at compile time;
    %% see maybe_multi_root_opt/3 in arizona_parse_transform. A nodiff item is never
    %% addressed by a keyed op, so it may keep any number of roots.
    erlang:error(stream_each_multi_root, none, [{error_info, #{module => ?MODULE}}]);
each(Source, #{t := ?EACH, d := DFun} = Tmpl) when is_function(DFun, 1); is_function(DFun, 2) ->
    #{t => ?EACH, source => Source, template => Tmpl};
each(Fun, _Source) when is_function(Fun, 1); is_function(Fun, 2) ->
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
to_bin(V) when is_float(V) -> float_to_bin(V);
to_bin(V) when is_atom(V) -> atom_to_binary(V);
%% Escape markers are unwrapped to their raw bytes here. to_bin never escapes;
%% escaping happens at the render-output boundary. At SSR that boundary is
%% `arizona_render:render_dyn/2` (it calls `escape_value/2` through the backend);
%% on the live diff the boundary is the JS client, which text-nodes a scalar
%% `?OP_TEXT` value (so `<` is literal text, never parsed) -- so the diff path
%% stays raw end to end.
to_bin({arizona_esc, V}) ->
    to_bin(V);
%% The trusted shapes of classify_trusted/1, matched in the clause heads rather
%% than by asking it: escape_value/2, escape_marked/2 and the backends'
%% render_attr/2 classify a value and then convert it here, and a second
%% classify_trusted/1 call per list value was the one thing that cost. A head
%% match dispatches without a call, and the answer is the same for every input.
to_bin({arizona_raw, V}) ->
    to_bin(V);
to_bin({arizona_effect, _} = Cmd) ->
    arizona_effect:encode(Cmd);
to_bin([{arizona_effect, _} | _] = Cmds) ->
    arizona_effect:encode(Cmds);
to_bin(V) when is_list(V) ->
    %% A charlist/iodata is text: decode it as Unicode codepoints so the output is
    %% valid UTF-8. `iolist_to_binary/1` truncates a 128..255 codepoint to an
    %% invalid latin-1 byte (`"café"` -> `<<...,233>>` instead of
    %% `<<...,195,169>>`) and crashes a codepoint > 255 with a bare `badarg`. A
    %% term that is not valid chardata (an atom/boolean inside an interpolated
    %% attribute such as `[~"a-", ?get(on)]`) yields the same `bad_template_value`
    %% as any other unsupported value. A list wrapping a binary of non-UTF-8 bytes
    %% is named `non_utf8_render` instead; the decoder's failure is examined only
    %% after it failed, so a valid value pays nothing for that.
    case chardata_to_binary(V) of
        Bin when is_binary(Bin) ->
            Bin;
        {_Status, Valid, Rest} when is_binary(Valid) ->
            case first_byte(Rest) of
                {ok, Byte} ->
                    erlang:error({non_utf8_render, Byte, byte_size(Valid)}, [V], [
                        {error_info, #{module => arizona_utf8}}
                    ]);
                error ->
                    erlang:error({bad_template_value, V}, [V], [
                        {error_info, #{module => ?MODULE}}
                    ])
            end;
        error ->
            %% A command list leading with a nested list or `[]` is not text either;
            %% asked only once the decoder refused, so text pays nothing for it.
            case leads_with_effect(V) of
                true ->
                    arizona_effect:encode(V);
                false ->
                    erlang:error({bad_template_value, V}, [V], [
                        {error_info, #{module => ?MODULE}}
                    ])
            end
    end;
to_bin(V) ->
    erlang:error({bad_template_value, V}, [V], [{error_info, #{module => ?MODULE}}]).

%% The first element the decoder could not convert: a byte when it is binary
%% data, `error` when it is an integer that is not a code point.
first_byte(<<Byte, _/binary>>) -> {ok, Byte};
first_byte([H | _]) -> first_byte(H);
first_byte(_CodePoint) -> error.

chardata_to_binary(Chardata) ->
    try
        unicode:characters_to_binary(Chardata)
    catch
        error:badarg -> error
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
    lists:flatten(io_lib:format("cannot convert ~0tp to binary in template", [V]));
format_error(stream_each_multi_root) ->
    "an ?each over a stream must return ONE element per item, not a list of elements, a "
    "fragment mixing text and elements, or bare text: a keyed op (insert, remove, move, "
    "item patch) addresses an item by the single element carrying its az_key, so any other "
    "root would be left behind on a remove and out of order on a move. Put the parts "
    "inside one item element, e.g. fun(Item, Key) -> {li, [{az_key, Key}], [Item]} end. "
    "The same callback over a map may return several roots".

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
    #{general => format_error(Reason)};
format_error(stream_each_multi_root = Reason, _ST) ->
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

%% Internal, emitted only by the parse transform for an interpolated attribute value
%% with a literal `raw/1` part. The assembled value is markup, so it is one raw value:
%% the raw parts verbatim, every other part escaped through `Backend` -- a static or
%% `?get` part means the same text with or without a raw sibling.
-doc false.
-spec raw_attr(Backend, Parts) -> {arizona_raw, binary()} when
    Backend :: module(),
    Parts :: [term()].
raw_attr(Backend, Parts) ->
    {arizona_raw, <<<<(escape_value(Backend, P))/binary>> || P <- Parts>>}.

%% Internal, emitted only by the parse transform for a `value`-boolean backend
%% (`boolean_attrs/0`, i.e. ?native): tags a dynamic attribute's boolean so the diff
%% sends `true`/`false` as the prop value instead of adding and removing it. Any
%% other value passes through. Never valid in a presence-boolean (?html) template.
-doc false.
-spec bool_attr(Value) -> Value | {arizona_bool, boolean()} when
    Value :: term().
bool_attr(Bool) when is_boolean(Bool) -> {arizona_bool, Bool};
bool_attr(Value) -> Value.

-doc """
Classifies the escape-exempt content shapes: a `raw/1` opt-out
(`{arizona_raw, V}`, returned unwrapped) and an already-encoded effect command
or command list (`{arizona_effect, _}`). Every other value is `value`.

The definition of the shape vocabulary -- `escape_value/2`, `mark_esc/1`, and
`arizona_html:render_attr/2` dispatch on it. `to_bin/1` matches the same shapes
in its clause heads instead, because it runs right after one of those callers
has already classified the value; growing the vocabulary means updating both.
Each call site decides the policy for a shape (`mark_esc/1` deliberately unwraps
and re-escapes a helper-produced raw).
""".
-spec classify_trusted(Value) -> {raw, term()} | {effect, Effect} | value when
    Value :: term(),
    Effect :: term().
classify_trusted({arizona_raw, V}) -> {raw, V};
classify_trusted({arizona_effect, _} = Cmd) -> {effect, Cmd};
classify_trusted([{arizona_effect, _} | _] = Cmds) -> {effect, Cmds};
classify_trusted([Nested | _] = Cmds) when is_list(Nested) -> classify_nested(Cmds);
classify_trusted(_Value) -> value.

%% A command list may lead with a nested list or `[]` (a conditional command item,
%% see arizona_effect:json_value/1), so its first leaf decides. Only the path to
%% that leaf is walked: text leads with a binary or a code point and is settled at
%% once, and a list of nothing but empty items is the empty value it renders as.
classify_nested(Cmds) ->
    case leads_with_effect(Cmds) of
        true -> {effect, Cmds};
        false -> value
    end.

leads_with_effect([{arizona_effect, _} | _]) -> true;
leads_with_effect([[] | Rest]) -> leads_with_effect(Rest);
leads_with_effect([[Head | Tail] | Rest]) -> leads_with_effect([Head, Tail | Rest]);
leads_with_effect(_) -> false.

-doc """
Renders a value to its output binary, escaping via `Backend` (an
`arizona_renderer`): HTML entity-escaping, terminal control-char sanitizing, or
JSON string encoding. `raw/1`-wrapped values and already-encoded
`arizona_effect` commands are classified out and pass through unescaped (the
`?raw` opt-out and effects are trusted, backend-independent).
""".
-spec escape_value(Backend, Value) -> binary() when
    Backend :: module(),
    Value :: term().
escape_value(Backend, V0) ->
    case classify_trusted(V0) of
        {raw, V} -> to_bin(V);
        {effect, Cmd} -> arizona_effect:encode(Cmd);
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
`a` attributes the template declares.

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
            #{diff := false, branch := _} -> Snap#{diff => false, nodiff => nodiff_identity(Tmpl)};
            #{diff := false, az_key := _} -> Snap#{diff => false, nodiff => nodiff_identity(Tmpl)};
            #{diff := false} -> Snap#{diff => false};
            #{} -> Snap
        end,
    Snap2 = maybe_put_fingerprint(Tmpl, Snap1),
    case Tmpl of
        #{backend := Backend} -> Snap2#{backend => Backend};
        #{} -> Snap2
    end.

%% An az-nodiff element's identity beside its fingerprint. Only a template that has one puts it
%% on its snapshot (an `?each` item never does), so an absent `nodiff` field reads as the
%% identity with neither part.
nodiff_identity(Tmpl) ->
    {maps:get(branch, Tmpl, undefined), maps:get(az_key, Tmpl, undefined)}.

snapshot_nodiff(Snap) ->
    maps:get(nodiff, Snap, {undefined, undefined}).

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

%% Whether `OldF` is `F` as scope_slot/2 scoped it into the slot `Az`. The prefix
%% is that slot's own (`colon_free(Az)` plus `-`, the size of `Az` plus one), so a
%% match on its size followed by exactly `F` decides it without building the scoped
%% fingerprint.
scoped_by(OldF, Az, F) ->
    Prefix = byte_size(Az) + 1,
    case OldF of
        <<_:Prefix/binary, F/binary>> -> true;
        _ -> false
    end.

-doc """
Stamps the value a conditional branch produced with that branch's index within
its slot, when the value is an az-nodiff template or a `?stateless` descriptor
(whose template takes the stamp when it is rendered). Two branches can produce
the same template -- the same markup, the same helper -- and the stamp is what
tells them apart once their az-nodiff elements are no longer evaluated. Any other value is
returned as is.

The parse transform stamps a literal template itself and calls this only for a
branch value it cannot see at compile time.
""".
-spec branch(Branch, Value) -> Value1 when
    Branch :: pos_integer(),
    Value :: term(),
    Value1 :: term().
branch(Branch, #{s := _, d := [{Az, {attr, ~"az-nodiff", _}, Loc} | D], f := F} = Tmpl) ->
    %% The az-nodiff element's token -- the `az-nodiff` value, its root's first dynamic (the
    %% parse transform puts it there) -- names the branch too.
    Key = maps:get(az_key, Tmpl, undefined),
    Name = {Az, {attr, ~"az-nodiff", fun() -> nodiff_token(F, Branch, Key) end}, Loc},
    Tmpl#{branch => Branch, d => [Name | D]};
branch(Branch, #{s := _, d := _, diff := false} = Tmpl) ->
    Tmpl#{branch => Branch};
branch(Branch, #{callback := _, props := _} = Desc) ->
    Desc#{branch => Branch};
branch(_Branch, Value) ->
    Value.

-doc """
The value of the `az-nodiff` attribute on the root of an az-nodiff element: its
identity as `same_nodiff/3` decides it -- the template's fingerprint `F`, the
conditional branch that produced it and its `az_key` -- spelled as one opaque
token. The client matches az-nodiff elements by it, so a wholesale re-render around a
az-nodiff element (a reconnect, a container `?OP_TEXT`) carries the existing DOM over to a
new render naming the same az-nodiff element, and replaces it otherwise.

The token is not scoped: where the az-nodiff element sits (its slot, the item it is in) is
read off the DOM by the client, which is where those addresses already are.
""".
-spec nodiff_token(F, Branch, Key) -> binary() when
    F :: binary(),
    Branch :: pos_integer() | undefined,
    Key :: term().
nodiff_token(F, Branch, Key) ->
    <<F/binary, (nodiff_branch_part(Branch))/binary, (nodiff_key_part(Key))/binary>>.

nodiff_branch_part(undefined) -> <<>>;
nodiff_branch_part(Branch) -> <<".b", (integer_to_binary(Branch))/binary>>.

%% A key is any term, so it is hashed: the attribute names it, it does not carry it.
nodiff_key_part(undefined) -> <<>>;
nodiff_key_part(Key) -> <<".k", (integer_to_binary(erlang:phash2(Key, 1 bsl 32), 36))/binary>>.

-doc """
Renders a `?stateless` descriptor to its template, passing on the branch stamp
the descriptor carries (see `branch/2`).
""".
-spec stateless_template(Descriptor) -> term() when
    Descriptor :: stateless_descriptor().
stateless_template(#{callback := Callback, props := Props, branch := Branch}) ->
    branch(Branch, Callback(Props));
stateless_template(#{callback := Callback, props := Props}) ->
    Callback(Props).

-doc """
Whether the template `Tmpl`, about to fill the slot `Az`, is the az-nodiff element
the slot's snapshot `Old` already holds: the same scoped fingerprint, the same
producing branch and the same `az_key`.
""".
-spec same_nodiff(Az, Tmpl, Old) -> boolean() when
    Az :: binary(),
    Tmpl :: term(),
    Old :: snapshot().
same_nodiff(Az, #{s := _, d := _, f := F, diff := false} = Tmpl, #{f := OldF} = Old) ->
    scoped_by(OldF, Az, F) andalso nodiff_identity(Tmpl) =:= snapshot_nodiff(Old);
same_nodiff(_Az, _Tmpl, _Old) ->
    false.

-doc """
Whether two az-nodiff snapshots of the same fingerprint are the same az-nodiff element: the
same producing branch and `az_key`, and the same child view when they are one.
""".
-spec same_nodiff(New, Old) -> boolean() when
    New :: snapshot(),
    Old :: snapshot().
same_nodiff(New, Old) ->
    snapshot_nodiff(New) =:= snapshot_nodiff(Old) andalso
        maps:get(view_id, New, undefined) =:= maps:get(view_id, Old, undefined).

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
Splits a list of `{Az, Val, Deps}` triples into the snapshot d-list and
deps list. (`arizona_render:render/2` needs the document too and fuses this
split into its zip walk, `zip_triples/3`.)
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

%% The shortest digits that read back as the same float (`short`), printed
%% positionally where JS prints a number positionally (1.0e-7 =< |x| < 1.0e21)
%% and in exponent form outside it. `short` alone picks whichever text is
%% shorter, so a total of 1500.0 would read "1.5e3". A fixed decimal count is no
%% fix: it rounds small values to "0.0" and raises `badarg` past ~1e244.
%% In range the exponent is -7..20, so its `e` can only be the second or third
%% byte from the end: two fixed-offset probes find it, no scan.
float_to_bin(V) when V >= 1.0e-7, V < 1.0e21; V =< -1.0e-7, V > -1.0e21 ->
    Bin = float_to_binary(V, [short]),
    Skip = byte_size(Bin) - 3,
    case Bin of
        <<_:Skip/binary, _, $e, _>> -> positional(Bin, Skip + 1);
        <<_:Skip/binary, $e, _, _>> -> positional(Bin, Skip);
        _ -> Bin
    end;
float_to_bin(V) ->
    float_to_binary(V, [short]).

positional(Bin, EPos) ->
    <<Mantissa:EPos/binary, $e, Exp/binary>> = Bin,
    shift_point(Mantissa, binary_to_integer(Exp)).

%% `short`'s exponent form has one integer digit, and a fractional part of
%% "0" only when it is that digit alone. Each binary built below starts with a
%% literal or an integer segment: one led by an unsized binary segment is built
%% appendable, several times the cost of a plain one.
shift_point(<<$-, Mantissa/binary>>, Exp) ->
    <<$-, (shift_point(Mantissa, Exp))/binary>>;
shift_point(<<Int, $., $0>>, Exp) ->
    shift_point(Int, <<>>, Exp);
shift_point(<<Int, $., Frac/binary>>, Exp) ->
    shift_point(Int, Frac, Exp).

%% The point moves `Exp` places right of the digit `Int`, padding with zeros on
%% whichever side runs out; an integral value keeps the `.0` suffix.
shift_point(Int, Frac, Exp) when Exp < 0 ->
    <<"0.", (binary:copy(~"0", -Exp - 1))/binary, Int, Frac/binary>>;
shift_point(Int, Frac, Exp) when Exp >= byte_size(Frac) ->
    <<Int, Frac/binary, (binary:copy(~"0", Exp - byte_size(Frac)))/binary, ".0">>;
shift_point(Int, Frac, Exp) ->
    <<Head:Exp/binary, Tail/binary>> = Frac,
    <<Int, Head:Exp/binary, $., Tail/binary>>.
