-module(arizona_json).
-moduledoc """
JSON render-target backend (`?json`).

Emits the byte sequences that make up the `s` statics as a JSON widget tree
for native clients (iOS/SwiftUI, Android/Compose, ...). Each element becomes a
flat JSON object:

```json
{"type": "Column", "az": "0", "padding": "16", "children": [ ... ]}
```

`type`, `az` and `children` are reserved keys; every other key is a widget
prop. The framework is vocabulary-agnostic -- the tag atom is emitted verbatim
as `type` and the client engine maps it to a real widget. Dynamic text children
become addressable `#text` nodes so the diff engine can target them by `az`.

String keys and values are escaped via `json:encode/1`, so the zipped statics
form valid JSON. So do the dynamics in the zipped document (`escape/1`,
`render_attr/2`): a scalar is a JSON string, an effect command its array and a
typed boolean prop a JSON boolean, exactly what a client builds by JSON-encoding
the wire value. On the wire itself a value stays raw
(`wire_escape/1`, `wire_attr/2`), because the client does that encoding.
""".
-behaviour(arizona_renderer).

-export([name/1]).
-export([element_open/1]).
-export([az_attr/1]).
-export([element_open_end/2]).
-export([element_void_close/0]).
-export([element_close/1]).
-export([attr/2]).
-export([attr_bool/2]).
-export([attr_command/2]).
-export([attr_dynamic_prefix/1]).
-export([children_sep/0]).
-export([each_items/1]).
-export([text_child/1]).
-export([content_slot_az/2]).
-export([content_slot_open/1]).
-export([content_slot_close/0]).
-export([is_void/1]).
-export([raw_text_kind/2]).
-export([content_context/2]).
-export([neutralize_raw_text/4]).
-export([scope_static/3]).
-export([supports_list_patch/0]).
-export([target/0]).
-export([supports_live_root/0]).
-export([content_type/0]).
-export([supports_local/0]).
-export([single_root_document/0]).
-export([supports_nodiff_keep/0]).
-export([supports_keyed_item_ops/0]).
-export([implied_attrs/2]).
-export([boolean_attrs/0]).
-export([attr_names/0]).
-export([escape/1]).
-export([render_attr/2]).
-export([wire_escape/1]).
-export([wire_attr/2]).
-export([embed_attrs/2]).
-export([format_error/1]).

-spec name(arizona_renderer:tag()) -> binary().
name(Bin) when is_binary(Bin) ->
    Bin;
name(Atom) ->
    atom_to_binary(Atom).

-spec element_open(binary()) -> binary().
element_open(TagName) ->
    <<"{\"type\":", (json_str(TagName))/binary>>.

-spec az_attr(binary()) -> binary().
az_attr(Az) ->
    <<",\"az\":", (json_str(Az))/binary>>.

-spec element_open_end(binary(), atom()) -> binary().
element_open_end(_Tag, _Context) ->
    <<",\"children\":[">>.

-spec element_void_close() -> binary().
element_void_close() ->
    <<",\"children\":[]}">>.

-spec element_close(binary()) -> binary().
element_close(_TagName) ->
    ~"]}".

-spec attr(binary(), binary()) -> binary().
attr(Name, Value) ->
    <<",", (json_str(Name))/binary, ":", (json_str(Value))/binary>>.

-spec attr_bool(binary(), boolean()) -> binary().
attr_bool(Name, Bool) ->
    <<",", (json_str(Name))/binary, ":", (atom_to_binary(Bool))/binary>>.

-spec attr_command(binary(), term()) -> binary().
attr_command(Name, Cmd) ->
    %% A folded effect command embedded as a raw JSON value (an array like
    %% `[0,"inc"]`), not a string -- the client interprets it directly.
    <<",", (json_str(Name))/binary, ":", (arizona_effect:encode_json(Cmd))/binary>>.

-spec attr_dynamic_prefix(binary()) -> binary().
attr_dynamic_prefix(Name) ->
    %% Bake the prop name into the static; the dynamic supplies just the value
    %% (the client JSON-encodes it like any other dynamic value).
    <<",", (json_str(Name))/binary, ":">>.

-spec children_sep() -> binary().
children_sep() ->
    ~",".

-spec each_items([iodata()]) -> iodata().
each_items(Items) ->
    [$[, lists:join($,, Items), $]].

-spec text_child(binary()) -> binary().
text_child(Text) ->
    json_str(Text).

-spec content_slot_az(binary(), non_neg_integer()) -> binary().
content_slot_az(ElemAz, Slot) ->
    %% Distinct from the element's own `az` (which shares `ElemAz`): JSON
    %% nodes live in one flat registry, so a slot node cannot reuse its
    %% parent's id the way an HTML comment marker can.
    <<ElemAz/binary, "t", (integer_to_binary(Slot))/binary>>.

-spec content_slot_open(binary()) -> binary().
content_slot_open(Az) ->
    %% A `#slot` is the addressable, transparent (fragment) wrapper for any
    %% dynamic child: its `children` hold the rendered value -- a string for
    %% text, the item widgets for an each, a component object for a child view.
    %% The client renders a `#slot` by splicing its children into the parent
    %% (flattening nested arrays). One node per dynamic, like the HTML markers.
    <<"{\"type\":\"#slot\",\"az\":", (json_str(Az))/binary, ",\"children\":[">>.

-spec content_slot_close() -> binary().
content_slot_close() ->
    ~"]}".

-spec is_void(arizona_renderer:tag()) -> boolean().
is_void(_Tag) ->
    false.

-spec raw_text_kind(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:raw_text_kind().
raw_text_kind(_Tag, _Context) ->
    %% This wire is JSON, not HTML -- dynamic slots are `#slot` objects,
    %% not comment markers, so the raw-text corruption does not apply.
    none.

-spec content_context(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:content_context().
content_context(_Tag, Parent) ->
    %% No foreign-content mode here -- the context never changes.
    Parent.

%% Anchored on the fingerprint: every framework-emitted `az` in a compiled
%% static is `<Fp>-<id>`, so `"az":"<Fp>` cannot be confused with a JSON `az`
%% key the template itself carries (a user attribute literally named `az`, or
%% an effect command whose payload has that key).
-spec scope_static(binary(), binary(), binary()) -> binary().
scope_static(Fp, Prefix, S0) ->
    binary:replace(
        S0, <<"\"az\":\"", Fp/binary>>, <<"\"az\":\"", Prefix/binary, "-", Fp/binary>>, [global]
    ).

%% The `?json` clients do not implement `?OP_LIST_PATCH`; single-root
%% list eachs keep the wholesale re-render it already handles.
supports_list_patch() -> false.

target() -> json.

supports_live_root() -> true.

content_type() -> ~"application/json".

supports_local() -> false.

single_root_document() -> true.

supports_nodiff_keep() -> false.

supports_keyed_item_ops() -> true.

-spec implied_attrs(arizona_renderer:tag(), [binary()]) -> [].
implied_attrs(_Tag, _Names) -> [].

boolean_attrs() -> value.

%% Props are JSON object keys.
-spec attr_names() -> case_sensitive.
attr_names() -> case_sensitive.

%% A content value in the zipped document is a JSON string.
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) -> json_str(Bin).

%% On the wire the value stays raw: the client JSON-encodes each `d` value as it
%% interleaves, so encoding it here would make it a string of a string.
-spec wire_escape(binary()) -> binary().
wire_escape(Bin) -> Bin.

%% The JSON wire has no raw-text elements (raw_text_kind/2 is always `none`), so
%% no dynamic is ever wrapped in this callback. Required by the behaviour; identity.
-spec neutralize_raw_text(arizona_renderer:tag(), binary(), binary(), term()) -> term().
neutralize_raw_text(_Tag, _Before, _After, Value) -> Value.

%% Render a dynamic attribute value: the prop name is baked into the static by
%% attr_dynamic_prefix/1, so the dynamic is just the value -- the JSON the client builds
%% by encoding the wire value: an array for an effect command, as a folded one
%% (attr_command/2) is, a boolean for a typed boolean, else a string.
-spec render_attr(binary(), term()) -> binary().
render_attr(Name, Value) ->
    json_value(wire_attr(Name, Value)).

%% The prop as the client receives it, before it JSON-encodes it: an effect
%% command as its array, so a tap prop reads the same dynamic or folded, a typed
%% boolean (`arizona_template:bool_attr/1`) as itself, and any other value
%% stringified: to_bin/1 unwraps a `?raw` and the markup a literal one assembles, as
%% JSON has no escaping to opt out of.
-spec wire_attr(binary(), term()) -> json:encode_value().
wire_attr(_Name, {arizona_bool, Bool}) ->
    Bool;
wire_attr(_Name, Value) ->
    case arizona_template:classify_trusted(Value) of
        {effect, Cmds} -> arizona_effect:json_value(Cmds);
        {raw, V} when is_map(V) -> arizona_template:raw_not_text(V);
        _NotACommand -> arizona_template:to_bin(Value)
    end.

-doc """
The JSON wording of the errors the core raises for a `?json` template
(`c:arizona_renderer:format_error/1`), in the terms of its native clients: widget
nodes (`'Row'`, `'Text'`) in the examples, an `on_tap` prop rather than an event
attribute, no client-owned `?local` state, and no directive kept in the document.
JSON has no raw-text elements and no selection roles, so those reasons never reach it.
""".
-spec format_error(term()) -> string().
format_error(computed_element_tag) ->
    "a node's type must be a literal atom or binary. The type is baked into the "
    "template's statics at compile time, which is what lets a later change ship only "
    "the values, "
    "so a type chosen at runtime has no single set of statics to bake. Put the CHOICE "
    "inside a stable node instead of choosing the node: {'Column', [], [case Kind of "
    "note -> {'Text', [], [X]}; _ -> {'Button', [], [X]} end]} -- a conditional in a "
    "content slot may return bare node tuples, so the branches can differ completely. "
    "Inside an ?each the stable node is the item itself and carries az_key, which is "
    "also what keeps its identity across a change: "
    "fun(Item, Key) -> {'Row', [{az_key, Key}], [case Item of ... end]} end";
format_error(element_in_comprehension) ->
    "a comprehension in a content slot builds its list at render time, so the slot "
    "holds one plain value and a node (or template) per item fails at render. Render a "
    "runtime list of nodes with ?each, which compiles the item once and diffs it per "
    "item: ?each(fun(X) -> {'Text', [], [X]} end, ?get(items)). A comprehension of "
    "plain values is fine";
format_error(each_body_not_element) ->
    "an ?each callback over a list must return a node ({Type, Props, Children}) or a "
    "list of nodes -- ?each builds a per-item template for fine-grained diffing, so a "
    "single-value body defeats its purpose (and a template or descriptor value crashes on "
    "the first diff). For a list of plain values use a list comprehension or "
    "lists:map/2. For a conditional, put it inside a node as a text/value child: "
    "{'Text', [], [case ... of ... end]}";
format_error(each_stream_body_not_element) ->
    "an ?each callback over a stream or map (a 2-arg fun) must return a node ({Type, "
    "Props, Children}); a map source may also take a list of nodes or a static/mixed "
    "fragment -- a stream/map keys each item for per-item diffing, which a single-value "
    "body throws away. Unlike a list there is no comprehension fallback (a comprehension "
    "has no stream/keyed semantics): wrap the value in a node, e.g. "
    "fun(Item, Key) -> {'Text', [{az_key, Key}], [Item]} end";
format_error(each_named_fun_multi_clause) ->
    "an ?each callback given as a local fun reference (fun name/1 or fun name/2) must have "
    "a single clause -- ?each inlines the function's body into one shared per-item "
    "template, so multiple clauses (which would select different per-item structures) "
    "can't be compiled to a single template. Collapse them into one clause with a case "
    "inside the returned node: name(I) -> {'Row', [], [case I of ... end]}";
format_error(each_named_fun_undefined) ->
    "the ?each callback references a local fun (fun name/1 or fun name/2) that is not "
    "defined in this module. Define it as a single-clause function returning a node, or "
    "inline the callback (fun(I) -> {'Text', [], [I]} end)";
format_error(each_remote_fun_ref) ->
    "an ?each callback cannot be a remote fun reference (fun mod:name/arity) -- ?each "
    "inlines the callback body to build a per-item template, which is impossible across "
    "a module boundary. Inline it (fun(I) -> {'Text', [], [I]} end), or move the body "
    "into a single-clause local function and pass fun name/1";
format_error(improper_template_list) ->
    "a template's prop, child and body lists must be proper list literals: their items "
    "are laid out one slot each at compile time, so a list ending in an expression "
    "([A, B | Rest]) has no length to lay out. Write every item in the list; render a "
    "runtime list of children with ?each, as one item of it: {'Column', [], "
    "[{'Text', [], [~\"first\"]}, ?each(fun(I) -> {'Text', [], [I]} end, Rest)]}";
format_error(local_unsupported) ->
    "?local is client-owned state that the browser keeps, and a ?json view's native "
    "client keeps none: it has no local slot to bind the key to. Keep the value in the "
    "view's bindings and change it from handle_event/3, which diffs it to the client";
format_error(local_orphaned) ->
    "?local must sit inside a node, as a content child or a prop value, and a ?json "
    "view's native client keeps no ?local state at all: keep the value in the view's "
    "bindings and change it from handle_event/3";
format_error(nodiff_value) ->
    "az-nodiff is a bare directive and takes no value -- write 'az-nodiff' (or "
    "az_nodiff / <<\"az-nodiff\">>) alone in the prop list. The JSON document does not "
    "carry it: the node renders once and is never patched";
format_error(nested_nodiff) ->
    "az-nodiff is only honored on a template's top-level (root) node -- it is a "
    "whole-template directive, not a per-node one, so on a nested node it cannot be "
    "scoped and would reach the native client as an ordinary prop. Move it to the root "
    "node, or split the subtree into its own ?json/?stateless template whose root "
    "carries az-nodiff";
format_error(effect_in_content) ->
    "an effect command (a command builder call, e.g. arizona_android:push_event(~\"inc\")) "
    "is not content: a content slot renders it as text, and no native client runs a "
    "command there. Put the command in an event prop's value, e.g. {'Button', "
    "[{on_tap, Cmd}], [~\"+\"]}, or return it from a handler. To show a command as text, "
    "encode it explicitly: arizona_effect:encode_json(Cmd)";
format_error(each_stream_body_control_flow) ->
    "an ?each callback over a stream or map (a 2-arg fun) must return a node, and a "
    "control-flow body (case, if, begin, try, receive) is not one: every item shares one "
    "compiled per-item template (statics once, dynamics per item), so an item's outer "
    "widget type is fixed at compile time. The branches can still be entirely different "
    "widgets -- put them inside one stable item node carrying az_key, e.g. "
    "fun(Item, Key) -> {'Row', [{az_key, Key}], [case Item of ... end]} end, where a "
    "conditional child may return bare node tuples ('Text' vs 'Button' vs a whole "
    "'Column' subtree all work)";
format_error(stream_each_multi_root) ->
    "an ?each over a stream must return ONE node per item, not a list of nodes, a "
    "fragment mixing text and nodes, or bare text: a keyed op (insert, remove, move, "
    "item patch) addresses an item by the single node carrying its az_key, so any other "
    "root would be left behind on a remove and out of order on a move. Put the parts "
    "inside one item node, e.g. fun(Item, Key) -> {'Row', [{az_key, Key}], "
    "[{'Text', [], [Item]}]} end. The same callback over a map may return several roots";
format_error(stream_each_keyless) ->
    "an ?each over a stream must put az_key on each item's root node: a keyed op "
    "(remove, move, item patch) finds the item by it, so without one every such op is "
    "skipped and the native client's widget tree drifts from the stream. Key the root "
    "with the callback's second argument, e.g. fun(Item, Key) -> {'Text', "
    "[{az_key, Key}], [Item]} end. The same callback over a map needs no key";
format_error({stateful_in_layout, Mod, Fun, Handler}) ->
    lists:flatten(
        io_lib:format(
            "layout ~s:~s/1 embeds ?stateful(~s, ...), but a layout renders once "
            "at SSR and never joins the live view tree, so the component would "
            "claim a view node the server never registers, and every event its "
            "native client sends from it would be dropped. Use ?stateless for layout "
            "chrome. A ?json client has no navigation that keeps a view live (a navigate "
            "unmounts the page and mounts the next), so chrome that must stay live "
            "belongs in one view that changes its content through its state and events "
            "instead of navigating.",
            [Mod, Fun, Handler]
        )
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec json_str(binary()) -> binary().
json_str(Bin) ->
    json_value(Bin).

%% A value that is not UTF-8 is refused by the encoder for free and named
%% `non_utf8_render` (arizona_utf8:refuse/3), inside the slot's located
%% evaluation, so the template line comes with it. The try is inline: this
%% runs per value, and only a failure pays.
json_value(Value) ->
    try
        iolist_to_binary(json:encode(Value))
    catch
        error:Reason:Stacktrace -> arizona_utf8:refuse(Value, Reason, Stacktrace)
    end.

%% A JSON document is one value, which cannot take extra content; the static
%% client is handed the names by `start({attrs})` instead.
-spec embed_attrs(iodata(), [binary()]) -> iodata().
embed_attrs(Doc, _AttrNames) ->
    Doc.
