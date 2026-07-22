-module(arizona_native).
-moduledoc """
Native (JSON) render-target backend.

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
form valid JSON.
""".
-behaviour(arizona_renderer).

-export([name/1]).
-export([element_open/1]).
-export([az_attr/1]).
-export([element_open_end/0]).
-export([element_void_close/0]).
-export([element_close/1]).
-export([attr/2]).
-export([attr_boolean/1]).
-export([attr_command/2]).
-export([attr_dyn_name/1]).
-export([children_sep/0]).
-export([text_child/1]).
-export([text_az/2]).
-export([text_slot_open/1]).
-export([text_slot_close/0]).
-export([is_void/1]).
-export([raw_text_kind/1]).
-export([scope_static/2]).
-export([supports_list_patch/0]).
-export([escape/1]).
-export([render_attr/2]).

-spec name(atom()) -> binary().
name(Atom) ->
    atom_to_binary(Atom).

-spec element_open(binary()) -> binary().
element_open(TagName) ->
    <<"{\"type\":", (json_str(TagName))/binary>>.

-spec az_attr(binary()) -> binary().
az_attr(Az) ->
    <<",\"az\":", (json_str(Az))/binary>>.

-spec element_open_end() -> binary().
element_open_end() ->
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

-spec attr_boolean(binary()) -> binary().
attr_boolean(Name) ->
    <<",", (json_str(Name))/binary, ":true">>.

-spec attr_command(binary(), term()) -> binary().
attr_command(Name, Cmd) ->
    %% A folded effect command embedded as a raw JSON value (an array like
    %% `[0,"inc"]`), not a string -- the client interprets it directly.
    <<",", (json_str(Name))/binary, ":", (arizona_effect:encode_json(Cmd))/binary>>.

-spec attr_dyn_name(binary()) -> binary().
attr_dyn_name(Name) ->
    %% Bake the prop name into the static; the dynamic supplies just the value
    %% (the client JSON-encodes it like any other dynamic value).
    <<",", (json_str(Name))/binary, ":">>.

-spec children_sep() -> binary().
children_sep() ->
    ~",".

-spec text_child(binary()) -> binary().
text_child(Text) ->
    json_str(Text).

-spec text_az(binary(), non_neg_integer()) -> binary().
text_az(ElemAz, Slot) ->
    %% Distinct from the element's own `az` (which shares `ElemAz`): native
    %% nodes live in one flat registry, so a slot node cannot reuse its
    %% parent's id the way an HTML comment marker can.
    <<ElemAz/binary, "t", (integer_to_binary(Slot))/binary>>.

-spec text_slot_open(binary()) -> binary().
text_slot_open(Az) ->
    %% A `#slot` is the addressable, transparent (fragment) wrapper for any
    %% dynamic child: its `children` hold the rendered value -- a string for
    %% text, the item widgets for an each, a component object for a child view.
    %% The client renders a `#slot` by splicing its children into the parent
    %% (flattening nested arrays). One node per dynamic, like the HTML markers.
    <<"{\"type\":\"#slot\",\"az\":", (json_str(Az))/binary, ",\"children\":[">>.

-spec text_slot_close() -> binary().
text_slot_close() ->
    ~"]}".

-spec is_void(atom()) -> boolean().
is_void(_Tag) ->
    false.

-spec raw_text_kind(atom()) -> none | raw | escapable.
raw_text_kind(_Tag) ->
    %% The native wire is JSON, not HTML -- dynamic slots are `#slot` objects,
    %% not comment markers, so the raw-text corruption does not apply.
    none.

-spec scope_static(binary(), binary()) -> binary().
scope_static(Fp, S0) ->
    binary:replace(S0, <<"\"az\":\"">>, <<"\"az\":\"", Fp/binary, "-">>, [global]).

%% The native (`?native`) client does not implement `?OP_LIST_PATCH`; single-root
%% list eachs keep the wholesale re-render it already handles.
supports_list_patch() -> false.

%% Native output is a JSON wire, not text with an in-band escape vocabulary --
%% values are carried as JSON strings (encoded downstream), so nothing is escaped
%% here. Required by the `arizona_renderer` behaviour; the parse transform never
%% marks native values, so this is not reached via the escape marker.
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) -> Bin.

%% Render a dynamic attribute value: the prop name is baked into the static by
%% attr_dyn_name/1, so the dynamic carries just the stringified value (the client
%% JSON-encodes it like any other dynamic value). escape/1 is the identity here,
%% so escape_value/2 only classifies out a `?raw`/effect and stringifies.
-spec render_attr(binary(), term()) -> binary().
render_attr(_Name, Value) ->
    arizona_template:escape_value(?MODULE, Value).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec json_str(binary()) -> binary().
json_str(Bin) ->
    iolist_to_binary(json:encode(Bin)).
