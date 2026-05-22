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
-export([children_sep/0]).
-export([text_child/1]).
-export([text_slot_open/1]).
-export([text_slot_close/0]).
-export([is_void/1]).
-export([scope_static/2]).

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

-spec children_sep() -> binary().
children_sep() ->
    ~",".

-spec text_child(binary()) -> binary().
text_child(Text) ->
    json_str(Text).

-spec text_slot_open(binary()) -> binary().
text_slot_open(Az) ->
    <<"{\"type\":\"#text\",\"az\":", (json_str(Az))/binary, ",\"value\":">>.

-spec text_slot_close() -> binary().
text_slot_close() ->
    ~"}".

-spec is_void(atom()) -> boolean().
is_void(_Tag) ->
    false.

-spec scope_static(binary(), binary()) -> binary().
scope_static(Fp, S0) ->
    binary:replace(S0, <<"\"az\":\"">>, <<"\"az\":\"", Fp/binary, "-">>, [global]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec json_str(binary()) -> binary().
json_str(Bin) ->
    iolist_to_binary(json:encode(Bin)).
