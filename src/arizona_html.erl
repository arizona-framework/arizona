-module(arizona_html).
-moduledoc """
HTML render-target backend.

Emits the byte sequences that make up the `s` statics for HTML output:
element tags, attributes, and the comment markers that delimit dynamic text
slots. Extracted verbatim from the parse transform -- output is byte-for-byte
identical to the previous inlined emission.
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

-spec name(atom()) -> binary().
name(Atom) ->
    binary:replace(atom_to_binary(Atom), ~"_", ~"-", [global]).

-spec element_open(binary()) -> binary().
element_open(TagName) ->
    <<"<", TagName/binary>>.

-spec az_attr(binary()) -> binary().
az_attr(Az) ->
    <<" az=\"", Az/binary, "\"">>.

-spec element_open_end() -> binary().
element_open_end() ->
    ~">".

-spec element_void_close() -> binary().
element_void_close() ->
    ~" />".

-spec element_close(binary()) -> binary().
element_close(TagName) ->
    <<"</", TagName/binary, ">">>.

-spec attr(binary(), binary()) -> binary().
attr(Name, Value) ->
    <<" ", Name/binary, "=\"", Value/binary, "\"">>.

-spec attr_boolean(binary()) -> binary().
attr_boolean(Name) ->
    <<" ", Name/binary>>.

-spec attr_command(binary(), term()) -> binary().
attr_command(Name, Cmd) ->
    %% A folded effect command: its JSON, escaped for the HTML attribute
    %% context, as a normal name="value" attribute.
    attr(Name, arizona_effect:encode(Cmd)).

-spec attr_dyn_name(binary()) -> binary().
attr_dyn_name(_Name) ->
    %% HTML keeps the name in the dynamic (rendered by render_attr); nothing
    %% is baked into the static.
    <<>>.

-spec children_sep() -> binary().
children_sep() ->
    <<>>.

-spec text_child(binary()) -> binary().
text_child(Text) ->
    Text.

-spec text_az(binary(), non_neg_integer()) -> binary().
text_az(ElemAz, 0) ->
    ElemAz;
text_az(ElemAz, Slot) ->
    <<ElemAz/binary, ":", (integer_to_binary(Slot))/binary>>.

-spec text_slot_open(binary()) -> binary().
text_slot_open(Az) ->
    <<"<!--az:", Az/binary, "-->">>.

-spec text_slot_close() -> binary().
text_slot_close() ->
    ~"<!--/az-->".

-spec is_void(atom()) -> boolean().
is_void(area) -> true;
is_void(base) -> true;
is_void(br) -> true;
is_void(col) -> true;
is_void(embed) -> true;
is_void(hr) -> true;
is_void(img) -> true;
is_void(input) -> true;
is_void(link) -> true;
is_void(meta) -> true;
is_void(param) -> true;
is_void(source) -> true;
is_void(track) -> true;
is_void(wbr) -> true;
is_void(_) -> false.

-spec raw_text_kind(atom()) -> none | raw | escapable.
%% Raw-text elements: content is never parsed for comments or character
%% references, so a dynamic slot must render verbatim and markerless.
raw_text_kind(script) -> raw;
raw_text_kind(style) -> raw;
%% Escapable-raw-text elements: character references are decoded, so a scalar
%% slot is HTML-escaped, but comments are still literal -- so still markerless.
raw_text_kind(textarea) -> escapable;
raw_text_kind(title) -> escapable;
raw_text_kind(_) -> none.

-spec scope_static(binary(), binary()) -> binary().
scope_static(Fp, S0) ->
    S1 = binary:replace(S0, <<" az=\"">>, <<" az=\"", Fp/binary, "-">>, [global]),
    binary:replace(S1, <<"<!--az:">>, <<"<!--az:", Fp/binary, "-">>, [global]).

%% The web client implements `?OP_LIST_PATCH` (positional single-root plain-list
%% `?each` diffing), so single-root list items are flagged for it.
supports_list_patch() -> true.
