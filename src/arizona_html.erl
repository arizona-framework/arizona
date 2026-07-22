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
-export([escape/1]).
-export([raw_text/1]).
-export([render_attr/2]).

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
    %% HTML-escape the value: an attribute value is text, so a literal `"`/`&`/`<`
    %% (from a static template literal or a dynamic scalar) must be entity-escaped
    %% or it terminates/breaks the attribute. This is the sole escaping boundary
    %% for attribute values -- render_attr/2 hands plain scalars straight here and
    %% routes the trusted cases (a `?raw` opt-out, a pre-escaped effect command)
    %% through attr_unescaped/2 instead, so nothing is double-escaped.
    attr_unescaped(Name, escape(Value)).

%% Emit ` Name="Value"` with the value spliced verbatim -- for values that are
%% already safe for the attribute context (an effect command's JSON, escaped by
%% arizona_effect:encode/1, or a trusted `?raw` opt-out).
-spec attr_unescaped(binary(), binary()) -> binary().
attr_unescaped(Name, Value) ->
    <<" ", Name/binary, "=\"", Value/binary, "\"">>.

-spec attr_boolean(binary()) -> binary().
attr_boolean(Name) ->
    <<" ", Name/binary>>.

-spec attr_command(binary(), term()) -> binary().
attr_command(Name, Cmd) ->
    %% A folded effect command: its JSON, escaped for the HTML attribute
    %% context by arizona_effect:encode/1, as a normal name="value" attribute --
    %% emitted verbatim so the already-escaped JSON is not escaped twice.
    attr_unescaped(Name, arizona_effect:encode(Cmd)).

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
    %% Static text is spliced verbatim: it is the documented raw-HTML seam (a
    %% layout emits `~"<!DOCTYPE html>"` / literal fragments as static text), so it
    %% is deliberately NOT escaped here. Escaped element content comes from dynamic
    %% values (the escape/1 boundary); `?raw` is the opt-out for trusted dynamic
    %% HTML. Only static attribute *values* are escaped (attr/2), where a literal
    %% `"` would otherwise terminate the attribute with no raw-splice use case.
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

%% HTML-escape the five metacharacters; safe for element content and
%% double/single-quoted attribute values. Byte-at-a-time over the tail is
%% UTF-8 safe (continuation bytes are all > 127, never a metacharacter).
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) ->
    escape(Bin, <<>>).

escape(<<>>, Acc) -> Acc;
escape(<<"&", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&amp;">>);
escape(<<"<", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&lt;">>);
escape(<<">", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&gt;">>);
escape(<<"\"", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&quot;">>);
escape(<<"'", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&#39;">>);
escape(<<C, R/binary>>, Acc) -> escape(R, <<Acc/binary, C>>).

%% Neutralize a raw-text (script/style) close-tag breakout. Raw-text content is
%% emitted verbatim -- HTML entity-escaping does not apply (the browser decodes
%% nothing there) -- but a value carrying `</script>`/`</style>` would still close
%% the element and drop into HTML parsing (the classic JSON-in-script XSS). Insert
%% a backslash after the `<` of a case-insensitive `</script`/`</style`, so the
%% raw-text tokenizer never matches the end tag. `<\/script` is transparent in the
%% string/JSON/CSS contexts such content lives in (`\/` decodes to `/`), and a
%% value that legitimately needs a literal `</script` in a raw-text element is a
%% breakout by definition. Only the two raw-text tag names close a raw-text
%% element, so nothing else is touched. Non-binary values (a nested template, an
%% integer) cannot carry the sequence and pass through unchanged.
-spec raw_text(term()) -> term().
raw_text(Value) when is_binary(Value) ->
    neutralize_raw_text(Value, <<>>);
raw_text(Value) ->
    Value.

neutralize_raw_text(<<>>, Acc) ->
    Acc;
neutralize_raw_text(<<"<", R/binary>>, Acc) ->
    case raw_text_breakout(R) of
        true -> neutralize_raw_text(R, <<Acc/binary, "<\\">>);
        false -> neutralize_raw_text(R, <<Acc/binary, "<">>)
    end;
neutralize_raw_text(<<C, R/binary>>, Acc) ->
    neutralize_raw_text(R, <<Acc/binary, C>>).

%% Does the text right after a `<` begin a `/script` or `/style` end tag (ASCII
%% case-insensitive)? Only these close a raw-text element.
raw_text_breakout(<<$/, R/binary>>) ->
    ci_prefix(R, <<"script">>) orelse ci_prefix(R, <<"style">>);
raw_text_breakout(_) ->
    false.

%% Case-insensitive (ASCII) prefix match; the pattern is always lowercase letters.
ci_prefix(_Bin, <<>>) -> true;
ci_prefix(<<>>, _Pattern) -> false;
ci_prefix(<<C, R/binary>>, <<P, PR/binary>>) when C =:= P; C =:= P - 32 -> ci_prefix(R, PR);
ci_prefix(_Bin, _Pattern) -> false.

%% Render a dynamic attribute value: `false` strips the attribute, `true` emits a
%% bare name. A `?raw` opt-out and an effect command are trusted (classified out
%% and emitted verbatim, mirroring escape_value/2); any other scalar goes through
%% attr/2, which entity-escapes it -- the one attribute-value escaping boundary.
-spec render_attr(binary(), term()) -> binary().
render_attr(_Name, false) -> <<>>;
render_attr(Name, true) -> attr_boolean(Name);
render_attr(Name, {arizona_raw, V}) -> attr_unescaped(Name, arizona_template:to_bin(V));
render_attr(Name, {arizona_effect, _} = Cmd) -> attr_command(Name, Cmd);
render_attr(Name, [{arizona_effect, _} | _] = Cmd) -> attr_command(Name, Cmd);
render_attr(Name, Value) -> attr(Name, arizona_template:to_bin(Value)).
