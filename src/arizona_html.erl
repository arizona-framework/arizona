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
-export([scope_static/3]).
-export([supports_list_patch/0]).
-export([target/0]).
-export([supports_local/0]).
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

%% Every framework-emitted `az` in a compiled static is `<Fp>-<id>` (the parse
%% transform builds the marker from the id, so the fingerprint is always the
%% leading component), which makes the fingerprint the anchor that separates a
%% real marker from user-authored bytes. Matching ` az="` / `<!--az:` alone
%% would rewrite a static text child that merely *shows* markup -- static text
%% is spliced verbatim (the raw-HTML seam), so a page about Arizona itself
%% carries those sequences as ordinary content.
-spec scope_static(binary(), binary(), binary()) -> binary().
scope_static(Fp, Prefix, S0) ->
    S1 = binary:replace(
        S0, <<" az=\"", Fp/binary>>, <<" az=\"", Prefix/binary, "-", Fp/binary>>, [global]
    ),
    binary:replace(
        S1, <<"<!--az:", Fp/binary>>, <<"<!--az:", Prefix/binary, "-", Fp/binary>>, [global]
    ).

%% The web client implements `?OP_LIST_PATCH` (positional single-root plain-list
%% `?each` diffing), so single-root list items are flagged for it.
supports_list_patch() -> true.

target() -> html.

supports_local() -> true.

%% HTML-escape the five metacharacters; safe for element content and
%% double/single-quoted attribute values. Byte-at-a-time over the tail is
%% UTF-8 safe (continuation bytes are all > 127, never a metacharacter).
%%
%% Almost every value rendered through here (a name, a number, a date) carries no
%% metacharacter at all, so locate the first one before building anything: with
%% none, the input is returned as-is -- no accumulator, no copy. Only a value that
%% really needs an entity allocates, and only from that first metacharacter on
%% (the clean prefix seeds the accumulator in one slice). Measured over 200k calls,
%% a clean value is ~5x the plain accumulator loop at every size tried (5 B to
%% 1.2 KB), ~3x when the first metacharacter is near the end, and at parity when
%% the value is escape-dense. A word-at-a-time (SWAR) scan was measured too: ~2x
%% faster again on pure ASCII but slower on multi-byte UTF-8 and far harder to
%% read, so the plain scan wins on the balance.
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) ->
    case first_meta(Bin, 0) of
        none ->
            Bin;
        Pos ->
            <<Clean:Pos/binary, Rest/binary>> = Bin,
            escape(Rest, Clean)
    end.

%% Byte offset of the first character needing an entity, `none` when there is none.
first_meta(<<C, R/binary>>, Pos) when C =/= $&, C =/= $<, C =/= $>, C =/= $", C =/= $' ->
    first_meta(R, Pos + 1);
first_meta(<<>>, _Pos) ->
    none;
first_meta(_Bin, Pos) ->
    Pos.

escape(<<>>, Acc) -> Acc;
escape(<<"&", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&amp;">>);
escape(<<"<", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&lt;">>);
escape(<<">", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&gt;">>);
escape(<<"\"", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&quot;">>);
escape(<<"'", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&#39;">>);
escape(<<C, R/binary>>, Acc) -> escape(R, <<Acc/binary, C>>).

%% Neutralize a raw-text (script/style) tokenizer breakout. Raw-text content is
%% emitted verbatim -- HTML entity-escaping does not apply (the browser decodes
%% nothing there) -- so a value spelling one of the sequences the HTML script-data
%% tokenizer reacts to escapes the element (the classic JSON-in-script XSS).
%% raw_text_breakout/1 below defines that set and what each `<` becomes; a value
%% that legitimately needs one of them inside a raw-text element is a breakout by
%% definition. Non-binary values (a nested template, an integer) cannot carry a
%% sequence and pass through unchanged.
-spec raw_text(term()) -> term().
raw_text(Value) when is_binary(Value) ->
    neutralize_raw_text(Value, <<>>);
raw_text(Value) ->
    %% A `?raw` opt-out is the *only* shape a script/style content slot can carry
    %% (the parse transform rejects an unmarked one), and it opts out of HTML
    %% *escaping*, not out of the raw-text tokenizer: a trusted JSON blob's own
    %% string data can still spell a breakout. Unwrap it, neutralize the payload,
    %% and re-wrap, so the documented opt-out is not a hole around this check.
    case arizona_template:classify_trusted(Value) of
        {raw, Raw} -> arizona_template:raw(raw_text(Raw));
        _Other -> raw_text_chardata(Value)
    end.

%% The documented remedy is `?raw(json:encode(Data))`, and `json:encode/1` returns
%% **iodata** -- so matching only binaries above would wave a breakout through on the
%% exact form this module's own error message recommends. Flatten chardata (a
%% charlist and a nested iolist alike) and neutralize the result, which is what the
%% render boundary would have produced anyway (`to_bin/1` flattens with the same
%% call). A list that is not chardata is returned untouched, leaving `to_bin/1` the
%% single place that names a bad template value.
raw_text_chardata(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> neutralize_raw_text(Bin, <<>>);
        _NotChardata -> Value
    end;
raw_text_chardata(Value) ->
    Value.

neutralize_raw_text(<<>>, Acc) ->
    Acc;
neutralize_raw_text(<<"<", R/binary>>, Acc) ->
    Lt = raw_text_breakout(R),
    neutralize_raw_text(R, <<Acc/binary, Lt/binary>>);
neutralize_raw_text(<<C, R/binary>>, Acc) ->
    neutralize_raw_text(R, <<Acc/binary, C>>).

%% What the `<` becomes, given the text right after it. Three sequences move the
%% HTML script-data tokenizer, and each is defused where it starts:
%%
%%   `</script` / `</style` -- ends the element, dropping the rest of the value
%%       into HTML parsing. A backslash after the `<` stops the end-tag match:
%%       `<\/script` is transparent wherever such content lives (`\/` decodes to
%%       `/` in JSON and in a JavaScript string).
%%   `<!--` -- enters script-data-escaped state, where a following `<script`
%%       reaches script-data-double-escaped and the element's OWN `</script>` no
%%       longer closes it: the remainder of the document is swallowed, so
%%       neutralizing only the close tag above is not enough.
%%   `<script` -- inert from plain script-data state, but it is the second half of
%%       that double escape, and the first half can come from the template's own
%%       static text (the legacy `<script><!-- ... //--></script>` idiom), which is
%%       spliced verbatim and never passes through here.
%%
%% The last two replace the `<` with the escape `\u003c` instead of inserting a
%% backslash: `\!` and `\s` are not valid JSON escapes, and a JSON blob is the
%% documented content of a `raw` raw-text slot, whereas `\u003c` is valid in both
%% JSON and JavaScript strings and decodes back to `<`. Tag names match ASCII
%% case-insensitively, as the tokenizer does; any other `<` keeps its own byte.
raw_text_breakout(<<$/, R/binary>>) ->
    case ci_prefix(R, ~"script") orelse ci_prefix(R, ~"style") of
        true -> ~"<\\";
        false -> ~"<"
    end;
raw_text_breakout(<<"!--", _R/binary>>) ->
    ~"\\u003c";
raw_text_breakout(R) ->
    case ci_prefix(R, ~"script") of
        true -> ~"\\u003c";
        false -> ~"<"
    end.

%% Case-insensitive (ASCII) prefix match; the pattern is always lowercase letters.
ci_prefix(_Bin, <<>>) -> true;
ci_prefix(<<>>, _Pattern) -> false;
ci_prefix(<<C, R/binary>>, <<P, PR/binary>>) when C =:= P; C =:= P - 32 -> ci_prefix(R, PR);
ci_prefix(_Bin, _Pattern) -> false.

%% Render a dynamic attribute value: `false` strips the attribute, `true` emits a
%% bare name. A `?raw` opt-out and an effect command are trusted (classified out
%% via arizona_template:classify_trusted/1 and emitted verbatim, mirroring
%% escape_value/2); any other scalar goes through attr/2, which entity-escapes
%% it -- the one attribute-value escaping boundary.
-spec render_attr(binary(), term()) -> binary().
render_attr(_Name, false) ->
    <<>>;
render_attr(Name, true) ->
    attr_boolean(Name);
render_attr(Name, Value) ->
    case arizona_template:classify_trusted(Value) of
        {raw, V} -> attr_unescaped(Name, arizona_template:to_bin(V));
        {effect, Cmd} -> attr_command(Name, Cmd);
        value -> attr(Name, arizona_template:to_bin(Value))
    end.
