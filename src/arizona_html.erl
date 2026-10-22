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
-export([attr_bool/2]).
-export([attr_command/2]).
-export([attr_dyn_name/1]).
-export([children_sep/0]).
-export([each_items/1]).
-export([text_child/1]).
-export([text_az/2]).
-export([text_slot_open/1]).
-export([text_slot_close/0]).
-export([is_void/1]).
-export([raw_text_kind/2]).
-export([content_context/2]).
-export([scope_static/3]).
-export([supports_list_patch/0]).
-export([target/0]).
-export([content_type/0]).
-export([supports_local/0]).
-export([single_root_document/0]).
-export([supports_nodiff_keep/0]).
-export([implied_attrs/2]).
-export([boolean_attrs/0]).
-export([attr_names/0]).
-export([escape/1]).
-export([raw_text/4]).
-export([render_attr/2]).
-export([wire_escape/1]).
-export([wire_attr/2]).

%% A byte that needs no entity. Letters and UTF-8 bytes (> `>`) answer on the
%% first test; the rest of the safe set lies between and around `"`, `&`/`'`
%% and `<`/`>`.
-define(SAFE(C),
    (C > $> orelse (C > $' andalso C < $<) orelse C < $" orelse C =:= $= orelse
        (C > $" andalso C < $&))
).
%% Past the first metacharacter, a tail shorter than this has no run long enough
%% for slicing to pay off, so the plain append loop takes it.
-define(SHORT_TAIL, 16).

-spec name(arizona_renderer:tag()) -> binary().
%% An ATOM name is translated `_` -> `-` so `az_click` needs no quoting. A BINARY
%% name is taken verbatim, which is the only way to write a name that really does
%% contain an underscore -- `~"my-widget_v2"` would otherwise come out
%% `my-widget-v2`, a different element, silently. Same two forms, same rule, for a
%% tag as for an attribute.
name(Bin) when is_binary(Bin) ->
    Bin;
name(Atom) ->
    binary:replace(atom_to_binary(Atom), ~"_", ~"-", [global]).

-spec element_open(binary()) -> binary().
element_open(TagName) ->
    %% `<plaintext>` switches the tokenizer to PLAINTEXT, which nothing leaves: the
    %% element's own close tag, the rest of the page and the client script after it
    %% would all render as text. No template can use it, so refuse it here, at
    %% compile time (the parse transform turns this into a located compile error).
    case string:equal(TagName, ~"plaintext", true) of
        true ->
            erlang:error(
                {arizona_render_reject, <<
                    "<plaintext> has no end tag: the browser reads everything after it, "
                    "the rest of the page included, as text. Use <pre> instead"
                >>}
            );
        false ->
            <<"<", TagName/binary>>
    end.

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

-spec attr_bool(binary(), boolean()) -> binary().
attr_bool(Name, true) ->
    <<" ", Name/binary>>;
attr_bool(_Name, false) ->
    <<>>.

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

-spec each_items([iodata()]) -> iodata().
each_items(Items) ->
    Items.

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

%% Every tag classification below matches on this, never on the atom as written:
%% HTML tag names are ASCII case-insensitive, so `{'BR', ...}` is the void element
%% `br` and `{'SCRIPT', ...}` is a script element as far as the browser is
%% concerned. Classifying only the lowercase atom emitted `<BR></BR>` (malformed --
%% a void element has no end tag) and handed an uppercase `<SCRIPT>` the
%% ordinary-element treatment (comment markers in the script, no opt-out guard).
%%
%% Only the *classification* folds case. `name/1` still emits the tag exactly as
%% written, which is what keeps a case-sensitive SVG attribute (`viewBox`) or a
%% camelCase SVG element intact -- normalizing the output would break those.
%%
%% Both callers run in the parse transform, at compile time, so the lowercasing
%% costs nothing at render. (`raw_text/4`'s own tag test is on the render path and
%% uses a comparison that allocates nothing -- see raw_text_mode/1.)
tag_name(Tag) when is_binary(Tag) ->
    ascii_lowercase(Tag);
tag_name(Tag) ->
    ascii_lowercase(atom_to_binary(Tag)).

%% ASCII only, as the browser folds names: `string:lowercase/1` maps the Kelvin
%% sign (U+212A) to `k`, which classified `lin\x{212A}` as the void `link` while
%% the browser sees an unknown element.
ascii_lowercase(Name) ->
    <<<<(ascii_lower(C))>> || <<C>> <= Name>>.

ascii_lower(C) when C >= $A, C =< $Z -> C + ($a - $A);
ascii_lower(C) -> C.

-spec is_void(arizona_renderer:tag()) -> boolean().
is_void(Tag) ->
    is_void_name(tag_name(Tag)).

is_void_name(~"area") -> true;
is_void_name(~"base") -> true;
is_void_name(~"br") -> true;
is_void_name(~"col") -> true;
is_void_name(~"embed") -> true;
is_void_name(~"hr") -> true;
is_void_name(~"img") -> true;
is_void_name(~"input") -> true;
is_void_name(~"link") -> true;
is_void_name(~"meta") -> true;
is_void_name(~"param") -> true;
is_void_name(~"source") -> true;
is_void_name(~"track") -> true;
is_void_name(~"wbr") -> true;
is_void_name(_Other) -> false.

-spec raw_text_kind(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:raw_text_kind().
raw_text_kind(Tag, Context) ->
    raw_text_kind_name(tag_name(Tag), Context).

%% Raw-text elements: content is never parsed for comments or character
%% references, so a dynamic slot must render verbatim and markerless.
%%
%% Only in HTML. Inside `<svg>` the parser is in foreign content, where `script`
%% and `style` content IS ordinary parsed markup: a comment there is a real
%% comment node, and a bare `<` starts an element. Classifying them `raw` there
%% was wrong twice over. It froze the slot for no reason, and -- the part that
%% matters -- `raw` forces the author to mark the value `?raw`, which splices it
%% verbatim; the neutralization behind that opt-out guards the `</script>` /
%% `</style>` breakout, which is not the breakout available in foreign content.
%% As `none` the slot is escaped instead, and the parser decodes the references
%% straight back (`a &gt; b` reaches CSS as `a > b`), so the content is unchanged
%% and the value can no longer escape its element.
raw_text_kind_name(~"script", html) ->
    raw;
%% Escapable-raw-text elements: character references are decoded, so a scalar
%% slot is HTML-escaped, but comments are still literal -- so still markerless.
%% Only in HTML: inside `<svg>` a `<title>` is ordinary parsed content whose
%% comments really are comments, so its slot can keep markers and stay diffable.
%% That matters because an SVG `<title>` is a graphic's accessible name, and
%% classifying it here would freeze a bound one after SSR.
raw_text_kind_name(~"textarea", html) ->
    escapable;
raw_text_kind_name(~"title", html) ->
    escapable;
raw_text_kind_name(Name, html) ->
    case lists:member(Name, rawtext_names()) of
        true -> raw;
        false -> none
    end;
raw_text_kind_name(_Other, _Context) ->
    none.

%% Every element the HTML tree builder switches into RAWTEXT for (the "generic raw
%% text element parsing algorithm"), `<script>` aside, which has its own script-data
%% states. `<xmp>` displays its content as text; the others' content is never
%% displayed by a browser that supports the element, but it is still RAWTEXT, so a
%% comment marker or an entity in it would be literal content and a value could
%% close it.
%%
%% Two near-relatives are deliberately absent. `<noscript>` is RAWTEXT only where
%% scripting is enabled, and there it is never displayed; it is displayed only where
%% scripting is off, and that parser reads it as ordinary HTML -- markers are
%% comments, entities decode, and no live view connects to patch it. Classifying it
%% `raw` would force `?raw` on its values and splice them unescaped into the HTML
%% that audience parses. Its flip side: `?raw` HTML inside a `<noscript>` is read as
%% RAWTEXT by a scripting-enabled browser, so sanitizer output carrying
%% `</noscript>` in an attribute value (`<p title="</noscript><img onerror=...>">`)
%% closes it there -- never put `?raw` markup in one. `<plaintext>` has no end tag
%% at all, so element_open/1 refuses it outright.
rawtext_names() ->
    [~"style", ~"xmp", ~"iframe", ~"noembed", ~"noframes"].

-spec content_context(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:content_context().
content_context(Tag, Parent) ->
    content_context_name(tag_name(Tag), Parent).

%% `<svg>` switches the parser into foreign content; `<foreignObject>` switches
%% back for its children. MathML is deliberately not tracked: it would need its
%% own integration points (`mtext` and friends) to be correct, and leaving it in
%% `html` keeps today's behaviour rather than half-handling it.
content_context_name(~"svg", _Parent) ->
    foreign;
content_context_name(~"foreignobject", foreign) ->
    html;
content_context_name(_Other, Parent) ->
    Parent.

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

content_type() -> ~"text/html; charset=utf-8".

supports_local() -> true.

single_root_document() -> false.

supports_nodiff_keep() -> true.

-spec implied_attrs(arizona_renderer:tag(), [binary()]) -> [{binary(), binary()}].
%% The client intercepts every `az-submit` submit, but only once arizona.js runs: a
%% submit before that (Enter while the deferred module still loads) is native, and a
%% method-less form submits as a GET -- every field, a password included, in the
%% URL. As a POST the fields stay in the body (a live route answers 405). The
%% client's `fetch` already treats a method-less form as POST, so nothing else moves.
implied_attrs(Tag, Names) ->
    case tag_name(Tag) of
        ~"form" ->
            Lower = [ascii_lowercase(N) || N <- Names],
            case lists:member(~"az-submit", Lower) andalso not lists:member(~"method", Lower) of
                true -> [{~"method", ~"post"}];
                false -> []
            end;
        _Other ->
            []
    end.

boolean_attrs() -> presence.

-spec attr_names() -> case_insensitive.
attr_names() -> case_insensitive.

%% HTML-escape the five metacharacters; safe for element content and
%% double/single-quoted attribute values. Comparisons are per byte, so UTF-8 is
%% safe (continuation bytes are all > 127, never a metacharacter).
%%
%% Almost every value rendered through here (a name, a number, a date) carries no
%% metacharacter at all, so locate the first one before building anything: with
%% none, the input is returned as-is -- no accumulator, no copy. The scan takes 8
%% (then 4) bytes per step under one guard, which the JIT compiles to plain byte
%% compares; that halves clean values of ~10-20 B and up and is ~2.5x on long
%% ones. Past the first metacharacter a short tail goes through the plain append
%% loop, and a longer one copies each clean run as one slice once two clean bytes
%% in a row suggest the run is long, so dense markup never pays for a chunk
%% attempt per byte. Measured shapes and the alternatives that lost
%% (binary:match/2, SWAR, an iodata result) are in docs/performance.md.
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) ->
    case first_meta(Bin, 0) of
        none ->
            Bin;
        Pos when byte_size(Bin) - Pos < ?SHORT_TAIL ->
            <<Clean:Pos/binary, Rest/binary>> = Bin,
            escape(Rest, Clean);
        Pos ->
            <<Clean:Pos/binary, Rest/binary>> = Bin,
            escape_runs(Rest, Clean, 0)
    end.

%% Byte offset of the first character needing an entity, `none` when there is none.
%% A chunk that fails holds a metacharacter (or is past the end), so the single
%% byte step after it hands over to a byte-only scan instead of retrying chunks.
%% ?SAFE and the advances must never over-accept: a position reported early is
%% only slower, one reported late leaves a metacharacter unescaped (XSS).
first_meta(<<A, B, C, D, E, F, G, H, R/binary>>, Pos) when
    ?SAFE(A), ?SAFE(B), ?SAFE(C), ?SAFE(D), ?SAFE(E), ?SAFE(F), ?SAFE(G), ?SAFE(H)
->
    first_meta(R, Pos + 8);
first_meta(<<A, B, C, D, R/binary>>, Pos) when ?SAFE(A), ?SAFE(B), ?SAFE(C), ?SAFE(D) ->
    first_meta(R, Pos + 4);
first_meta(<<C, R/binary>>, Pos) when ?SAFE(C) ->
    first_meta_bytes(R, Pos + 1);
first_meta(<<>>, _Pos) ->
    none;
first_meta(_Bin, Pos) ->
    Pos.

first_meta_bytes(<<C, R/binary>>, Pos) when ?SAFE(C) ->
    first_meta_bytes(R, Pos + 1);
first_meta_bytes(<<>>, _Pos) ->
    none;
first_meta_bytes(_Bin, Pos) ->
    Pos.

escape(<<>>, Acc) -> Acc;
escape(<<"&", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&amp;">>);
escape(<<"<", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&lt;">>);
escape(<<">", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&gt;">>);
escape(<<"\"", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&quot;">>);
escape(<<"'", R/binary>>, Acc) -> escape(R, <<Acc/binary, "&#39;">>);
escape(<<C, R/binary>>, Acc) -> escape(R, <<Acc/binary, C>>).

%% `Run` counts the clean bytes appended one by one since the last entity; at two,
%% the rest of the run is found with first_meta/2 and appended as one slice.
escape_runs(<<"&", R/binary>>, Acc, _Run) ->
    escape_runs(R, <<Acc/binary, "&amp;">>, 0);
escape_runs(<<"<", R/binary>>, Acc, _Run) ->
    escape_runs(R, <<Acc/binary, "&lt;">>, 0);
escape_runs(<<">", R/binary>>, Acc, _Run) ->
    escape_runs(R, <<Acc/binary, "&gt;">>, 0);
escape_runs(<<"\"", R/binary>>, Acc, _Run) ->
    escape_runs(R, <<Acc/binary, "&quot;">>, 0);
escape_runs(<<"'", R/binary>>, Acc, _Run) ->
    escape_runs(R, <<Acc/binary, "&#39;">>, 0);
escape_runs(<<C, R/binary>>, Acc, Run) when Run < 2 ->
    escape_runs(R, <<Acc/binary, C>>, Run + 1);
escape_runs(<<>>, Acc, _Run) ->
    Acc;
escape_runs(Bin, Acc, _Run) ->
    case first_meta(Bin, 0) of
        none ->
            <<Acc/binary, Bin/binary>>;
        Pos ->
            <<Clean:Pos/binary, Rest/binary>> = Bin,
            escape_runs(Rest, <<Acc/binary, Clean/binary>>, 0)
    end.

%% Neutralize a raw-text (script, style, ...) tokenizer breakout. Raw-text content is
%% emitted verbatim -- HTML entity-escaping does not apply (the browser decodes
%% nothing there) -- so a value spelling one of the sequences the HTML script-data
%% tokenizer reacts to escapes the element (the classic JSON-in-script XSS).
%% breakout/2 below defines that set; a value that legitimately needs one of them
%% inside a raw-text element is a breakout by definition.
%%
%% `Before` and `After` are the (at most 8) bytes of template static text right
%% before and right after the slot in the same element, `<<>>` where a dynamic or the
%% element boundary sits instead. The tokenizer reads static and value as one stream,
%% so a sequence can straddle them: `~"var s = '<"` then a value `/script>...` closes
%% the element though the value holds no `<`. A `<` inside the value is rewritten in
%% place; when the `<` is the static's, the value's FIRST byte -- the one that
%% completes the sequence -- is escaped instead (lead_break/4).
%%
%% The end of the known text is judged conservatively. `After` lets a value ending in
%% a partial sequence (`x</scr`) be judged against the static that finishes it; where
%% `After` runs out, the next bytes are unknown -- another slot's, decided only at
%% render time, or the element's own end -- so a STRICT PREFIX of a sequence there
%% (`<`, `</scr`, `<!`, `<!-`, `<scr`) counts as the sequence. That is what makes
%% each slot safe whatever its neighbours hold: a constant `?raw(~"x='<")` before
%% an attacker's `/script>`, two slots split by a static (`</scr`, `i`, `pt>`), or
%% three reaching the double escape (`<!`, `--<scr`, `ipt>`) all have their partial
%% defused in the slot that holds it.
%%
%% The covered shapes are every one `arizona_template:to_bin/1` can turn into
%% attacker-chosen bytes: a **binary**, an **atom** (`to_bin` renders it with
%% `atom_to_binary`, so an atom carries arbitrary bytes just like a binary),
%% **chardata**, and a `?raw` opt-out wrapping any of those. An integer and a float
%% stringify to digits and a sign, and an effect command's JSON is already
%% HTML-escaped by `arizona_effect:encode/1` (`<` -> `&lt;`), so none of them can
%% spell a sequence below; they pass through untouched, as does a map (a nested
%% template renders structurally through the escaping path, not as bytes).
%%
%% The template's own static text is not checked: it is the author's markup, so
%% statics that spell a sequence around an EMPTY value (`</scr`, `<<>>`, `ipt>`) are
%% the author's own close tag, and an empty value has no byte to escape anyway.
-spec raw_text(atom(), binary(), binary(), term()) -> term().
raw_text(Tag, Before, After, Value) ->
    %% Resolve the element's tokenizer state ONCE per slot, then thread the answer
    %% (not the tag) through the byte loop, so the per-character path stays a plain
    %% match and no atom is built at render time.
    raw_text_1({raw_text_mode(Tag), Before, After}, Value).

raw_text_1(Ctx, Value) when is_binary(Value) ->
    neutralize(Ctx, Value);
raw_text_1(Ctx, Value) when is_atom(Value) ->
    %% `to_bin/1` renders an atom with `atom_to_binary`, so its name reaches the
    %% output byte for byte -- an atom is as much a carrier as a binary. Normalize to
    %% the binary it would have become and neutralize that; the rendered bytes are
    %% identical either way.
    neutralize(Ctx, atom_to_binary(Value));
raw_text_1(Ctx, Value) ->
    %% A `?raw` opt-out is the *only* shape a raw-text content slot can carry (the
    %% parse transform rejects an unmarked one), and it opts out of HTML *escaping*,
    %% not out of the raw-text tokenizer: a trusted JSON blob's own string data can
    %% still spell a breakout. Unwrap it, neutralize the payload, and re-wrap, so the
    %% documented opt-out is not a hole around this check.
    case arizona_template:classify_trusted(Value) of
        {raw, Raw} -> arizona_template:raw(raw_text_1(Ctx, Raw));
        _Other -> raw_text_chardata(Ctx, Value)
    end.

%% How the tokenizer reads this element's content. `<script>` is read in the
%% **script data** states; every other raw-text element (`<style>`, `<xmp>`,
%% `<iframe>`, `<noembed>`, `<noframes>`) is plain RAWTEXT, whose sole exit is an end
%% tag carrying the element's OWN name -- so that name rides along (lowercased:
%% HTML tag names are ASCII case-insensitive, and the match below is too).
raw_text_mode(Tag) when is_atom(Tag) ->
    raw_text_mode(atom_to_binary(Tag));
raw_text_mode(Tag) ->
    case string:equal(Tag, ~"script", true) of
        true -> script;
        false -> {rawtext, rawtext_name(Tag, rawtext_names())}
    end.

%% The canonical lowercase name, found by a comparison that allocates nothing. Only
%% a tag raw_text_kind/2 classified `raw` reaches here, so the list always holds it.
rawtext_name(Tag, [Name | Names]) ->
    case string:equal(Tag, Name, true) of
        true -> Name;
        false -> rawtext_name(Tag, Names)
    end.

%% The documented remedy is `?raw(json:encode(Data))`, and `json:encode/1` returns
%% **iodata** -- so matching only binaries above would wave a breakout through on the
%% exact form this module's own error message recommends. Flatten chardata (a
%% charlist and a nested iolist alike) and neutralize the result, which is what the
%% render boundary would have produced anyway (`to_bin/1` flattens with the same
%% call). A list that is not chardata is returned untouched, leaving `to_bin/1` the
%% single place that names a bad template value.
raw_text_chardata(Ctx, Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> neutralize(Ctx, Bin);
        _NotChardata -> Value
    end;
raw_text_chardata(_Ctx, Value) ->
    Value.

neutralize({_Mode, _Lead, _After}, <<>>) ->
    <<>>;
neutralize({Mode, Lead, After}, Bin) ->
    Out = neutralize_raw_text(Mode, Bin, After, <<>>),
    case lead_break(Mode, Lead, Bin, After) of
        true ->
            %% The first byte continues a sequence begun after `<`, so it is never a
            %% `<` itself and the loop above emitted it unchanged.
            <<C, Rest/binary>> = Out,
            <<(escape_byte(Mode, C))/binary, Rest/binary>>;
        false ->
            Out
    end.

neutralize_raw_text(_Mode, <<>>, _After, Acc) ->
    Acc;
neutralize_raw_text(Mode, <<"<", R/binary>>, After, Acc) ->
    Lt = raw_text_breakout(Mode, tail_with(R, After)),
    neutralize_raw_text(Mode, R, After, <<Acc/binary, Lt/binary>>);
neutralize_raw_text(Mode, <<C, R/binary>>, After, Acc) ->
    neutralize_raw_text(Mode, R, After, <<Acc/binary, C>>).

%% A `<` near the value's end reads on into the following static. No sequence after
%% `<` takes more than 8 bytes, so a farther `<` never reaches it, and 8 bytes of
%% `After` always decide the sequence before they run out.
tail_with(R, After) when byte_size(R) < 8 ->
    <<R/binary, After/binary>>;
tail_with(R, _After) ->
    R.

%% Does a `<` among the static's trailing bytes begin a sequence that runs into the
%% value? One that ends inside the static is the template's own text, not this
%% slot's to break. The text after the `<` is only assembled once a `<` is found,
%% and from the value's first 8 bytes, not a copy of the whole value.
lead_break(Mode, <<"<", S/binary>>, Bin, After) ->
    Head = binary_part(Bin, 0, min(8, byte_size(Bin))),
    case breakout(Mode, <<S/binary, Head/binary, After/binary>>) of
        {_Kind, Span} when Span > byte_size(S) -> true;
        _NoneOrStatic -> lead_break(Mode, S, Bin, After)
    end;
lead_break(Mode, <<_C, S/binary>>, Bin, After) ->
    lead_break(Mode, S, Bin, After);
lead_break(_Mode, <<>>, _Bin, _After) ->
    false.

%% The value's first byte, escaped so the sequence no longer reads as one. In a
%% script it is the JSON/JavaScript `\u00XX`, in a style a CSS hex escape (which
%% consumes its terminating space): both decode back to the byte. The other RAWTEXT
%% elements decode nothing, so no escape is transparent there; a backslash before
%% the byte breaks the sequence. `<iframe>`, `<noembed>` and `<noframes>` content is
%% never displayed, and `<xmp>` shows the backslash -- only in text that would
%% otherwise have closed it. Every byte that can complete a sequence is ASCII.
escape_byte(script, C) ->
    <<"\\u00", (binary:encode_hex(<<C>>, lowercase))/binary>>;
escape_byte({rawtext, ~"style"}, C) ->
    <<"\\", (binary:encode_hex(<<C>>, lowercase))/binary, " ">>;
escape_byte({rawtext, _Name}, C) ->
    <<"\\", C>>.

%% What a `<` inside the value becomes, given the text right after it.
raw_text_breakout(Mode, R) ->
    case breakout(Mode, R) of
        nomatch -> ~"<";
        {close, _Span} -> ~"<\\";
        {escape, _Span} -> ~"\\u003c"
    end.

%% Which sequence, if any, the text after a `<` spells, and how many of its bytes it
%% takes. Three sequences move the HTML tokenizer out of raw text:
%%
%%   `</script` / `</style` / `</xmp` ... -- ends the element, dropping the rest of
%%       the value into HTML parsing. An end tag only closes a raw-text element when
%%       it is the *appropriate* one -- its name matches the element being parsed --
%%       so strictly only `</script` matters in `<script>` and only `</style` in
%%       `<style>`; the other is ordinary text there. Both names are neutralized in
%%       both of those anyway: it is a harmless superset (the rewrite decodes back to
%%       the original in the string contexts their content lives in), and it keeps
%%       that half of the rule independent of the script-data half below. The other
%%       RAWTEXT elements (`<xmp>`, `<iframe>`, `<noembed>`, `<noframes>`) check only
%%       their own name: nothing decodes there, so a rewrite is never transparent, and
%%       `<xmp>` displays its content -- the superset would put a visible backslash in
%%       text that could not have closed it (end_tag_name/2).
%%       A backslash after the `<` stops the end-tag match: `<\/script` is transparent
%%       wherever script/style content lives (`\/` decodes to `/` in JSON, in a
%%       JavaScript string, and in CSS); in `<xmp>` it shows.
%%   `<!--` -- enters script-data-escaped state, where a following `<script`
%%       reaches script-data-double-escaped and the element's OWN `</script>` no
%%       longer closes it: the remainder of the document is swallowed, so
%%       neutralizing only the close tag above is not enough.
%%   `<script` -- inert from plain script-data state, but it is the second half of
%%       that double escape, and the first half can come from the template's own
%%       static text (the legacy `<script><!-- ... //--></script>` idiom), which is
%%       spliced verbatim and never passes through here.
%%
%% A name counts only where the tokenizer reads one (tag_name/2): `i<scripts.length`
%% is ordinary JavaScript, not a tag, and is left alone. At the end of the known text
%% a strict prefix of a sequence counts (see the end-of-text rule above): the text
%% runs out at `<`, `</`, `</scr`, `<!`, `<!-` or `<scr`, and whatever the next slot
%% holds may finish it.
%%
%% The last two replace the `<` with the escape `\u003c` instead of inserting a
%% backslash: `\!` and `\s` are not valid JSON escapes, and a JSON blob is the
%% documented content of a `raw` raw-text slot, whereas `\u003c` is valid in both
%% JSON and JavaScript strings and decodes back to `<`.
%%
%% They also apply ONLY in `<script>` (`Mode =:= script`). `<style>` content is RAWTEXT,
%% which has no escaped state at all: rewriting these there would defend nothing
%% and would corrupt the stylesheet, because that escape is a JS/JSON one which a
%% CSS parser reads as the identifier bytes `u003c` -- and `<!--`/`-->` are
%% themselves legitimate CSS tokens (CDO/CDC). Tag names match ASCII
%% case-insensitively, as the tokenizer does; any other `<` keeps its own byte.
breakout(Mode, <<$/, R/binary>>) ->
    case end_tag_name(Mode, R) of
        nomatch -> nomatch;
        Len -> {close, 1 + Len}
    end;
breakout(script, <<"!--", _R/binary>>) ->
    {escape, 3};
breakout(script, <<"!-">>) ->
    {escape, 2};
breakout(script, <<"!">>) ->
    {escape, 1};
breakout(script, R) ->
    case tag_name(R, ~"script") of
        nomatch -> nomatch;
        Len -> {escape, Len}
    end;
%% A bare `<` ending the text: in RAWTEXT only an end tag follows it.
breakout({rawtext, _Name}, <<>>) ->
    {close, 0};
breakout({rawtext, _Name}, _R) ->
    nomatch.

%% Which end tags close this element, as end_tag_name/2's bytes-taken or `nomatch`.
end_tag_name({rawtext, Name}, R) when Name =/= ~"style" ->
    tag_name(R, Name);
end_tag_name(_ScriptOrStyle, R) ->
    case tag_name(R, ~"script") of
        nomatch -> tag_name(R, ~"style");
        Len -> Len
    end.

%% Does `Bin` start with the tag name `Pattern` as the tokenizer reads one: the name,
%% then whitespace (CR too -- input preprocessing turns it into LF), `/` or `>`? The
%% end of the text counts as well, conservatively, wherever it falls -- after the
%% name (the delimiter may come next) or inside it (so may the rest of the name).
%% Returns the bytes taken or `nomatch`. The pattern is always lowercase letters.
tag_name(Bin, Pattern) ->
    tag_name(Bin, Pattern, 0).

tag_name(<<>>, _Pattern, N) ->
    N;
tag_name(<<D, _/binary>>, <<>>, N) when
    D =:= $\t; D =:= $\n; D =:= $\f; D =:= $\r; D =:= $\s; D =:= $/; D =:= $>
->
    N + 1;
tag_name(<<C, R/binary>>, <<P, PR/binary>>, N) when C =:= P; C =:= P - 32 ->
    tag_name(R, PR, N + 1);
tag_name(_Bin, _Pattern, _N) ->
    nomatch.

%% Render a dynamic attribute value: `false` strips the attribute, `true` emits a
%% bare name. A `?raw` opt-out and an effect command are trusted (classified out
%% via arizona_template:classify_trusted/1 and emitted verbatim, mirroring
%% escape_value/2); any other scalar goes through attr/2, which entity-escapes
%% it -- the one attribute-value escaping boundary.
-spec render_attr(binary(), term()) -> binary().
render_attr(_Name, false) ->
    <<>>;
render_attr(Name, true) ->
    attr_bool(Name, true);
render_attr(Name, Value) ->
    case arizona_template:classify_trusted(Value) of
        {raw, V} ->
            attr_unescaped(Name, arizona_template:to_bin(V));
        {effect, Cmd} ->
            %% A live render already observed this name where it evaluated the
            %% value (arizona_eval). The request-free SSR walk evaluates inline
            %% and never reaches arizona_eval, so this is where a LAYOUT's opaque
            %% command is caught: unarmed, observe_attr/1 warns that no frame can
            %% ever deliver it.
            ok = arizona_event_attrs:observe_attr(Name),
            attr_command(Name, Cmd);
        value ->
            attr(Name, arizona_template:to_bin(Value))
    end.

%% The browser splices the fingerprint payload as text, so the wire carries what
%% the document does.
-spec wire_escape(binary()) -> binary().
wire_escape(Bin) ->
    escape(Bin).

-spec wire_attr(binary(), term()) -> binary().
wire_attr(Name, Value) ->
    render_attr(Name, Value).
