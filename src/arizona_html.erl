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
-export([live_root/0]).
-export([content_type/0]).
-export([supports_local/0]).
-export([single_root_document/0]).
-export([supports_nodiff_keep/0]).
-export([keyed_item_ops/0]).
-export([implied_attrs/2]).
-export([selection_role/1]).
-export([mark_choices/2]).
-export([static_text/1]).
-export([boolean_attrs/0]).
-export([attr_names/0]).
-export([escape/1]).
-export([raw_text/4]).
-export([render_attr/2]).
-export([wire_escape/1]).
-export([wire_attr/2]).
-export([static_document/2]).

-define(IS_ALPHA(C), ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z))).
-define(IS_ALNUM(C), (?IS_ALPHA(C) orelse (C >= $0 andalso C =< $9))).
-define(IS_SPACE(C),
    (C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\f orelse C =:= $\r)
).
%% The longest named character reference valid without its `;`.
-define(LEGACY_REF_MAX, 6).

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
raw_text_kind_name(~"script", undefined) ->
    raw;
%% Escapable-raw-text elements: character references are decoded, so a scalar
%% slot is HTML-escaped, but comments are still literal -- so still markerless.
%% Only in HTML: inside `<svg>` a `<title>` is ordinary parsed content whose
%% comments really are comments, so its slot can keep markers and stay diffable.
%% That matters because an SVG `<title>` is a graphic's accessible name, and
%% classifying it here would freeze a bound one after SSR.
raw_text_kind_name(~"textarea", undefined) ->
    escapable;
raw_text_kind_name(~"title", undefined) ->
    escapable;
raw_text_kind_name(Name, undefined) ->
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
%% HTML content keeps today's behaviour rather than half-handling it. HTML content
%% is the root context, `undefined`, so `<foreignObject>` returns to it.
content_context_name(~"svg", _Parent) ->
    foreign;
content_context_name(~"foreignobject", foreign) ->
    undefined;
content_context_name(_Other, Parent) ->
    Parent.

-spec static_document(iodata(), [binary()]) -> iodata().
static_document(Doc, Names) ->
    %% Before the last `</body>`, where the static client reads it. A document with
    %% none (a fragment) is left as rendered: appending markup could corrupt it.
    %% `<` is escaped so no name can close the script element.
    Html = iolist_to_binary(Doc),
    case binary:matches(Html, ~"</body>") of
        [] ->
            Html;
        Matches ->
            {At, _Len} = lists:last(Matches),
            Json = binary:replace(iolist_to_binary(json:encode(Names)), ~"<", ~"\\u003c", [global]),
            <<Before:At/binary, After/binary>> = Html,
            [
                Before,
                ~"<script type=\"application/json\" id=\"arizona-attrs\">",
                Json,
                ~"</script>",
                After
            ]
    end.

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

live_root() -> true.

content_type() -> ~"text/html; charset=utf-8".

supports_local() -> true.

single_root_document() -> false.

supports_nodiff_keep() -> true.

keyed_item_ops() -> true.

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

-spec selection_role(arizona_renderer:tag()) ->
    none
    | {chooser, #{value := binary(), multiple := binary(), choice := binary()}}
    | {choice, #{value := binary(), selected := binary()}}.
%% `<select>` has no `value` attribute: the parser ignores it and selects the first
%% option, so the page would load on one option and an update select another. The
%% option's own `selected` is what the parser reads.
selection_role(Tag) ->
    case tag_name(Tag) of
        ~"select" ->
            {chooser, #{value => ~"value", multiple => ~"multiple", choice => ~"option"}};
        ~"option" ->
            {choice, #{value => ~"value", selected => ~"selected"}};
        _Other ->
            none
    end.

-spec static_text(binary()) -> {ok, binary()} | error.
%% Static content is spliced as markup: its text is what the parser makes of it,
%% references decoded, unless it opens a tag (or anything else `<` starts).
static_text(Markup) ->
    case binary:match(Markup, ~"<") of
        nomatch -> {ok, decode_refs(Markup, text)};
        _Tag -> error
    end.

-spec mark_choices(iodata(), [binary()]) -> iodata().
%% `Fragment` is what a child the parse transform could not see into rendered under a
%% `<select>` with a value (a component, a runtime value, raw markup). Mark `selected`
%% every `<option>` in it whose value is one of `Picks`, so the page loads on it: its
%% `value` attribute, else its text with whitespace stripped and collapsed, both read
%% as the parser reads them (newlines and NUL preprocessed, character references
%% decoded from the full named set), the picks preprocessed as attribute text too.
%%
%% The fragment is tokenized the way the parser would, so markup that only looks like
%% an option is left alone: attribute values, comments, bogus comments, the content of
%% raw-text elements (raw_text_kind/2) and of `<template>`, and, in foreign content
%% (content_context/2), CDATA sections; an `option` there is not an HTML option either.
%% What closes the select (`</select>`, or a `select`/`input`/`keygen`/`textarea` start
%% tag) ends the walk: an option after it is not this select's. A `selected` the
%% markup already carries is the value's to decide, as the client decides it: kept on
%% an option the value picks, cut from any other.
%%
%% Raw text is told by the in-body rules (raw_text_kind/2). The classic in-select
%% parsing switches tokenization only for `script` and `template` and ignores the other
%% raw-text, `svg` and `math` start tags, so an option written inside one of those
%% there is a real one this walk leaves unmarked; the client marks it once it applies
%% the value. An option whose text runs on past the fragment (into the template's
%% static markup after the slot) is valued by the part inside it.
mark_choices(Fragment, Picks) ->
    Bin = iolist_to_binary(Fragment),
    %% The picks render as the select's attribute, so they read as one would.
    Texts = [preprocess(P) || P <- Picks],
    Picked = fun(Text) -> lists:member(Text, Texts) end,
    case choices(Bin, 0, {undefined, []}, Picked, []) of
        [] -> Fragment;
        Marks -> splice_selected(Bin, lists:keysort(1, Marks), 0, [])
    end.

choices(Bin, Pos, Ctx, Picked, Marks) ->
    case next_lt(Bin, Pos) of
        nomatch -> Marks;
        At -> choices_at(Bin, At, Ctx, Picked, Marks)
    end.

choices_at(Bin, At, {Content, _Stack} = Ctx, Picked, Marks) ->
    case markup_at(Bin, At, Content) of
        {Skip, Next} when Skip =:= skip; Skip =:= skip_text ->
            choices(Bin, Next, Ctx, Picked, Marks);
        {start, Name, Attrs, Close} ->
            choices_start(Bin, Name, Attrs, Close, Ctx, Picked, Marks);
        {'end', Name, Close} ->
            choices_end(Bin, Name, Close, Ctx, Picked, Marks)
    end.

choices_start(_Bin, _Name, _Attrs, eof, _Ctx, _Picked, Marks) ->
    Marks;
choices_start(Bin, Name, Attrs, Close, {undefined, _} = Ctx, Picked, Marks) ->
    case Name of
        ~"option" ->
            choices_after(
                Bin, Name, Close, Ctx, Picked, mark_option(Bin, Attrs, Close, Picked, Marks)
            );
        _Other ->
            case closes_select(Name) of
                true -> Marks;
                false -> choices_after(Bin, Name, Close, Ctx, Picked, Marks)
            end
    end;
choices_start(Bin, Name, _Attrs, Close, Ctx, Picked, Marks) ->
    choices_after(Bin, Name, Close, Ctx, Picked, Marks).

%% Past a start tag: over the content of an element that is not parsed as markup, and
%% into the content context of one that changes it.
choices_after(Bin, Name, Close, {Content, Stack} = Ctx, Picked, Marks) ->
    case skip_content(Bin, Name, Close + 1, Content) of
        {skipped, Next} ->
            choices(Bin, Next, Ctx, Picked, Marks);
        parsed ->
            Inner = content_context_name(Name, Content),
            Self = binary:at(Bin, Close - 1) =:= $/ andalso Content =/= undefined,
            case Inner of
                Content ->
                    choices(Bin, Close + 1, Ctx, Picked, Marks);
                _Switched when Self -> choices(Bin, Close + 1, Ctx, Picked, Marks);
                _Switched ->
                    choices(Bin, Close + 1, {Inner, [{Name, Content} | Stack]}, Picked, Marks)
            end
    end.

choices_end(_Bin, _Name, eof, _Ctx, _Picked, Marks) ->
    Marks;
choices_end(_Bin, ~"select", _Close, {undefined, _}, _Picked, Marks) ->
    Marks;
choices_end(Bin, Name, Close, {_Content, [{Name, Outer} | Stack]}, Picked, Marks) ->
    choices(Bin, Close + 1, {Outer, Stack}, Picked, Marks);
choices_end(Bin, _Name, Close, Ctx, Picked, Marks) ->
    choices(Bin, Close + 1, Ctx, Picked, Marks).

%% The start tags that end a select's content, as the parser treats them there.
closes_select(~"select") -> true;
closes_select(~"input") -> true;
closes_select(~"keygen") -> true;
closes_select(~"textarea") -> true;
closes_select(_Name) -> false.

%% Content the parser does not read as markup: raw text runs to the element's end tag,
%% and a `<template>`'s content is inert, its own nested templates included.
skip_content(Bin, ~"template", From, undefined) ->
    {skipped, past_template(Bin, From, 1)};
skip_content(Bin, Name, From, Content) ->
    case raw_text_kind_name(Name, Content) of
        none -> parsed;
        _Raw -> {skipped, past_end_tag(Bin, Name, From)}
    end.

past_template(Bin, Pos, Depth) ->
    case next_lt(Bin, Pos) of
        nomatch ->
            byte_size(Bin);
        At ->
            case markup_at(Bin, At, undefined) of
                {start, ~"template", _Attrs, Close} when is_integer(Close) ->
                    past_template(Bin, Close + 1, Depth + 1);
                {'end', ~"template", Close} when is_integer(Close), Depth =:= 1 ->
                    Close + 1;
                {'end', ~"template", Close} when is_integer(Close) ->
                    past_template(Bin, Close + 1, Depth - 1);
                {Skip, Next} when Skip =:= skip; Skip =:= skip_text ->
                    past_template(Bin, Next, Depth);
                {_Tag, _Name, _Attrs, Close} when is_integer(Close) ->
                    past_template(Bin, Close + 1, Depth);
                {_Tag, _Name, Close} when is_integer(Close) ->
                    past_template(Bin, Close + 1, Depth);
                _Eof ->
                    byte_size(Bin)
            end
    end.

%% The end of a raw-text element's content: the first `</name` followed by what ends
%% a tag name, then that end tag.
past_end_tag(Bin, Name, From) ->
    case raw_text_end(Bin, Name, From) of
        nomatch ->
            byte_size(Bin);
        At ->
            case markup_at(Bin, At, undefined) of
                {'end', _Name, Close} when is_integer(Close) -> Close + 1;
                _Eof -> byte_size(Bin)
            end
    end.

raw_text_end(Bin, Name, From) ->
    case match_from(Bin, ~"</", From) of
        nomatch ->
            nomatch;
        {At, 2} ->
            Len = byte_size(Name),
            case Bin of
                <<_:(At + 2)/binary, Candidate:Len/binary, Rest/binary>> ->
                    case fold_name(Candidate) =:= Name andalso ends_name(Rest) of
                        true -> At;
                        false -> raw_text_end(Bin, Name, At + 2)
                    end;
                _Short ->
                    nomatch
            end
    end.

ends_name(<<C, _/binary>>) when C =/= $>, C =/= $/, not ?IS_SPACE(C) -> false;
ends_name(_End) -> true.

%% Mark the option whose start tag closes at `Close` when `Picked` accepts its value.
mark_option(Bin, Attrs, Close, Picked, Marks) ->
    Value =
        case lists:keyfind(~"value", 1, Attrs) of
            {_, V} ->
                V;
            %% The tree builder drops a NUL in text; collapsing folds CR away.
            false ->
                collapse_ws(binary:replace(option_text(Bin, Close + 1, []), <<0>>, <<>>, [global]))
        end,
    case {lists:keymember(~"selected", 1, Attrs), Picked(Value)} of
        {false, true} -> [{Close, insert} | Marks];
        {false, false} -> Marks;
        {true, true} -> Marks;
        %% A `selected` the value does not pick goes, every copy (the parser keeps the
        %% first, so a later one would take its place): the client clears it too.
        {true, false} -> [{Start, {cut, End}} || {selected_span, Start, End} <- Attrs] ++ Marks
    end.

%% An option's text, as its `text` property reads it: the text it holds up to what
%% ends it, character references decoded, tags and comments left out, and a script's
%% content too.
option_text(Bin, Pos, Acc) ->
    case match_from(Bin, [~"<", ~"&"], Pos) of
        nomatch ->
            iolist_to_binary(lists:reverse(Acc, [rest_from(Bin, Pos)]));
        {At, 1} ->
            Text = binary_part(Bin, Pos, At - Pos),
            case binary:at(Bin, At) of
                $& ->
                    {Char, Next} = char_ref(Bin, At, text),
                    option_text(Bin, Next, [Char, Text | Acc]);
                $< ->
                    option_markup(Bin, At, [Text | Acc])
            end
    end.

option_markup(Bin, At, Acc) ->
    case markup_at(Bin, At, undefined) of
        {skip, Next} ->
            option_text(Bin, Next, Acc);
        {skip_text, Next} ->
            option_text(Bin, Next, [~"<" | Acc]);
        {start, Name, _Attrs, Close} when is_integer(Close) ->
            case ends_option(start, Name) of
                true -> iolist_to_binary(lists:reverse(Acc));
                false -> option_child(Bin, Name, Close + 1, Acc)
            end;
        {'end', Name, Close} when is_integer(Close) ->
            case ends_option('end', Name) of
                true -> iolist_to_binary(lists:reverse(Acc));
                false -> option_text(Bin, Close + 1, Acc)
            end;
        _Eof ->
            iolist_to_binary(lists:reverse(Acc))
    end.

ends_option(start, Name) ->
    Name =:= ~"option" orelse Name =:= ~"optgroup" orelse
        Name =:= ~"hr" orelse closes_select(Name);
ends_option('end', Name) ->
    Name =:= ~"option" orelse Name =:= ~"optgroup" orelse
        Name =:= ~"select".

%% Inside an option: a script's content is not its text, a raw-text element's is (as
%% written, or decoded for escapable raw text), and any other tag adds nothing.
option_child(Bin, ~"script", From, Acc) ->
    option_text(Bin, past_end_tag(Bin, ~"script", From), Acc);
option_child(Bin, ~"template", From, Acc) ->
    %% Inert content, no text node of the option.
    option_text(Bin, past_template(Bin, From, 1), Acc);
option_child(Bin, Name, From, Acc) ->
    case raw_text_kind_name(Name, undefined) of
        none ->
            option_text(Bin, From, Acc);
        Kind ->
            End = raw_text_close(Bin, Name, From),
            Raw = binary_part(Bin, From, End - From),
            option_text(Bin, past_end_tag(Bin, Name, From), [raw_text_value(Kind, Raw) | Acc])
    end.

raw_text_close(Bin, Name, From) ->
    case raw_text_end(Bin, Name, From) of
        nomatch -> byte_size(Bin);
        At -> At
    end.

raw_text_value(escapable, Raw) -> decode_refs(Raw, text);
raw_text_value(raw, Raw) -> Raw.

%% What the markup at `At` (a `<`) is. A start tag comes with its attributes and the
%% position of its `>` (`eof` when the fragment ends inside it), an end tag with that
%% position; comments, bogus comments and, in foreign content, CDATA sections are
%% skipped; a `<` that opens nothing is text.
markup_at(Bin, At, Content) ->
    case Bin of
        <<_:At/binary, "<!--", _/binary>> ->
            {skip, comment_end(Bin, At + 4)};
        <<_:At/binary, "<![CDATA[", _/binary>> when Content =/= undefined ->
            {skip, past(Bin, At + 9, ~"]]>")};
        <<_:At/binary, "<!", _/binary>> ->
            {skip, past(Bin, At + 2, ~">")};
        <<_:At/binary, "<?", _/binary>> ->
            {skip, past(Bin, At + 2, ~">")};
        <<_:At/binary, "</", C, _/binary>> when ?IS_ALPHA(C) ->
            {Name, NameEnd} = tag_name_at(Bin, At + 2),
            {_Attrs, Close} = tag_attrs(Bin, NameEnd, []),
            {'end', Name, Close};
        <<_:At/binary, "</>", _/binary>> ->
            {skip, At + 3};
        <<_:At/binary, "</", _/binary>> ->
            {skip, past(Bin, At + 2, ~">")};
        <<_:At/binary, "<", C, _/binary>> when ?IS_ALPHA(C) ->
            {Name, NameEnd} = tag_name_at(Bin, At + 1),
            {Attrs, Close} = tag_attrs(Bin, NameEnd, []),
            {start, Name, Attrs, Close};
        <<_:At/binary, "<", _/binary>> ->
            {skip_text, At + 1}
    end.

%% A comment ends at `-->`, or `--!>`; `<!-->` and `<!--->` are empty ones.
comment_end(Bin, From) ->
    case Bin of
        <<_:From/binary, ">", _/binary>> ->
            From + 1;
        <<_:From/binary, "->", _/binary>> ->
            From + 2;
        _Body ->
            case match_from(Bin, [~"-->", ~"--!>"], From) of
                {At, Len} -> At + Len;
                nomatch -> byte_size(Bin)
            end
    end.

past(Bin, From, Pattern) ->
    case match_from(Bin, Pattern, From) of
        {At, Len} -> At + Len;
        nomatch -> byte_size(Bin)
    end.

tag_name_at(Bin, From) ->
    End = name_end(Bin, From),
    {fold_name(binary_part(Bin, From, End - From)), End}.

name_end(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, C, _/binary>> when C =/= $>, C =/= $/, not ?IS_SPACE(C) ->
            name_end(Bin, Pos + 1);
        _End ->
            Pos
    end.

%% A name as the parser compares it, ASCII case folded; most are lowercase already.
fold_name(Name) ->
    case has_upper(Name) of
        true -> ascii_lowercase(Name);
        false -> Name
    end.

has_upper(<<C, _/binary>>) when C >= $A, C =< $Z -> true;
has_upper(<<_, Rest/binary>>) -> has_upper(Rest);
has_upper(<<>>) -> false.

%% A tag's attributes, as the tokenizer reads them, and the position of the `>` that
%% closes the tag, or `eof`. Only `value` and `selected` matter here: those are kept,
%% the value decoded, the first of a repeated name winning as in the parser.
tag_attrs(Bin, Pos0, Acc) ->
    Pos = skip_attr_space(Bin, Pos0),
    case Bin of
        <<_:Pos/binary>> ->
            {Acc, eof};
        <<_:Pos/binary, ">", _/binary>> ->
            {Acc, Pos};
        _Attr ->
            %% A `=` may start a name (a parse error the parser recovers from).
            NameEnd = attr_name_end(Bin, Pos + 1),
            Name = binary_part(Bin, Pos, NameEnd - Pos),
            AfterName = skip_space(Bin, NameEnd),
            case Bin of
                <<_:AfterName/binary, "=", _/binary>> ->
                    {Value, After} = attr_value(Bin, skip_space(Bin, AfterName + 1)),
                    tag_attrs(Bin, After, add_attr(Name, Value, {Pos, After}, Acc));
                _NoValue ->
                    tag_attrs(Bin, AfterName, add_attr(Name, <<>>, {Pos, NameEnd}, Acc))
            end
    end.

%% Every `selected` also leaves its byte span, for mark_option/5 to cut.
add_attr(Name, Value, Span, Acc) when byte_size(Name) =:= 5; byte_size(Name) =:= 8 ->
    case fold_name(Name) of
        ~"value" ->
            first_attr(~"value", Value, Acc);
        ~"selected" ->
            [
                {selected_span, element(1, Span), element(2, Span)}
                | first_attr(~"selected", Value, Acc)
            ];
        _Other ->
            Acc
    end;
add_attr(_Name, _Value, _Span, Acc) ->
    Acc.

first_attr(Name, Value, Acc) ->
    case lists:keymember(Name, 1, Acc) of
        true -> Acc;
        false -> [{Name, decode_refs(preprocess(Value), attribute)} | Acc]
    end.

%% What the parser does to an attribute value before it decodes it: newlines
%% normalized (CR LF and CR to LF), NUL replaced with U+FFFD.
preprocess(Text) ->
    case binary:match(Text, [~"\r", <<0>>]) of
        nomatch ->
            Text;
        _Some ->
            Lf = binary:replace(binary:replace(Text, ~"\r\n", ~"\n", [global]), ~"\r", ~"\n", [
                global
            ]),
            binary:replace(Lf, <<0>>, <<16#FFFD/utf8>>, [global])
    end.

attr_value(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, Quote, _/binary>> when Quote =:= $"; Quote =:= $' ->
            case match_from(Bin, <<Quote>>, Pos + 1) of
                {Close, 1} -> {binary_part(Bin, Pos + 1, Close - Pos - 1), Close + 1};
                nomatch -> {binary_part(Bin, Pos + 1, byte_size(Bin) - Pos - 1), byte_size(Bin)}
            end;
        _Unquoted ->
            End = unquoted_end(Bin, Pos),
            {binary_part(Bin, Pos, End - Pos), End}
    end.

unquoted_end(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, C, _/binary>> when C =/= $>, not ?IS_SPACE(C) -> unquoted_end(Bin, Pos + 1);
        _End -> Pos
    end.

attr_name_end(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, C, _/binary>> when C =/= $=, C =/= $>, C =/= $/, not ?IS_SPACE(C) ->
            attr_name_end(Bin, Pos + 1);
        _End ->
            Pos
    end.

skip_attr_space(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, C, _/binary>> when C =:= $/; ?IS_SPACE(C) -> skip_attr_space(Bin, Pos + 1);
        _Other -> Pos
    end.

skip_space(Bin, Pos) ->
    case Bin of
        <<_:Pos/binary, C, _/binary>> when ?IS_SPACE(C) -> skip_space(Bin, Pos + 1);
        _Other -> Pos
    end.

next_lt(Bin, Pos) ->
    case match_from(Bin, ~"<", Pos) of
        {At, 1} -> At;
        nomatch -> nomatch
    end.

match_from(Bin, _Pattern, Pos) when Pos >= byte_size(Bin) ->
    nomatch;
match_from(Bin, Pattern, Pos) ->
    binary:match(Bin, Pattern, [{scope, {Pos, byte_size(Bin) - Pos}}]).

collapse_ws(Text) ->
    iolist_to_binary(
        lists:join(~" ", binary:split(Text, [~" ", ~"\t", ~"\n", ~"\f", ~"\r"], [global, trim_all]))
    ).

%% Character references, decoded as the tokenizer decodes them in text or in an
%% attribute value: numeric ones (with the replacements the standard makes), and named
%% ones from the standard's whole table, a legacy name without its `;` included -- in
%% an attribute value, only when what follows it could not continue a name.
decode_refs(Bin, Mode) ->
    case binary:match(Bin, ~"&") of
        nomatch -> Bin;
        {At, 1} -> decode_refs(Bin, At, Mode, [binary_part(Bin, 0, At)])
    end.

decode_refs(Bin, At, Mode, Acc) ->
    {Char, Next} = char_ref(Bin, At, Mode),
    case match_from(Bin, ~"&", Next) of
        nomatch ->
            iolist_to_binary(lists:reverse(Acc, [Char, rest_from(Bin, Next)]));
        {Amp, 1} ->
            decode_refs(Bin, Amp, Mode, [binary_part(Bin, Next, Amp - Next), Char | Acc])
    end.

%% The reference at `At` (an `&`): the text it decodes to and where the input resumes.
%% Not a reference: the `&` itself.
char_ref(Bin, At, Mode) ->
    case Bin of
        <<_:At/binary, "&#", X, Rest/binary>> when X =:= $x; X =:= $X ->
            numeric_ref(Bin, hex_digits(Rest, <<>>), At, 3, 16);
        <<_:At/binary, "&#", Rest/binary>> ->
            numeric_ref(Bin, dec_digits(Rest, <<>>), At, 2, 10);
        <<_:At/binary, "&", Rest/binary>> ->
            named_ref(Rest, At + 1, Mode)
    end.

%% `&#` (or `&#x`) with no digit is no reference: the `&` is text.
numeric_ref(_Bin, <<>>, At, _Lead, _Base) ->
    {~"&", At + 1};
numeric_ref(Bin, Digits, At, Lead, Base) ->
    End = At + Lead + byte_size(Digits),
    Next =
        case Bin of
            <<_:End/binary, ";", _/binary>> -> End + 1;
            _NoSemicolon -> End
        end,
    {code_point_text(binary_to_integer(Digits, Base)), Next}.

hex_digits(<<C, Rest/binary>>, Acc) when
    (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F)
->
    hex_digits(Rest, <<Acc/binary, C>>);
hex_digits(_Rest, Acc) ->
    Acc.

dec_digits(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    dec_digits(Rest, <<Acc/binary, C>>);
dec_digits(_Rest, Acc) ->
    Acc.

%% What a numeric reference decodes to: U+FFFD for NUL, a surrogate or past Unicode,
%% the windows-1252 character for a C1 control the standard remaps.
code_point_text(0) ->
    <<16#FFFD/utf8>>;
code_point_text(N) when N > 16#10FFFF ->
    <<16#FFFD/utf8>>;
code_point_text(N) when N >= 16#D800, N =< 16#DFFF ->
    <<16#FFFD/utf8>>;
code_point_text(N) when N >= 16#80, N =< 16#9F ->
    <<(c1_replacement(N))/utf8>>;
code_point_text(N) ->
    <<N/utf8>>.

c1_replacement(16#80) -> 16#20AC;
c1_replacement(16#82) -> 16#201A;
c1_replacement(16#83) -> 16#0192;
c1_replacement(16#84) -> 16#201E;
c1_replacement(16#85) -> 16#2026;
c1_replacement(16#86) -> 16#2020;
c1_replacement(16#87) -> 16#2021;
c1_replacement(16#88) -> 16#02C6;
c1_replacement(16#89) -> 16#2030;
c1_replacement(16#8A) -> 16#0160;
c1_replacement(16#8B) -> 16#2039;
c1_replacement(16#8C) -> 16#0152;
c1_replacement(16#8E) -> 16#017D;
c1_replacement(16#91) -> 16#2018;
c1_replacement(16#92) -> 16#2019;
c1_replacement(16#93) -> 16#201C;
c1_replacement(16#94) -> 16#201D;
c1_replacement(16#95) -> 16#2022;
c1_replacement(16#96) -> 16#2013;
c1_replacement(16#97) -> 16#2014;
c1_replacement(16#98) -> 16#02DC;
c1_replacement(16#99) -> 16#2122;
c1_replacement(16#9A) -> 16#0161;
c1_replacement(16#9B) -> 16#203A;
c1_replacement(16#9C) -> 16#0153;
c1_replacement(16#9E) -> 16#017E;
c1_replacement(16#9F) -> 16#0178;
c1_replacement(N) -> N.

%% A named reference: the longest name the standard knows, with its `;`, else the
%% longest legacy name (one valid without it) the run of letters and digits starts with.
named_ref(Rest, From, Mode) ->
    Name = alnum_run(Rest, <<>>),
    Len = byte_size(Name),
    Full =
        case Rest of
            <<Name:Len/binary, ";", _/binary>> -> arizona_html_entities:named(<<Name/binary, ";">>);
            _NoSemicolon -> error
        end,
    case Full of
        {ok, Text} -> {Text, From + Len + 1};
        error -> legacy_ref(Name, min(Len, ?LEGACY_REF_MAX), Rest, From, Mode)
    end.

legacy_ref(_Name, Len, _Rest, From, _Mode) when Len < 2 ->
    {~"&", From};
legacy_ref(Name, Len, Rest, From, Mode) ->
    case arizona_html_entities:named(binary_part(Name, 0, Len)) of
        {ok, Text} ->
            case Mode =:= attribute andalso continues_name(Rest, Len) of
                true -> {~"&", From};
                false -> {Text, From + Len}
            end;
        error ->
            legacy_ref(Name, Len - 1, Rest, From, Mode)
    end.

%% In an attribute value a legacy name followed by `=` or a letter or digit is kept as
%% written: `?a=1&copy=2` is a query string, not a copyright sign.
continues_name(Rest, Len) ->
    case Rest of
        <<_:Len/binary, C, _/binary>> -> C =:= $= orelse ?IS_ALNUM(C);
        _End -> false
    end.

alnum_run(<<C, Rest/binary>>, Acc) when ?IS_ALNUM(C) ->
    alnum_run(Rest, <<Acc/binary, C>>);
alnum_run(_Rest, Acc) ->
    Acc.

splice_selected(Bin, [], From, Acc) ->
    lists:reverse(Acc, [rest_from(Bin, From)]);
splice_selected(Bin, [{Gt, insert} | Marks], From, Acc) ->
    splice_selected(Bin, Marks, Gt, [~" selected", binary_part(Bin, From, Gt - From) | Acc]);
splice_selected(Bin, [{Start, {cut, End}} | Marks], From, Acc) ->
    splice_selected(Bin, Marks, End, [binary_part(Bin, From, Start - From) | Acc]).

rest_from(Bin, From) ->
    binary_part(Bin, From, byte_size(Bin) - From).

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
%% stringify to digits and a sign, so neither can spell a sequence below; both
%% pass through untouched, as does a map (a nested template renders structurally
%% through the escaping path, not as bytes). An effect command is refused: it is
%% not text (`effect_in_content`).
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
        {effect, Cmd} -> arizona_template:effect_in_content(Cmd);
        value -> raw_text_chardata(Ctx, Value)
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
