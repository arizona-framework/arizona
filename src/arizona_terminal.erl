-module(arizona_terminal).
-moduledoc """
Terminal (ANSI) render-target backend.

Emits the byte sequences that make up the `s` statics as plain text decorated
with ANSI escape codes, for views rendered into a terminal via `?terminal(...)`.
Built for a transport that **repaints the whole frame** each update (see
`arizona_live:render_current/1`) rather than applying diff ops, so unlike the
HTML and JSON backends it writes **no `az`/slot markers** into the output --
`az_attr/1`, `text_slot_open/1`, `text_slot_close/0` and `scope_static/3` are
no-ops. (`text_az/2` still hands out distinct slot ids so the snapshot's
`d`-list stays well-formed for the diff engine, whose ops the terminal
transport ignores.)

The frame is printed from the live process's diffed snapshot, not from a
fresh render of the template, so a repaint runs no child `mount/1` or
`handle_update/3`. A terminal therefore sees what a web client sees:
`az-nodiff` content renders once, a slot whose reads are not tracked freezes
after its first render, and after a hot reload edited template text appears
once a slot depending on it changes (the terminal does not repaint on reload).

## Vocabulary

- `col` / `row` -- transparent containers (a trailing style reset on close).
- `line` -- a block element: its content followed by a style reset and a
  newline. Vertical layout is a `col` of `line`s.
- `text` / `span` -- inline elements: content followed by a style reset.
- `br` -- a void element emitting a newline.

Styling is set with **bare-atom boolean attributes** mapped to `io_ansi`
escape codes (compile-time constants): `bold`, `dim`, `italic`, `underline`,
`inverse`, and the colours `black`/`red`/`green`/`yellow`/`blue`/`magenta`/
`cyan`/`white` (plus their `light_*` variants). For anything the named atoms
do not cover, use a valued attribute: `{sgr, Escape}` emits a raw escape
verbatim, `{fg, Index}` / `{bg, Index}` select 256-colour palette entries.
Styles do not inherit across container boundaries (every element resets on
close), so set the style on the `text`/`line` that needs it. An unrecognised
style atom, an unknown valued attribute, a dynamic attribute value, or an event
command attribute is **rejected at compile time** with a clear message rather
than silently dropped.

## Escaping

Dynamic content values are **control-char sanitized** at render (`escape/1`, the
`arizona_renderer` escape callback): C0 bytes (except `\\n`/`\\t`), DEL and the
C1 controls are stripped, and every byte outside well-formed UTF-8 becomes U+FFFD,
so an interpolated value cannot inject ANSI/OSC escape sequences into the terminal
(the terminal analog of XSS -- e.g. an OSC 52 clipboard write or a scrollback wipe
in a shared-channel app) and the output is always valid UTF-8. That guarantee
assumes a UTF-8 terminal (see `escape/1`). The `?raw`
opt-out bypasses this, so never wrap untrusted data in `?raw` in a `?terminal`
template. Static styling escapes (the `s` bytes above) are framework-emitted and
never sanitized.
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
-export([raw_text/4]).
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
-export([boolean_attrs/0]).
-export([attr_names/0]).
-export([escape/1]).
-export([render_attr/2]).
-export([wire_escape/1]).
-export([wire_attr/2]).

%% Full SGR reset (clears colour and every text style at once); io_ansi has no
%% whole-reset helper, only per-attribute `*_off` and `default_color`.
-define(RESET, ~"\e[0m").

%% A UTF-8 continuation byte, 0x80-0xBF.
-define(CONT(B), ((B) band 16#C0 =:= 16#80)).

-spec name(arizona_renderer:tag()) -> binary().
name(Bin) when is_binary(Bin) ->
    Bin;
name(Atom) ->
    atom_to_binary(Atom).

-spec element_open(binary()) -> binary().
element_open(TagName) ->
    %% No structural open in a terminal; styling comes from the attributes. This is
    %% still where the tag is checked, because it is the one callback every element
    %% passes through exactly once -- the same place attr/2 and attr_bool/2 police
    %% attribute names.
    %%
    %% The vocabulary is closed, so an unrecognized tag is a typo, not an extension
    %% point. Accepting it as a transparent container swallowed the author's intent
    %% silently: `{'BR', [], []}` emitted a style reset instead of a newline and
    %% `{'Line', ...}` dropped its newline, with nothing to notice at compile time --
    %% while an unknown *attribute* right beside it was rejected outright. Reject it
    %% the same way.
    case is_known_tag(TagName) of
        true ->
            <<>>;
        false ->
            reject([
                "unknown ?terminal element ",
                quote(TagName),
                " -- expected one of ",
                tag_names(),
                ". Tag names are case-sensitive here (the vocabulary is Arizona's own, "
                "not HTML's), so `line` is not `Line`"
            ])
    end.

%% The documented `?terminal` vocabulary: only `line` and `br` affect layout (see
%% element_close/1 and element_void_close/0); the rest are transparent containers
%% whose names are documentary. Kept as one list so the error message and the check
%% can never drift apart.
-spec tags() -> [binary()].
tags() ->
    [~"line", ~"col", ~"row", ~"text", ~"span", ~"br"].

is_known_tag(TagName) ->
    lists:member(TagName, tags()).

-spec tag_names() -> iodata().
tag_names() ->
    lists:join(~", ", tags()).

-spec az_attr(binary()) -> binary().
az_attr(_Az) ->
    %% No diff-target markers: the terminal transport repaints whole frames.
    <<>>.

-spec element_open_end() -> binary().
element_open_end() ->
    <<>>.

-spec element_void_close() -> binary().
element_void_close() ->
    %% The only void tag is `br`.
    ~"\n".

-spec element_close(binary()) -> binary().
element_close(~"line") ->
    <<?RESET/binary, "\n">>;
element_close(_TagName) ->
    ?RESET.

-spec attr(binary(), binary()) -> binary().
attr(~"sgr", Escape) ->
    %% Escape hatch: emit a raw ANSI/SGR escape sequence verbatim, so any styling
    %% the named atoms do not cover (24-bit colour, blink, ...) is still possible.
    Escape;
attr(~"fg", Index) ->
    %% 256-colour foreground by palette index, e.g. {fg, ~"208"}.
    <<"\e[38;5;", Index/binary, "m">>;
attr(~"bg", Index) ->
    %% 256-colour background by palette index, e.g. {bg, ~"22"}.
    <<"\e[48;5;", Index/binary, "m">>;
attr(Name, _Value) ->
    reject([
        "unknown ?terminal attribute ",
        quote(Name),
        " -- use a style atom (",
        style_names(),
        "), or {sgr, Escape} / {fg, Index} / {bg, Index} for a custom code"
    ]).

%% A `false` emits nothing, but its name is still checked, so `{bolld, false}` is the
%% same typo `{bolld, true}` is.
-spec attr_bool(binary(), boolean()) -> binary().
attr_bool(Name, Bool) ->
    case {find_style(Name), Bool} of
        {{ok, _Escape}, false} ->
            <<>>;
        {{ok, Escape}, true} ->
            iolist_to_binary(Escape);
        {error, _Bool} ->
            reject([
                "unknown ?terminal style ",
                quote(Name),
                " -- expected one of ",
                style_names(),
                ", or {sgr, Escape} / {fg, Index} / {bg, Index} for a custom code"
            ])
    end.

-spec attr_command(binary(), term()) -> no_return().
attr_command(Name, _Cmd) ->
    reject([
        "?terminal does not bind event command attribute ",
        quote(Name),
        " -- handle keypresses in the terminal driver, not the template"
    ]).

-spec attr_dyn_name(binary()) -> no_return().
attr_dyn_name(Name) ->
    reject([
        "?terminal does not support dynamic attribute ",
        quote(Name),
        " -- use a static style, {sgr, Escape}, or put dynamic content in children"
    ]).

-spec children_sep() -> binary().
children_sep() ->
    %% Newlines come from `line`/`br`, not from a between-children separator.
    <<>>.

-spec each_items([iodata()]) -> iodata().
each_items(Items) ->
    Items.

-spec text_child(binary()) -> binary().
text_child(Text) ->
    Text.

-spec text_az(binary(), non_neg_integer()) -> binary().
text_az(ElemAz, Slot) ->
    %% Distinct id per dynamic slot (markers are absent from the output, but the
    %% snapshot d-list still keys dynamics by az for the diff engine).
    <<ElemAz/binary, "t", (integer_to_binary(Slot))/binary>>.

-spec text_slot_open(binary()) -> binary().
text_slot_open(_Az) ->
    <<>>.

-spec text_slot_close() -> binary().
text_slot_close() ->
    <<>>.

-spec is_void(arizona_renderer:tag()) -> boolean().
is_void(br) -> true;
is_void(~"br") -> true;
is_void(_) -> false.

-spec raw_text_kind(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:raw_text_kind().
raw_text_kind(_Tag, _Context) ->
    %% Terminal output is plain styled text, not HTML -- no comment markers, so
    %% the raw-text corruption does not apply.
    none.

-spec content_context(arizona_renderer:tag(), arizona_renderer:content_context()) ->
    arizona_renderer:content_context().
content_context(_Tag, Parent) ->
    %% No foreign-content mode here -- the context never changes.
    Parent.

-spec scope_static(binary(), binary(), binary()) -> binary().
scope_static(_Fp, _Prefix, S0) ->
    %% No az references live in terminal statics, so there is nothing to scope.
    S0.

%% The terminal client does not implement `?OP_LIST_PATCH`; single-root list eachs
%% keep the wholesale re-render.
supports_list_patch() -> false.

target() -> terminal.

live_root() -> false.

content_type() -> ~"text/plain; charset=utf-8".

supports_local() -> false.

single_root_document() -> false.

supports_nodiff_keep() -> false.

keyed_item_ops() -> false.

-spec implied_attrs(arizona_renderer:tag(), [binary()]) -> [].
implied_attrs(_Tag, _Names) -> [].

boolean_attrs() -> presence.

%% Styles are SGR escapes emitted in order, so a repeated one (two `sgr` escapes) is
%% meaningful rather than a collision.
-spec attr_names() -> repeatable.
attr_names() -> repeatable.

-doc """
Sanitize a dynamic value's bytes for terminal output. Strips the C0 control bytes
`0x00-0x1F` (keeping `\\n` and `\\t`), `0x7F` (DEL) and the C1 controls, so an
interpolated value cannot inject ANSI/OSC/DCS escape sequences (e.g. `\\e]52;c;...`
clipboard writes, `\\e[3J` scrollback wipes) into other users' terminals. Dropping
`\\e` (0x1B) neutralizes the 7-bit escape forms; the C1 controls are their one-code
introducers (U+009B is CSI, U+009D is OSC), which a terminal honours both UTF-8
encoded (`C2 80`-`C2 9F`) and, to a lenient decoder, as a bare byte `0x80-0x9F`.

The output is always valid UTF-8: the UTF-8 encoded C1 controls are stripped, and
every byte outside a well-formed sequence (a bare `0x80-0x9F`, a stray lead byte,
Latin-1 text, an overlong form, a surrogate, anything past U+10FFFF) becomes
U+FFFD. A stray lead byte cannot then pair with what follows it (`C2` then `[`
would read as U+009B), and a unicode io device, which refuses invalid UTF-8, can
always write the frame. A byte in `0x80-0x9F` that belongs to a well-formed
sequence (the `81` of `ā`, the `82` of `€`) is part of a character and is kept.

This assumes a UTF-8 terminal. An 8-bit (ISO-8859-1) terminal reads those same
continuation bytes as C1 controls, and no sanitizer can protect it without
dropping UTF-8 text.
""".
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) ->
    %% Almost every rendered value carries no stripped byte at all, so locate
    %% the first one before building anything: with none, the input is returned
    %% as-is -- no accumulator, no copy. Only a value that really needs
    %% stripping allocates, and it copies the clean runs between controls as
    %% whole slices. The same fast path, and the same measured rationale, as
    %% arizona_html:escape/1.
    case first_control(Bin) of
        none ->
            Bin;
        RestSize ->
            strip(Bin, RestSize, <<>>)
    end.

%% Size of the remainder that starts at the first byte to strip or replace,
%% `none` when the value is already clean. Stripped: a C0 control other than
%% `\n`/`\t`, DEL 0x7F, and a UTF-8 encoded C1 control (`C2 80`-`C2 9F`).
%% Replaced: any byte that starts no well-formed sequence (the RFC 3629 byte table:
%% no overlong form, no surrogate, nothing past U+10FFFF). Each well-formed
%% sequence is stepped over whole, so a continuation byte of `ā`/`€` in 0x80-0x9F
%% is never mistaken for a control. Matched byte by byte rather than with
%% `/utf8`, which measured about twice as slow on CJK.
first_control(<<C, R/binary>>) when C >= 16#20 andalso C < 16#7F ->
    first_control(R);
first_control(<<$\n, R/binary>>) ->
    first_control(R);
first_control(<<$\t, R/binary>>) ->
    first_control(R);
first_control(<<L, C, R/binary>>) when
    L >= 16#C2 andalso L =< 16#DF andalso ?CONT(C) andalso (L =/= 16#C2 orelse C >= 16#A0)
->
    first_control(R);
first_control(<<L, C1, C2, R/binary>>) when
    L >= 16#E0 andalso
        L =< 16#EF andalso
        ?CONT(C1) andalso
        ?CONT(C2) andalso
        (L =/= 16#E0 orelse C1 >= 16#A0) andalso
        (L =/= 16#ED orelse C1 =< 16#9F)
->
    first_control(R);
first_control(<<L, C1, C2, C3, R/binary>>) when
    L >= 16#F0 andalso
        L =< 16#F4 andalso
        ?CONT(C1) andalso
        ?CONT(C2) andalso
        ?CONT(C3) andalso
        (L =/= 16#F0 orelse C1 >= 16#90) andalso
        (L =/= 16#F4 orelse C1 =< 16#8F)
->
    first_control(R);
first_control(<<>>) ->
    none;
first_control(Rest) ->
    byte_size(Rest).

%% Bin's last RestSize bytes start with a byte to strip or replace: keep the
%% clean run before it, swap that out, and scan on from there.
strip(Bin, RestSize, Acc) ->
    Pos = byte_size(Bin) - RestSize,
    <<Clean:Pos/binary, Rest/binary>> = Bin,
    {Swap, Tail} = swap_unsafe(Rest),
    case first_control(Tail) of
        none ->
            <<Acc/binary, Clean/binary, Swap/binary, Tail/binary>>;
        Next ->
            strip(Tail, Next, <<Acc/binary, Clean/binary, Swap/binary>>)
    end.

%% What first_control/1 stopped on: a UTF-8 encoded C1 control (two bytes) or an
%% ASCII control (one byte) is dropped; any other byte starts no well-formed
%% sequence and becomes U+FFFD, one per byte.
swap_unsafe(<<16#C2, C, R/binary>>) when C >= 16#80 andalso C =< 16#9F ->
    {<<>>, R};
swap_unsafe(<<C, R/binary>>) when C < 16#80 ->
    {<<>>, R};
swap_unsafe(<<_, R/binary>>) ->
    {~"\x{FFFD}", R}.

%% Terminal output has no raw-text elements (raw_text_kind/2 is always `none`), so
%% no dynamic is ever wrapped in this callback. Required by the behaviour; identity.
-spec raw_text(atom(), binary(), binary(), term()) -> term().
raw_text(_Tag, _Before, _After, Value) -> Value.

%% Unreachable: attr_dyn_name/1 rejects dynamic attributes at compile time, so a
%% ?terminal template never produces a dynamic-attr dynamic to render. Kept to
%% satisfy the arizona_renderer behaviour; delegates to the same rejection.
-spec render_attr(binary(), term()) -> no_return().
render_attr(Name, _Value) ->
    attr_dyn_name(Name).

%% A frame is repainted from the zipped document, so the wire carries what the
%% document does.
-spec wire_escape(binary()) -> binary().
wire_escape(Bin) ->
    escape(Bin).

-spec wire_attr(binary(), term()) -> no_return().
wire_attr(Name, Value) ->
    render_attr(Name, Value).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% The named boolean styles, each paired with its ANSI escape (a compile-time
%% constant from io_ansi). Single source of truth for both the lookup and the
%% "valid names" error message.
-spec styles() -> [{binary(), iodata()}].
styles() ->
    [
        {~"bold", io_ansi:bold()},
        {~"dim", io_ansi:dim()},
        {~"italic", io_ansi:italic()},
        {~"underline", io_ansi:underline()},
        {~"inverse", io_ansi:inverse()},
        {~"black", io_ansi:black()},
        {~"red", io_ansi:red()},
        {~"green", io_ansi:green()},
        {~"yellow", io_ansi:yellow()},
        {~"blue", io_ansi:blue()},
        {~"magenta", io_ansi:magenta()},
        {~"cyan", io_ansi:cyan()},
        {~"white", io_ansi:white()},
        {~"light_black", io_ansi:light_black()},
        {~"light_red", io_ansi:light_red()},
        {~"light_green", io_ansi:light_green()},
        {~"light_yellow", io_ansi:light_yellow()},
        {~"light_blue", io_ansi:light_blue()},
        {~"light_magenta", io_ansi:light_magenta()},
        {~"light_cyan", io_ansi:light_cyan()},
        {~"light_white", io_ansi:light_white()}
    ].

%% Looks up a named style's escape. Key first, returns `{ok, Escape} | error`.
-spec find_style(binary()) -> {ok, iodata()} | error.
find_style(Name) ->
    case lists:keyfind(Name, 1, styles()) of
        {Name, Escape} -> {ok, Escape};
        false -> error
    end.

-spec style_names() -> iodata().
style_names() ->
    lists:join(~", ", [Name || {Name, _Escape} <- styles()]).

-spec quote(binary()) -> iodata().
quote(Name) ->
    [$`, Name, $'].

%% Rejects an attribute the terminal backend cannot express. The parse transform
%% (`emit_attr/2`) catches this and re-raises it as a line-accurate compile error.
-spec reject(iodata()) -> no_return().
reject(Message) ->
    erlang:error({arizona_render_reject, unicode:characters_to_binary(Message)}).
