-module(arizona_terminal).
-moduledoc """
Terminal (ANSI) render-target backend.

Emits the byte sequences that make up the `s` statics as plain text decorated
with ANSI escape codes, for views rendered into a terminal via `?terminal(...)`.
Built for a transport that **repaints the whole frame** each update (see
`arizona_live:render_current/1`) rather than applying diff ops, so unlike the
HTML and native backends it writes **no `az`/slot markers** into the output --
`az_attr/1`, `text_slot_open/1`, `text_slot_close/0` and `scope_static/2` are
no-ops. (`text_az/2` still hands out distinct slot ids so the snapshot's
`d`-list stays well-formed for the diff engine, whose ops the terminal
transport ignores.)

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
`arizona_renderer` escape callback): C0 bytes (except `\\n`/`\\t`) and DEL are
stripped so an interpolated value cannot inject ANSI/OSC escape sequences into
the terminal (the terminal analog of XSS -- e.g. an OSC 52 clipboard write or a
scrollback wipe in a shared-channel app). UTF-8 is preserved. The `?raw`
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

%% Full SGR reset (clears colour and every text style at once); io_ansi has no
%% whole-reset helper, only per-attribute `*_off` and `default_color`.
-define(RESET, ~"\e[0m").

-spec name(atom()) -> binary().
name(Atom) ->
    atom_to_binary(Atom).

-spec element_open(binary()) -> binary().
element_open(_TagName) ->
    %% No structural open in a terminal; styling comes from the attributes.
    <<>>.

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

-spec attr_boolean(binary()) -> binary().
attr_boolean(Name) ->
    case find_style(Name) of
        {ok, Escape} ->
            iolist_to_binary(Escape);
        error ->
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

-spec is_void(atom()) -> boolean().
is_void(br) -> true;
is_void(_) -> false.

-spec raw_text_kind(atom()) -> none | raw | escapable.
raw_text_kind(_Tag) ->
    %% Terminal output is plain styled text, not HTML -- no comment markers, so
    %% the raw-text corruption does not apply.
    none.

-spec scope_static(binary(), binary()) -> binary().
scope_static(_Fp, S0) ->
    %% No az references live in terminal statics, so there is nothing to scope.
    S0.

%% The terminal client does not implement `?OP_LIST_PATCH`; single-root list eachs
%% keep the wholesale re-render.
supports_list_patch() -> false.

-doc """
Sanitize a dynamic value's bytes for terminal output. Strips the C0 control bytes
`0x00-0x1F` (keeping `\\n` and `\\t`) and `0x7F` (DEL), so an interpolated value
cannot inject ANSI/OSC/DCS escape sequences (e.g. `\\e]52;c;...` clipboard writes,
`\\e[3J` scrollback wipes) into other users' terminals. Bytes `>= 0x80` are kept
verbatim so UTF-8 multi-byte sequences are not corrupted; dropping `\\e` (0x1B)
already neutralizes the 7-bit C1 forms and every escape-sequence introducer.
""".
-spec escape(binary()) -> binary().
escape(Bin) when is_binary(Bin) ->
    escape(Bin, <<>>).

escape(<<>>, Acc) ->
    Acc;
escape(<<C, R/binary>>, Acc) when C >= 16#20, C =/= 16#7F ->
    escape(R, <<Acc/binary, C>>);
escape(<<$\n, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, $\n>>);
escape(<<$\t, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, $\t>>);
escape(<<_C, R/binary>>, Acc) ->
    %% Any other C0 control (incl. ESC 0x1B, BEL 0x07) or DEL 0x7F: drop it.
    escape(R, Acc).

%% Unreachable: attr_dyn_name/1 rejects dynamic attributes at compile time, so a
%% ?terminal template never produces a dynamic-attr dynamic to render. Kept to
%% satisfy the arizona_renderer behaviour; delegates to the same rejection.
-spec render_attr(binary(), term()) -> no_return().
render_attr(Name, _Value) ->
    attr_dyn_name(Name).

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
