-module(arizona_terminal_io_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([printable_to_char_code/1]).
-export([named_control_bytes/1]).
-export([arrow_keys/1]).
-export([navigation_keys/1]).
-export([escape_and_alt/1]).
-export([ctrl_letters/1]).
-export([tokenises_multiple_keys/1]).
-export([drops_unknown_input/1]).
-export([driver_quits_on_ctrl_d/1]).
-export([driver_emits_key_events/1]).
-export([effect_builders/1]).
-export([default_driver_setup_teardown/1]).
-export([default_paint_repaints_in_place/1]).
-export([default_paint_quit_stops/1]).
-export([default_paint_title_bell_and_unknown/1]).
-export([default_paint_sanitizes_title/1]).
-export([terminal_escape_sanitizes/1]).

%% arizona_terminal_io:keys/1 decodes raw terminal reads into idiomatic keys;
%% arizona_terminal_default_driver turns those into events (quitting on Ctrl-D),
%% repaints frames in place, and interprets arizona_terminal_effect (quit/title/bell).

all() ->
    [
        printable_to_char_code,
        named_control_bytes,
        arrow_keys,
        navigation_keys,
        escape_and_alt,
        ctrl_letters,
        tokenises_multiple_keys,
        drops_unknown_input,
        driver_quits_on_ctrl_d,
        driver_emits_key_events,
        effect_builders,
        default_driver_setup_teardown,
        default_paint_repaints_in_place,
        default_paint_quit_stops,
        default_paint_title_bell_and_unknown,
        default_paint_sanitizes_title,
        terminal_escape_sanitizes
    ].

printable_to_char_code(Config) when is_list(Config) ->
    ?assertEqual([$a], arizona_terminal_io:keys(~"a")),
    ?assertEqual([$J], arizona_terminal_io:keys(~"J")),
    ?assertEqual([$+], arizona_terminal_io:keys(~"+")),
    ?assertEqual([$\s], arizona_terminal_io:keys(~" ")),
    ?assertEqual([$~], arizona_terminal_io:keys(~"~")).

named_control_bytes(Config) when is_list(Config) ->
    ?assertEqual([enter], arizona_terminal_io:keys(~"\r")),
    ?assertEqual([enter], arizona_terminal_io:keys(~"\n")),
    ?assertEqual([tab], arizona_terminal_io:keys(~"\t")),
    ?assertEqual([backspace], arizona_terminal_io:keys(<<127>>)),
    ?assertEqual([backspace], arizona_terminal_io:keys(<<8>>)).

arrow_keys(Config) when is_list(Config) ->
    ?assertEqual([up], arizona_terminal_io:keys(~"\e[A")),
    ?assertEqual([down], arizona_terminal_io:keys(~"\e[B")),
    ?assertEqual([right], arizona_terminal_io:keys(~"\e[C")),
    ?assertEqual([left], arizona_terminal_io:keys(~"\e[D")),
    %% SS3 (application cursor-key mode) variants.
    ?assertEqual([up], arizona_terminal_io:keys(~"\eOA")),
    ?assertEqual([left], arizona_terminal_io:keys(~"\eOD")).

navigation_keys(Config) when is_list(Config) ->
    ?assertEqual([home], arizona_terminal_io:keys(~"\e[H")),
    ?assertEqual(['end'], arizona_terminal_io:keys(~"\e[F")),
    ?assertEqual([home], arizona_terminal_io:keys(~"\e[1~")),
    ?assertEqual(['end'], arizona_terminal_io:keys(~"\e[4~")),
    ?assertEqual([page_up], arizona_terminal_io:keys(~"\e[5~")),
    ?assertEqual([page_down], arizona_terminal_io:keys(~"\e[6~")),
    ?assertEqual([insert], arizona_terminal_io:keys(~"\e[2~")),
    ?assertEqual([delete], arizona_terminal_io:keys(~"\e[3~")).

escape_and_alt(Config) when is_list(Config) ->
    ?assertEqual([escape], arizona_terminal_io:keys(<<27>>)),
    ?assertEqual([{alt, $a}], arizona_terminal_io:keys(~"\ea")),
    ?assertEqual([{alt, $X}], arizona_terminal_io:keys(~"\eX")).

ctrl_letters(Config) when is_list(Config) ->
    ?assertEqual([{ctrl, $a}], arizona_terminal_io:keys(<<1>>)),
    ?assertEqual([{ctrl, $c}], arizona_terminal_io:keys(<<3>>)),
    ?assertEqual([{ctrl, $z}], arizona_terminal_io:keys(<<26>>)).

tokenises_multiple_keys(Config) when is_list(Config) ->
    %% A read can carry several keypresses (fast typing / paste).
    ?assertEqual([$h, $i], arizona_terminal_io:keys(~"hi")),
    ?assertEqual([up, $j], arizona_terminal_io:keys(~"\e[Aj")),
    ?assertEqual([{ctrl, $c}, $a], arizona_terminal_io:keys(<<3, $a>>)).

drops_unknown_input(Config) when is_list(Config) ->
    %% An unsupported CSI sequence (e.g. shift-tab / a position report) is dropped.
    ?assertEqual([], arizona_terminal_io:keys(~"\e[Z")),
    ?assertEqual([], arizona_terminal_io:keys(~"\e[12;34R")),
    %% A lone non-ASCII byte (e.g. a UTF-8 continuation) is dropped.
    ?assertEqual([], arizona_terminal_io:keys(<<200>>)).

driver_quits_on_ctrl_d(Config) when is_list(Config) ->
    %% Ctrl-D (EOF) is the one always-available escape.
    ?assertEqual([stop], driver_keys(<<4>>)),
    %% Ctrl-C is a normal key event now -- the app decides what it means.
    ?assertEqual([{event, ~"key", #{key => {ctrl, $c}}}], driver_keys(<<3>>)),
    %% Other control keys are ordinary events too.
    ?assertEqual([{event, ~"key", #{key => {ctrl, $a}}}], driver_keys(<<1>>)).

driver_emits_key_events(Config) when is_list(Config) ->
    ?assertEqual([{event, ~"key", #{key => $j}}], driver_keys(~"j")),
    ?assertEqual([{event, ~"key", #{key => up}}], driver_keys(~"\e[A")),
    ?assertEqual([{event, ~"key", #{key => enter}}], driver_keys(~"\r")).

effect_builders(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [quit]}, arizona_terminal_effect:quit()),
    ?assertEqual(
        {arizona_effect, [set_title, ~"Title"]}, arizona_terminal_effect:set_title(~"Title")
    ),
    %% iodata title is normalized to a binary
    ?assertEqual(
        {arizona_effect, [set_title, ~"ab"]}, arizona_terminal_effect:set_title([~"a", ~"b"])
    ),
    ?assertEqual({arizona_effect, [bell]}, arizona_terminal_effect:bell()).

default_driver_setup_teardown(Config) when is_list(Config) ->
    ?assertEqual(#{}, arizona_terminal_default_driver:init(undefined)),
    %% setup hides the cursor and threads state; teardown restores it.
    ?assertEqual({io_ansi:cursor_hide(), #{}}, arizona_terminal_default_driver:setup(#{})),
    ?assertEqual(io_ansi:cursor_show(), arizona_terminal_default_driver:teardown(#{})).

default_paint_repaints_in_place(Config) when is_list(Config) ->
    {Out, Next} = paint_result(~"a\nb\n", []),
    ?assertEqual(continue, Next),
    %% home + per-line clear-to-EOL + clear-below, and NOT a full-screen \e[2J clear
    ?assert(contains(Out, ~"\e[H")),
    ?assert(contains(Out, ~"a\e[K\r\n")),
    ?assert(contains(Out, ~"\e[J")),
    ?assertNot(contains(Out, ~"\e[2J")).

default_paint_quit_stops(Config) when is_list(Config) ->
    {_Out, Next} = paint_result(~"frame\n", [arizona_terminal_effect:quit()]),
    ?assertEqual(stop, Next).

default_paint_title_bell_and_unknown(Config) when is_list(Config) ->
    %% The OSC terminator and the bell are a real BEL byte (0x07); Erlang has no
    %% `\a` escape (that would be the literal byte `a`).
    {TitleOut, _} = paint_result(~"x\n", [arizona_terminal_effect:set_title(~"Hi")]),
    ?assert(contains(TitleOut, <<"\e]0;Hi", 7>>)),
    {BellOut, _} = paint_result(~"x\n", [arizona_terminal_effect:bell()]),
    ?assert(contains(BellOut, <<7>>)),
    %% an unknown effect is ignored: no stop, no output beyond the plain repaint
    {PlainOut, _} = paint_result(~"x\n", []),
    {UnknownOut, UnknownNext} = paint_result(~"x\n", [{arizona_effect, [something_else]}]),
    ?assertEqual(continue, UnknownNext),
    ?assertEqual(PlainOut, UnknownOut).

%% A user-influenced title carrying ESC/BEL must not break out of the OSC string.
%% Those bytes are stripped, so the only OSC introducer left is the driver's own
%% `\e]0;` and the injected sequence is neutralized to inert text.
default_paint_sanitizes_title(Config) when is_list(Config) ->
    %% 'ev' ESC ]0;il BEL '!' -- an attempt to close the OSC early and inject.
    {Out, _} = paint_result(~"x\n", [
        arizona_terminal_effect:set_title(<<"ev\e]0;il", 7, "!">>)
    ]),
    %% Exactly one `\e]` -- the driver's introducer; the value's is stripped.
    %% (The frame repaint uses CSI `\e[`, never `\e]`.)
    ?assertEqual(1, length(binary:matches(Out, ~"\e]"))),
    %% The title text survives with its ESC/BEL removed, hence contiguous.
    ?assert(contains(Out, ~"ev]0;il!")).

%% arizona_terminal:escape/1 strips control bytes that could inject escape
%% sequences, keeps `\n`/`\t`, and preserves UTF-8 multi-byte sequences intact.
terminal_escape_sanitizes(Config) when is_list(Config) ->
    ?assertEqual(~"a]52;c;xb", arizona_terminal:escape(<<"a\e]52;c;x", 7, "b">>)),
    ?assertEqual(~"line1\nline2\tcol", arizona_terminal:escape(~"line1\nline2\tcol")),
    ?assertEqual(<<"café"/utf8>>, arizona_terminal:escape(<<"café"/utf8>>)),
    ?assertEqual(~"", arizona_terminal:escape(<<7, 27, 127>>)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

driver_keys(Bytes) ->
    {Commands, _State} = arizona_terminal_default_driver:keys(Bytes, #{}),
    Commands.

paint_result(Frame, Effects) ->
    {Out, Next, _State} = arizona_terminal_default_driver:paint(Frame, Effects, #{}),
    {iolist_to_binary(Out), Next}.

contains(Bin, Sub) ->
    binary:match(Bin, Sub) =/= nomatch.
