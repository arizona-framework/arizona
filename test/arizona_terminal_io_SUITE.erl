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

%% arizona_terminal_io:keys/1 decodes raw terminal reads into idiomatic keys;
%% arizona_terminal_default_driver turns those into events, quitting on Ctrl-D (EOF).

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
        driver_emits_key_events
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

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

driver_keys(Bytes) ->
    {Commands, _State} = arizona_terminal_default_driver:keys(Bytes, #{}),
    Commands.
