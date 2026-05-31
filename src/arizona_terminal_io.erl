-module(arizona_terminal_io).
-moduledoc """
Low-level terminal byte helpers -- the arizona complement to OTP's `m:io_ansi`,
which only emits *output* sequences. This module starts on the *input* side and
has room to grow (CRLF, mouse, ...).

`keys/1` translates a raw terminal read into a list of idiomatic keys, so a driver
(see `m:arizona_terminal_driver`) can pattern-match on them rather than on raw
bytes:

- a printable character is its code -- `a` -> `$a`, `+` -> `$+`, space -> `$\\s`;
- named and escape keys are atoms -- `up`, `down`, `left`, `right`, `enter`, `tab`,
  `backspace`, `escape`, `home`, `'end'`, `page_up`, `page_down`, `insert`,
  `delete`;
- modified keys are tuples -- `{ctrl, $c}` (Ctrl-C), `{alt, $a}` (Alt-a).

A read can carry several keypresses (fast typing or a paste), so it returns a list.

Covers ASCII + the keys above. Not yet handled (skipped or split per byte): UTF-8
multibyte input and the function-key set (`f1`..`f12`); unrecognised escape
sequences are dropped.
""".

-export([keys/1]).

-export_type([key/0]).

-nominal key() :: char() | atom() | {ctrl, char()} | {alt, char()}.

-doc "Decode a raw terminal read into idiomatic keys.".
-spec keys(binary()) -> [key()].
keys(<<>>) ->
    [];
keys(Bytes) ->
    case decode(Bytes) of
        {skip, Rest} -> keys(Rest);
        {Key, Rest} -> [Key | keys(Rest)]
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Known CSI sequences (cursor / navigation / edit keys).
decode(<<"\e[A", Rest/binary>>) -> {up, Rest};
decode(<<"\e[B", Rest/binary>>) -> {down, Rest};
decode(<<"\e[C", Rest/binary>>) -> {right, Rest};
decode(<<"\e[D", Rest/binary>>) -> {left, Rest};
decode(<<"\e[H", Rest/binary>>) -> {home, Rest};
decode(<<"\e[F", Rest/binary>>) -> {'end', Rest};
decode(<<"\e[1~", Rest/binary>>) -> {home, Rest};
decode(<<"\e[7~", Rest/binary>>) -> {home, Rest};
decode(<<"\e[4~", Rest/binary>>) -> {'end', Rest};
decode(<<"\e[8~", Rest/binary>>) -> {'end', Rest};
decode(<<"\e[5~", Rest/binary>>) -> {page_up, Rest};
decode(<<"\e[6~", Rest/binary>>) -> {page_down, Rest};
decode(<<"\e[2~", Rest/binary>>) -> {insert, Rest};
decode(<<"\e[3~", Rest/binary>>) -> {delete, Rest};
%% Any other CSI sequence (e.g. an unsupported function key): consume and drop it.
decode(<<"\e[", Rest/binary>>) -> {skip, skip_csi(Rest)};
%% SS3 arrows (application cursor-key mode).
decode(<<"\eOA", Rest/binary>>) -> {up, Rest};
decode(<<"\eOB", Rest/binary>>) -> {down, Rest};
decode(<<"\eOC", Rest/binary>>) -> {right, Rest};
decode(<<"\eOD", Rest/binary>>) -> {left, Rest};
%% Any other SS3 sequence (e.g. F1-F4): consume the final byte and drop it.
decode(<<"\eO", _Final, Rest/binary>>) -> {skip, Rest};
%% Alt-<printable>: ESC followed by a printable character.
decode(<<$\e, Char, Rest/binary>>) when Char >= 32, Char < 127 -> {{alt, Char}, Rest};
%% Bare ESC.
decode(<<$\e, Rest/binary>>) -> {escape, Rest};
%% Named control bytes.
decode(<<$\r, Rest/binary>>) -> {enter, Rest};
decode(<<$\n, Rest/binary>>) -> {enter, Rest};
decode(<<$\t, Rest/binary>>) -> {tab, Rest};
decode(<<127, Rest/binary>>) -> {backspace, Rest};
decode(<<8, Rest/binary>>) -> {backspace, Rest};
%% Ctrl-<letter>: bytes 1-26 (minus the named ones handled above).
decode(<<Byte, Rest/binary>>) when Byte >= 1, Byte =< 26 -> {{ctrl, Byte + $a - 1}, Rest};
%% Printable ASCII.
decode(<<Char, Rest/binary>>) when Char >= 32, Char < 127 -> {Char, Rest};
%% Anything else (e.g. a UTF-8 continuation byte): drop one byte.
decode(<<_Byte, Rest/binary>>) -> {skip, Rest}.

%% Consume a CSI sequence's parameter/intermediate bytes up to and including its
%% final byte (0x40-0x7E), returning what's left.
skip_csi(<<Byte, Rest/binary>>) when Byte >= 16#40, Byte =< 16#7E -> Rest;
skip_csi(<<_Byte, Rest/binary>>) -> skip_csi(Rest);
skip_csi(<<>>) -> <<>>.
