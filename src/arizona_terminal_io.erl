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
-export([take_incomplete/1]).

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

-doc """
Split a read into its leading decodable bytes and a trailing **incomplete** escape
sequence. A read may end mid-sequence -- a transport delivers bytes in chunks (the
local TTY reads fixed-size chunks; a network transport fragments on TCP boundaries),
so an escape sequence like `\\e[A` (an arrow key) can be cut after `\\e[`. Decoded in
isolation that tail is lost and the next read's `A` mis-decodes into a spurious `$A`.

Returns `{Complete, Incomplete}`: `Complete` decodes cleanly via `keys/1`, and a
driver carries `Incomplete` in its state to prepend to the next read. The incomplete
tail is the trailing bytes of an unfinished CSI (`\\e[` + parameter/intermediate
bytes, awaiting a final byte), an unfinished SS3 introducer (`\\eO`), or a lone
trailing `\\e` (which could begin any of those). A trailing lone `\\e` therefore
holds until the next read -- the classic terminal ESC-vs-sequence ambiguity, resolved
here by buffering rather than a timer.
""".
-spec take_incomplete(binary()) -> {binary(), binary()}.
take_incomplete(Bytes) ->
    case last_esc_pos(Bytes) of
        none ->
            {Bytes, <<>>};
        Pos ->
            <<Complete:Pos/binary, Suffix/binary>> = Bytes,
            case incomplete_escape(Suffix) of
                true -> {Complete, Suffix};
                false -> {Bytes, <<>>}
            end
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

%% Byte offset of the last ESC (0x1B) in the read, or `none`. An incomplete escape
%% is always a trailing thing (bytes ran out mid-sequence), so any earlier ESC has
%% enough bytes after it to complete -- only the final ESC's suffix can be pending.
last_esc_pos(Bytes) ->
    case binary:matches(Bytes, <<$\e>>) of
        [] -> none;
        Matches -> element(1, lists:last(Matches))
    end.

%% Does a suffix starting at an ESC form an unfinished escape sequence? A lone
%% `\e`, an SS3 introducer `\eO`, or a CSI `\e[` whose bytes are all parameter/
%% intermediate (0x20-0x3F) with no final byte yet. Anything else starting with ESC
%% (Alt-<char>, a finished `\e[A`/`\eOA`) is decodable now.
incomplete_escape(<<$\e>>) -> true;
incomplete_escape(<<$\e, $O>>) -> true;
incomplete_escape(<<$\e, $[, Rest/binary>>) -> csi_unfinished(Rest);
incomplete_escape(_Suffix) -> false.

%% A CSI is unfinished while every byte so far is a parameter/intermediate byte
%% (0x20-0x3F) -- the final byte (0x40-0x7E) has not arrived.
csi_unfinished(<<>>) -> true;
csi_unfinished(<<Byte, Rest/binary>>) when Byte >= 16#20, Byte =< 16#3F -> csi_unfinished(Rest);
csi_unfinished(_Rest) -> false.
