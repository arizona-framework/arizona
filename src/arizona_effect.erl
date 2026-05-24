-module(arizona_effect).
-moduledoc """
Neutral wire format and encoders for client effect commands.

An effect is a `t:cmd/0` tuple `{arizona_effect, [OpCode | Args]}` that a client
runtime executes. The tuple shape and op codes (`include/arizona_effect.hrl`)
are the same on every client; only the *builders* are per platform
(`arizona_js` for the web, `arizona_android` for native, ...), so neither
platform owns the representation. This module is the shared plumbing: the tag,
the `t:cmd/0` type, and the two encoders.

`encode/1` serializes to JSON and HTML-escapes `&`, `"`, `<` for embedding in an
HTML attribute (the web path). `encode_json/1` serializes to raw JSON for clients
that embed the command as a JSON value (the native path).
""".

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/1]).
-export([encode_json/1]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-ifdef(TEST).
%% Inline EUnit tests intentionally repeat command tuples.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).
-endif.

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([cmd/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal cmd() :: {?MODULE, list()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Encodes a command (or list of commands) as an HTML-attribute-safe binary: JSON
with `&`, `"`, and `<` escaped to their HTML entity equivalents.
""".
-spec encode(Cmds) -> binary() when
    Cmds :: cmd() | [cmd()].
encode({?MODULE, Cmd}) ->
    escape_attr(iolist_to_binary(json:encode(Cmd)));
encode([{?MODULE, _} | _] = Cmds) ->
    escape_attr(iolist_to_binary(json:encode([C || {?MODULE, C} <:- Cmds]))).

-doc """
Serializes a command (or list of commands) to raw JSON, without the HTML
attribute escaping `encode/1` applies. Used by non-HTML backends (native) that
embed the command as a JSON value.
""".
-spec encode_json(Cmds) -> binary() when
    Cmds :: cmd() | [cmd()].
encode_json({?MODULE, Cmd}) ->
    iolist_to_binary(json:encode(Cmd));
encode_json([{?MODULE, _} | _] = Cmds) ->
    iolist_to_binary(json:encode([C || {?MODULE, C} <:- Cmds])).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% SWAR (SIMD Within A Register) byte scanner. Matches a 56-bit (7-byte)
%% word at a time and uses bitwise arithmetic to validate that none of
%% the 7 bytes is `&`, `"`, or `<` -- the only chars we need to escape.
%% On match, advances 7 bytes via a Skip/Len counter without copying
%% anything; the iolist Acc only grows when an escape is actually
%% emitted. For typical payloads (~30-50 bytes, mostly safe) this
%% beats the per-byte recursion by ~20%%. Inspired by erlang/otp#10938
%% which lands an equivalent SWAR scan in OTP 29's `json` module.
%%
%% 56 bits is the largest value that fits in a BEAM small (59-bit on
%% 64-bit), so all band/bxor/+/- guard ops compile to bare native
%% instructions without bignum fallbacks.
%%
%% The simplified Mycroft trick `((V - 0x01..) band 0x80..) =:= 0`
%% (no `bnot`) only works when bytes are < 0x80. The leading
%% `(W band 0x80..) =:= 0` check gates the rest via short-circuit
%% andalso, and XOR with constants < 0x80 preserves the high-bit-clear
%% invariant. False positives from borrow propagation are harmless --
%% we just fall through to the byte-by-byte path.
-define(SWAR_M01, 16#01010101010101).
-define(SWAR_M80, 16#80808080808080).
-define(SWAR_AMP7, 16#26262626262626).
-define(SWAR_QUOT7, 16#22222222222222).
-define(SWAR_LT7, 16#3C3C3C3C3C3C3C).

-define(swar_no_zero_byte(V),
    (((V) - ?SWAR_M01) band ?SWAR_M80) =:= 0
).

-define(swar_safe(W),
    ((W) band ?SWAR_M80) =:= 0 andalso
        ?swar_no_zero_byte((W) bxor ?SWAR_AMP7) andalso
        ?swar_no_zero_byte((W) bxor ?SWAR_QUOT7) andalso
        ?swar_no_zero_byte((W) bxor ?SWAR_LT7)
).

escape_attr(Bin) ->
    escape_attr(Bin, [], Bin, 0, 0).

escape_attr(Bin, Acc, Orig, Skip, Len) ->
    case Bin of
        <<W:56, Rest/binary>> when ?swar_safe(W) ->
            escape_attr(Rest, Acc, Orig, Skip, Len + 7);
        <<>> when Acc =:= [], Skip =:= 0 ->
            %% Common case: no escapes anywhere -- return original unchanged.
            Orig;
        <<>> ->
            iolist_to_binary([Acc, binary:part(Orig, Skip, Len)]);
        <<"&", Rest/binary>> ->
            Prefix = binary:part(Orig, Skip, Len),
            escape_attr(Rest, [Acc, Prefix, ~"&amp;"], Orig, Skip + Len + 1, 0);
        <<"\"", Rest/binary>> ->
            Prefix = binary:part(Orig, Skip, Len),
            escape_attr(Rest, [Acc, Prefix, ~"&quot;"], Orig, Skip + Len + 1, 0);
        <<"<", Rest/binary>> ->
            Prefix = binary:part(Orig, Skip, Len),
            escape_attr(Rest, [Acc, Prefix, ~"&lt;"], Orig, Skip + Len + 1, 0);
        <<_C, Rest/binary>> ->
            escape_attr(Rest, Acc, Orig, Skip, Len + 1)
    end.

-ifdef(TEST).

encode_single_test() ->
    ?assertEqual(~"[0,&quot;inc&quot;]", encode({?MODULE, [0, ~"inc"]})).

encode_multiple_test() ->
    ?assertEqual(
        ~"[[0,&quot;inc&quot;],[10,&quot;/p&quot;]]",
        encode([{?MODULE, [0, ~"inc"]}, {?MODULE, [10, ~"/p"]}])
    ).

encode_json_test() ->
    %% Raw JSON: no HTML entity escaping (a native client embeds it as a value).
    ?assertEqual(~"[0,\"inc\"]", encode_json({?MODULE, [0, ~"inc"]})).

encode_escapes_quotes_test() ->
    ?assertNotEqual(nomatch, binary:match(encode({?MODULE, [0, ~"a\"b"]}), ~"&quot;")).

encode_escapes_ampersand_test() ->
    ?assertNotEqual(nomatch, binary:match(encode({?MODULE, [0, ~"a&b"]}), ~"&amp;")).

encode_escapes_lt_test() ->
    Bin = encode({?MODULE, [0, ~"<script>"]}),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"&lt;")),
    ?assertEqual(nomatch, binary:match(Bin, ~"<script>")).

-endif.
