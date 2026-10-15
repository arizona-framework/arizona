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

These encoders serve commands that reach a client as data -- an event attribute or
prop, a controller response -- never the effects a live handler returns, which the
socket frames itself. So a `navigate`/`patch` carrying a `flash` opt is refused here
with `{flash_outside_live_effect, navigate | patch}`: the socket carries that flash
in-process to the navigation that follows its effect, and nothing would carry it
from a client that received it as data -- the message would be lost silently, its
content shipped to the page.
""".

-include("arizona_effect.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([encode/1]).
-export([encode_json/1]).
-export([json_value/1]).
-export([format_error/2]).
-export([wrapped/1]).

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
-export_type([cmds/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal cmd() :: {?MODULE, list()}.
%% A command list, nested to any depth; `[]` is no command.
-type cmds() :: [cmd() | cmds()].

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Encodes a command (or list of commands) as an HTML-attribute-safe binary: JSON
with `&`, `"`, and `<` escaped to their HTML entity equivalents.
""".
-spec encode(Cmds) -> binary() when
    Cmds :: cmd() | cmds().
encode(Cmds) ->
    escape_attr(encode_json(Cmds)).

-doc """
Serializes a command (or list of commands) to raw JSON, without the HTML
attribute escaping `encode/1` applies. Used by non-HTML backends (native) that
embed the command as a JSON value.
""".
-spec encode_json(Cmds) -> binary() when
    Cmds :: cmd() | cmds().
encode_json(Cmds) ->
    %% A string argument that is not UTF-8 is named `non_utf8_render`, not left
    %% as the encoder's `unexpected_end` (arizona_utf8:refuse/3, as in
    %% arizona_native). The try is inline: this runs per command per render, and
    %% only a failure should pay for the rename.
    Json = json_value(Cmds),
    try
        iolist_to_binary(json:encode(Json))
    catch
        error:Reason:Stacktrace -> arizona_utf8:refuse(Json, Reason, Stacktrace)
    end.

-doc """
The command (or list of commands) as the JSON term it serializes to: `[OpCode |
Args]`, or a list of those. A wire that JSON-encodes its values itself (a diff
op, the native hydration payload) carries this, so the client receives the
command as an array rather than a string of one.

A list may nest, and `[]` stands for no command: the list is flattened in order
and empty items dropped, so a conditional item (`case ... of true ->
arizona_js:hide(Sel); false -> [] end`) needs no filtering at the call site. Any
other item fails with `function_clause`.
""".
-spec json_value(Cmds) -> list() when
    Cmds :: cmd() | cmds().
json_value({?MODULE, Cmd}) ->
    no_flash(Cmd);
json_value(Cmds) when is_list(Cmds) ->
    [no_flash(C) || C <- lists:reverse(flatten(Cmds, []))].

-doc """
Formats the errors this module raises with an `error_info` annotation. Picked up
by `erl_error:format_exception/3`.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({flash_outside_live_effect, Op}, _Stacktrace) ->
    #{general => flash_message(Op)}.

-doc """
The commands a bare command (`[OpCode | Args]`, as `json_value/1` yields) wraps --
a `transition`, an `on_key`, or a `fetch`'s `on_error` -- as a list, with when they
run (`now` when the command does, `deferred` later, if ever) and a fun that
rebuilds the wrapper around a replacement list. `none` for any other command.
The one list of wrapper shapes, shared by every walk that must see inside them
(`arizona_socket`'s flash capture, this module's flash refusal).
""".
-spec wrapped(Cmd) -> {now | deferred, [list()], fun(([list()]) -> list())} | none when
    Cmd :: list().
wrapped([?EFFECT_TRANSITION, Opts, Cmds]) ->
    unwrap(now, Cmds, fun(New) -> [?EFFECT_TRANSITION, Opts, New] end);
wrapped([?EFFECT_ON_KEY, Key, Cmds]) ->
    unwrap(deferred, Cmds, fun(New) -> [?EFFECT_ON_KEY, Key, New] end);
wrapped([?EFFECT_FETCH, Url, #{on_error := Cmds} = Opts]) ->
    unwrap(deferred, Cmds, fun(New) -> [?EFFECT_FETCH, Url, Opts#{on_error => New}] end);
wrapped(_Cmd) ->
    none.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

flatten([], Acc) ->
    Acc;
flatten([{?MODULE, Cmd} | Rest], Acc) ->
    flatten(Rest, [Cmd | Acc]);
flatten([Nested | Rest], Acc) when is_list(Nested) ->
    flatten(Rest, flatten(Nested, Acc)).

%% The command as given, once refuse_flash/1 passed it -- see the moduledoc.
no_flash(Cmd) ->
    ok = refuse_flash(Cmd),
    Cmd.

%% Refuse a navigate/patch `flash` opt in a command or in a command it wraps
%% (see wrapped/1).
refuse_flash([Op, _Path, #{flash := _}]) when Op =:= ?EFFECT_NAVIGATE; Op =:= ?EFFECT_PATCH ->
    erlang:error({flash_outside_live_effect, op_name(Op)}, none, [
        {error_info, #{module => ?MODULE}}
    ]);
refuse_flash(Cmd) ->
    case wrapped(Cmd) of
        {_When, Inner, _Rewrap} -> lists:foreach(fun(C) -> ok = refuse_flash(C) end, Inner);
        none -> ok
    end.

%% A wrapper holds one command (its head is an op code) or a list: the walks get
%% a list either way, and the rebuild restores the shape it was given.
unwrap(When, [Op | _] = Cmd, Wrap) when is_integer(Op) ->
    {When, [Cmd], fun([New]) -> Wrap(New) end};
unwrap(When, Cmds, Wrap) ->
    {When, Cmds, Wrap}.

op_name(?EFFECT_NAVIGATE) -> navigate;
op_name(?EFFECT_PATCH) -> patch.

flash_message(Op) ->
    io_lib:format(
        "a ~s effect's `flash` opt only reaches its target when a live handler "
        "returns the effect: the socket carries the flash to the navigation that "
        "follows. In an event attribute or a controller response nothing carries "
        "it, so it would be lost and its content shipped to the page. From a "
        "template, push an event whose handler returns the ~s; from a controller, "
        "set the flash with arizona_req:put_flash/3 through the req/1 -> put_req/2 "
        "round trip and navigate with #{full => true}, so the browser's next GET "
        "carries the flash cookie",
        [Op, Op]
    ).

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
