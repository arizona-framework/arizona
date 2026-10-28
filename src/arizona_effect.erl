-module(arizona_effect).
-moduledoc """
Neutral wire format and encoders for client effect commands.

An effect is a `t:cmd/0` tuple `{arizona_effect, [OpCode | Args]}` that a client
runtime executes. The tuple shape and op codes (`include/arizona_effect.hrl`)
are the same on every client; only the *builders* are per platform
(`arizona_js` for the web, `arizona_android` for native, ...), so neither
platform owns the representation. This module is the shared plumbing: the tag,
the `t:cmd/0` type, the two encoders, and the builders of the commands the socket
protocol itself defines -- `push_event`, `navigate` and `patch` -- which the server
also builds (a redirect, a degraded navigation) and every platform's builders share.
The platform modules re-export them under their own names, with what their client
does with each option.

`encode/1` serializes to JSON and HTML-escapes `&`, `"`, `<` for embedding in an
HTML attribute (the web path). `encode_json/1` serializes to raw JSON for clients
that embed the command as a JSON value (the `?json` path).

These encoders serve commands that reach a client as data -- an event attribute or
prop, a controller response -- never the effects a live handler returns, which the
socket frames itself. So a `navigate`/`patch` carrying a `flash` opt is refused here
with `{flash_outside_live_effect, navigate | patch}`: the socket carries that flash
in-process to the navigation that follows its effect, and nothing would carry it
from a client that received it as data -- the message would be lost silently, its
content shipped to the page.
""".
-behaviour(arizona_command_builder).

-include("arizona_effect.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([target/0]).
-export([push_event/1]).
-export([push_event/2]).
-export([navigate/1]).
-export([navigate/2]).
-export([patch/1]).
-export([patch/2]).
-export([encode/1]).
-export([encode_json/1]).
-export([json_value/1]).
-export([to_wire/1]).
-export([nest/2]).
-export([mapfold_nested/3]).
-export([format_error/2]).

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
-export_type([nested/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal cmd() :: {?MODULE, list()}.
%% A command list, nested to any depth; `[]` is no command.
-type cmds() :: [cmd() | cmds()].
%% Commands held inside another command's arguments (see nest/2): one bare command
%% or a list of them, and when they run.
-nominal nested() :: {?MODULE, now | deferred, list()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Pushes the named event to the view the command runs against, which handles it in
`handle_event/3`. A client may collect a payload of its own (the web client reads
the triggering element's fields).
""".
-spec push_event(Event) -> cmd() when
    Event :: binary().
push_event(Event) -> {?MODULE, [?EFFECT_PUSH_EVENT, Event]}.

-doc "Pushes the named event with an explicit payload.".
-spec push_event(Event, Payload) -> cmd() when
    Event :: binary(),
    Payload :: map().
push_event(Event, Payload) -> {?MODULE, [?EFFECT_PUSH_EVENT, Event, Payload]}.

-doc "Navigates to the route `Path`, replacing the live view.".
-spec navigate(Path) -> cmd() when
    Path :: binary().
navigate(Path) -> {?MODULE, [?EFFECT_NAVIGATE, Path]}.

-doc """
Navigates to the route `Path` with options. The protocol defines:

- `flash` -- a flash map for the view navigated to, carried in-process by the
  socket and stripped from the client effect; only on an effect a live handler
  returns (see the moduledoc).
- `replace` -- the navigation replaces the current history entry.
- `full` -- a full page load instead of a live navigation.

A client without history or page loads ignores the last two.
""".
-spec navigate(Path, Opts) -> cmd() when
    Path :: binary(),
    Opts :: map().
navigate(Path, Opts) -> {?MODULE, [?EFFECT_NAVIGATE, Path, Opts]}.

-doc "Navigates in place to the route `Path`, keeping the live view (`handle_update/3`).".
-spec patch(Path) -> cmd() when
    Path :: binary().
patch(Path) -> {?MODULE, [?EFFECT_PATCH, Path]}.

-doc "Navigates in place to `Path` with the options of `navigate/2`.".
-spec patch(Path, Opts) -> cmd() when
    Path :: binary(),
    Opts :: map().
patch(Path, Opts) -> {?MODULE, [?EFFECT_PATCH, Path, Opts]}.

-doc """
Encodes a command (or list of commands) as an HTML-attribute-safe binary: JSON
with `&`, `"`, and `<` escaped to their HTML entity equivalents.
""".
-spec encode(Cmds) -> binary() when
    Cmds :: cmd() | cmds().
encode(Cmds) ->
    %% The escaping happens per string, inside the encode, rather than over the
    %% finished JSON: a string is the only place `&`, `"` or `<` can occur, and a
    %% string's own delimiters are its `"`s. Escaping the whole document instead
    %% met a `"` in nearly every word -- command JSON is mostly delimiters -- and
    %% sliced and flattened it a second time; per string, a clean one scans fast and
    %% splices. Same bytes as escaping `encode_json/1`'s output, which a test holds.
    Json = json_value(Cmds),
    try
        iolist_to_binary(json:encode(Json, fun attr_encode/2))
    catch
        error:Reason:Stacktrace -> arizona_utf8:refuse(Json, Reason, Stacktrace)
    end.

-doc """
Serializes a command (or list of commands) to raw JSON, without the HTML
attribute escaping `encode/1` applies. Used by non-HTML backends (JSON) that
embed the command as a JSON value.
""".
-spec encode_json(Cmds) -> binary() when
    Cmds :: cmd() | cmds().
encode_json(Cmds) ->
    %% A string argument that is not UTF-8 is named `non_utf8_render`, not left
    %% as the encoder's `unexpected_end` (arizona_utf8:refuse/3, as in
    %% arizona_json). The try is inline: this runs per command per render, and
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
op, the JSON hydration payload) carries this, so the client receives the
command as an array rather than a string of one.

A list may nest, and `[]` stands for no command: the list is flattened in order
and empty items dropped, so a conditional item (`case ... of true ->
arizona_js:hide(Sel); false -> [] end`) needs no filtering at the call site. Any
other item fails with `function_clause`.
""".
-spec json_value(Cmds) -> list() when
    Cmds :: cmd() | cmds().
json_value({?MODULE, _} = Cmd) ->
    to_wire(no_flash(bare(Cmd)));
json_value(Cmds) when is_list(Cmds) ->
    [to_wire(no_flash(C)) || C <- bare(Cmds)].

-doc """
A bare command (`[OpCode | Args]`) as its client receives it: every `t:nested/0`
in its arguments -- at any depth of lists and map values, and inside the commands
it holds -- lowered to the command or list it holds. `json_value/1` applies it;
a wire that encodes a live handler's effects itself (the socket) applies it last,
after walking them with `mapfold_nested/3`.
""".
-spec to_wire(Cmd) -> list() when
    Cmd :: list().
to_wire([Op | Args]) ->
    [Op | [wire_arg(A) || A <- Args]].

-doc """
Commands held inside another command, to put where its client expects them: a
builder that wraps commands (`arizona_js:transition/2`, `on_key/2`, a `fetch`'s
`on_error`) nests them with this. `When` is when they run: `now`, with the command
holding them, or `deferred`, later if ever. Encoding lowers it to the bare command
or list (`to_wire/1`), so a client sees the same array it always did, and every
walk that must see inside a wrapper finds it by this shape (`mapfold_nested/3`)
instead of knowing which commands wrap others.

`Cmds` is flattened like any command list (nested lists spliced, `[]` dropped);
a single command stays one command rather than a list of one.
""".
-spec nest(When, Cmds) -> nested() when
    When :: now | deferred,
    Cmds :: cmd() | cmds().
nest(When, Cmds) when When =:= now; When =:= deferred ->
    {?MODULE, When, bare(Cmds)}.

-doc """
Folds `Fun` over the command lists nested in a bare command (`[OpCode | Args]`) --
in any argument, list or map value -- and rebuilds the command around what it
returns. `Fun(When, Cmds, Acc)` gets each `t:nested/0`'s commands as a list (one
command nested alone too) and returns `{Cmds1, Acc1}`, `Cmds1` the same length
when it nested one. A command nesting nothing comes back as it is. The one walk
shared by everything that must see inside a wrapper (`arizona_socket`'s flash
capture, this module's flash refusal).
""".
-spec mapfold_nested(Fun, Acc, Cmd) -> {list(), Acc} when
    Fun :: fun((now | deferred, [list()], Acc) -> {[list()], Acc}),
    Acc :: term(),
    Cmd :: list().
mapfold_nested(Fun, Acc0, [Op | Args0]) ->
    {Args, Acc} = lists:mapfoldl(fun(A, Acc1) -> nested_arg(Fun, A, Acc1) end, Acc0, Args0),
    {[Op | Args], Acc}.

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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% A command, or a command list flattened in order with `[]` items dropped.
bare({?MODULE, Cmd}) ->
    Cmd;
bare(Cmds) when is_list(Cmds) ->
    lists:reverse(flatten(Cmds, [])).

wire_arg({?MODULE, _When, [Op | _] = Cmd}) when is_integer(Op) ->
    to_wire(Cmd);
wire_arg({?MODULE, _When, Cmds}) ->
    [to_wire(C) || C <- Cmds];
wire_arg(List) when is_list(List) ->
    [wire_arg(A) || A <- List];
wire_arg(Map) when is_map(Map) ->
    #{K => wire_arg(V) || K := V <- Map};
wire_arg(Arg) ->
    Arg.

%% A nested single command is handed to `Fun` as a list of one and rebuilt as one.
nested_arg(Fun, {?MODULE, When, [Op | _] = Cmd}, Acc0) when is_integer(Op) ->
    {[New], Acc} = Fun(When, [Cmd], Acc0),
    {{?MODULE, When, New}, Acc};
nested_arg(Fun, {?MODULE, When, Cmds}, Acc0) ->
    {New, Acc} = Fun(When, Cmds, Acc0),
    {{?MODULE, When, New}, Acc};
nested_arg(Fun, List, Acc0) when is_list(List) ->
    lists:mapfoldl(fun(A, Acc) -> nested_arg(Fun, A, Acc) end, Acc0, List);
nested_arg(Fun, Map, Acc0) when is_map(Map) ->
    {Pairs, Acc} = lists:mapfoldl(
        fun({K, V}, Acc1) ->
            {V1, Acc2} = nested_arg(Fun, V, Acc1),
            {{K, V1}, Acc2}
        end,
        Acc0,
        maps:to_list(Map)
    ),
    {maps:from_list(Pairs), Acc};
nested_arg(_Fun, Arg, Acc) ->
    {Arg, Acc}.

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

%% Refuse a navigate/patch `flash` opt in a command or in a command it nests.
refuse_flash([Op, _Path, #{flash := _}]) when Op =:= ?EFFECT_NAVIGATE; Op =:= ?EFFECT_PATCH ->
    erlang:error({flash_outside_live_effect, op_name(Op)}, none, [
        {error_info, #{module => ?MODULE}}
    ]);
refuse_flash(Cmd) ->
    {_Cmd, ok} = mapfold_nested(
        fun(_When, Inner, ok) ->
            ok = lists:foreach(fun(C) -> ok = refuse_flash(C) end, Inner),
            {Inner, ok}
        end,
        ok,
        Cmd
    ),
    ok.

op_name(?EFFECT_NAVIGATE) -> navigate;
op_name(?EFFECT_PATCH) -> patch.

flash_message(Op) ->
    io_lib:format(
        "a ~s effect's `flash` opt only reaches its target when a live handler "
        "returns the effect: the socket carries the flash to the navigation that "
        "follows. In an event attribute or a controller response nothing carries "
        "it, so it would be lost and its content shipped to the page. From a "
        "template, push an event whose handler returns the ~s; from a controller, "
        "set the flash with arizona_req:put_flash/3 on the request the action "
        "returns and navigate with #{full => true}, so the browser's next GET "
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

%% `json:encode/2`'s encoder for `encode/1`: a string comes out attribute-escaped,
%% delimiters included, and so does a map key, which `json` would otherwise quote
%% itself for an integer or float key. Everything else is the default encoding,
%% which hands nested values and the string of an atom back to this function.
attr_encode(Bin, _Encode) when is_binary(Bin) ->
    attr_string(Bin);
attr_encode(Map, Encode) when is_map(Map) ->
    %% `json:encode_map/2`'s walk, so the members come out in the same order.
    attr_object([[$,, attr_key(Key, Encode), $: | Encode(Value, Encode)] || Key := Value <- Map]);
attr_encode(Value, Encode) ->
    json:encode_value(Value, Encode).

attr_key(Key, Encode) when is_binary(Key) -> Encode(Key, Encode);
attr_key(Key, Encode) when is_atom(Key) -> Encode(atom_to_binary(Key, utf8), Encode);
attr_key(Key, _Encode) when is_integer(Key) -> [~"&quot;", integer_to_binary(Key), ~"&quot;"];
attr_key(Key, _Encode) when is_float(Key) -> [~"&quot;", float_to_binary(Key, [short]), ~"&quot;"].

attr_object([]) -> ~"{}";
attr_object([[_Comma | Member] | Rest]) -> [${, Member, Rest, $}].

%% `json` hands an unescaped string back whole between its quotes, so a string
%% with nothing to JSON-escape -- holding no `"` either -- only needs its `&` and
%% `<` escaped, which for the usual one is none.
attr_string(Bin) ->
    case json:encode_binary(Bin) of
        [$", Bin, $"] -> [~"&quot;", escape_attr(Bin), ~"&quot;"];
        Json -> escape_attr(iolist_to_binary(Json))
    end.

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

%% --------------------------------------------------------------------
%% arizona_command_builder callbacks
%% --------------------------------------------------------------------

-doc false.
-spec target() -> atom() | all.
target() -> all.

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
