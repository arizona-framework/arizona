-module(arizona_js).
-moduledoc """
Builders and encoder for client-side JavaScript commands.

Each function returns a `t:cmd/0` tuple `{arizona_js, [OpCode | Args]}`
that the client runtime knows how to execute. Use them inside event
attributes like `az-click` or as effects from `handle_event/3`.

The op codes are defined in `include/arizona_js.hrl` and shared with
the JS runtime so both sides agree on the wire format.

## Example

```erlang
%% Template event attribute
{'button', [{az_click, arizona_js:push_event(~"inc")}], [~"+"]}

%% Multiple commands chained
{'button', [{az_click, [
    arizona_js:push_event(~"inc"),
    arizona_js:toggle(~"#modal")
]}], [~"Both"]}

%% Effects returned from a handler
handle_event(~"inc", _Params, Bindings) ->
    {Bindings#{count := Count + 1}, #{}, [arizona_js:set_title(~"Updated")]}.
```

## Encoding

`encode/1` serializes a command (or list of commands) to JSON, then
HTML-escapes `&`, `"`, and `<` so the result can be safely placed
inside an HTML attribute value.
""".

-include("arizona_js.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([push_event/1]).
-export([push_event/2]).
-export([toggle/1]).
-export([show/1]).
-export([hide/1]).
-export([add_class/2]).
-export([remove_class/2]).
-export([toggle_class/2]).
-export([set_attr/3]).
-export([remove_attr/2]).
-export([dispatch_event/2]).
-export([navigate/1]).
-export([navigate/2]).
-export([focus/1]).
-export([blur/1]).
-export([scroll_to/1]).
-export([scroll_to/2]).
-export([set_title/1]).
-export([reload/0]).
-export([on_key/2]).
-export([encode/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    push_event/1,
    push_event/2,
    toggle/1,
    show/1,
    hide/1,
    add_class/2,
    remove_class/2,
    toggle_class/2,
    set_attr/3,
    remove_attr/2,
    dispatch_event/2,
    navigate/1,
    navigate/2,
    focus/1,
    blur/1,
    scroll_to/1,
    scroll_to/2,
    set_title/1,
    reload/0,
    on_key/2
]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-ifdef(TEST).
%% Inline EUnit tests intentionally repeat command tuples to verify
%% structural equality at different nesting levels.
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
Pushes a named event to the server. Auto-collects payload from
sibling inputs/forms when used inside one.
""".
-spec push_event(Event) -> cmd() when
    Event :: binary().
push_event(Event) -> {?MODULE, [?JS_PUSH_EVENT, Event]}.

-doc """
Pushes a named event with an explicit payload, merged on top of any
auto-collected form data.
""".
-spec push_event(Event, Payload) -> cmd() when
    Event :: binary(),
    Payload :: map().
push_event(Event, Payload) -> {?MODULE, [?JS_PUSH_EVENT, Event, Payload]}.

-doc """
Toggles the visibility of elements matching the CSS selector.
""".
-spec toggle(Selector) -> cmd() when
    Selector :: binary().
toggle(Sel) -> {?MODULE, [?JS_TOGGLE, Sel]}.

-doc """
Shows elements matching the CSS selector.
""".
-spec show(Selector) -> cmd() when
    Selector :: binary().
show(Sel) -> {?MODULE, [?JS_SHOW, Sel]}.

-doc """
Hides elements matching the CSS selector.
""".
-spec hide(Selector) -> cmd() when
    Selector :: binary().
hide(Sel) -> {?MODULE, [?JS_HIDE, Sel]}.

-doc """
Adds a CSS class to elements matching the selector.
""".
-spec add_class(Selector, Class) -> cmd() when
    Selector :: binary(),
    Class :: binary().
add_class(Sel, Cls) -> {?MODULE, [?JS_ADD_CLASS, Sel, Cls]}.

-doc """
Removes a CSS class from elements matching the selector.
""".
-spec remove_class(Selector, Class) -> cmd() when
    Selector :: binary(),
    Class :: binary().
remove_class(Sel, Cls) -> {?MODULE, [?JS_REMOVE_CLASS, Sel, Cls]}.

-doc """
Toggles a CSS class on elements matching the selector.
""".
-spec toggle_class(Selector, Class) -> cmd() when
    Selector :: binary(),
    Class :: binary().
toggle_class(Sel, Cls) -> {?MODULE, [?JS_TOGGLE_CLASS, Sel, Cls]}.

-doc """
Sets an attribute on elements matching the selector.
""".
-spec set_attr(Selector, Attr, Value) -> cmd() when
    Selector :: binary(),
    Attr :: binary(),
    Value :: binary().
set_attr(Sel, Attr, Val) -> {?MODULE, [?JS_SET_ATTR, Sel, Attr, Val]}.

-doc """
Removes an attribute from elements matching the selector.
""".
-spec remove_attr(Selector, Attr) -> cmd() when
    Selector :: binary(),
    Attr :: binary().
remove_attr(Sel, Attr) -> {?MODULE, [?JS_REMOVE_ATTR, Sel, Attr]}.

-doc """
Dispatches a custom DOM event with a payload.
""".
-spec dispatch_event(Name, Payload) -> cmd() when
    Name :: binary(),
    Payload :: map().
dispatch_event(Name, Payload) -> {?MODULE, [?JS_DISPATCH_EVENT, Name, Payload]}.

-doc """
Triggers a SPA navigation to `Path`.
""".
-spec navigate(Path) -> cmd() when
    Path :: binary().
navigate(Path) -> {?MODULE, [?JS_NAVIGATE, Path]}.

-doc """
Triggers a SPA navigation to `Path` with options (e.g. `replace`).
""".
-spec navigate(Path, Opts) -> cmd() when
    Path :: binary(),
    Opts :: map().
navigate(Path, Opts) -> {?MODULE, [?JS_NAVIGATE, Path, Opts]}.

-doc """
Focuses the first element matching the selector.
""".
-spec focus(Selector) -> cmd() when
    Selector :: binary().
focus(Sel) -> {?MODULE, [?JS_FOCUS, Sel]}.

-doc """
Blurs (removes focus from) the first element matching the selector.
""".
-spec blur(Selector) -> cmd() when
    Selector :: binary().
blur(Sel) -> {?MODULE, [?JS_BLUR, Sel]}.

-doc """
Scrolls to the first element matching the selector.
""".
-spec scroll_to(Selector) -> cmd() when
    Selector :: binary().
scroll_to(Sel) -> {?MODULE, [?JS_SCROLL_TO, Sel]}.

-doc """
Scrolls to the first element matching the selector with options
(e.g. `behavior: smooth`).
""".
-spec scroll_to(Selector, Opts) -> cmd() when
    Selector :: binary(),
    Opts :: map().
scroll_to(Sel, Opts) -> {?MODULE, [?JS_SCROLL_TO, Sel, Opts]}.

-doc """
Sets the document title.
""".
-spec set_title(Title) -> cmd() when
    Title :: binary().
set_title(Title) -> {?MODULE, [?JS_SET_TITLE, Title]}.

-doc """
Reloads the current page.
""".
-spec reload() -> cmd().
reload() -> {?MODULE, [?JS_RELOAD]}.

-doc """
Wraps a command (or list of commands) so it only fires when a key
matching `Key` is pressed. `Key` can be an atom (e.g. `enter`), a
list of atoms (matches any), or a regex pattern as a binary.
""".
-spec on_key(Key, Cmd) -> cmd() when
    Key :: atom() | [atom()] | binary(),
    Cmd :: cmd() | [cmd()].
on_key(Key, {?MODULE, Inner}) ->
    {?MODULE, [?JS_ON_KEY, encode_key(Key), Inner]};
on_key(Key, [_ | _] = Cmds) ->
    {?MODULE, [?JS_ON_KEY, encode_key(Key), [C || {?MODULE, C} <:- Cmds]]}.

-doc """
Encodes a command or list of commands as an HTML-attribute-safe binary.

The output is JSON with `&`, `"`, and `<` escaped to their HTML entity
equivalents so it can be embedded inside an attribute value.
""".
-spec encode(Cmds) -> binary() when
    Cmds :: cmd() | [cmd()].
encode({?MODULE, Cmd}) ->
    escape_attr(iolist_to_binary(json:encode(Cmd)));
encode([{?MODULE, _} | _] = Cmds) ->
    escape_attr(iolist_to_binary(json:encode([C || {?MODULE, C} <:- Cmds]))).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

encode_key(Key) when is_atom(Key) -> [Key];
encode_key(Keys) when is_list(Keys) -> Keys;
encode_key(Pattern) when is_binary(Pattern) -> Pattern.

%% SWAR (SIMD Within A Register) byte scanner. Matches a 56-bit (7-byte)
%% word at a time and uses bitwise arithmetic to validate that none of
%% the 7 bytes is `&`, `"`, or `<` -- the only chars we need to escape.
%% On match, advances 7 bytes via a Skip/Len counter without copying
%% anything; the iolist Acc only grows when an escape is actually
%% emitted. For typical JS payloads (~30-50 bytes, mostly safe) this
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

push_event_test() ->
    {arizona_js, [?JS_PUSH_EVENT, ~"inc"]} = push_event(~"inc").

push_event_with_payload_test() ->
    {arizona_js, [?JS_PUSH_EVENT, ~"ev", #{~"k" := ~"v"}]} =
        push_event(~"ev", #{~"k" => ~"v"}).

toggle_test() ->
    {arizona_js, [?JS_TOGGLE, ~"#m"]} = toggle(~"#m").

dispatch_event_test() ->
    {arizona_js, [?JS_DISPATCH_EVENT, ~"ev", #{~"x" := 1}]} =
        dispatch_event(~"ev", #{~"x" => 1}).

set_title_test() ->
    {arizona_js, [?JS_SET_TITLE, ~"T"]} = set_title(~"T").

reload_test() ->
    {arizona_js, [?JS_RELOAD]} = reload().

encode_single_test() ->
    Bin = encode(push_event(~"inc")),
    ?assertEqual(~"[0,&quot;inc&quot;]", Bin).

encode_multiple_test() ->
    Bin = encode([push_event(~"inc"), toggle(~"#m")]),
    ?assertEqual(~"[[0,&quot;inc&quot;],[1,&quot;#m&quot;]]", Bin).

encode_escapes_quotes_test() ->
    Bin = encode(push_event(~"a\"b")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"&quot;")).

encode_escapes_ampersand_test() ->
    Bin = encode(push_event(~"a&b")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"&amp;")).

encode_escapes_lt_test() ->
    Bin = encode(push_event(~"<script>")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"&lt;")),
    ?assertEqual(nomatch, binary:match(Bin, ~"<script>")).

on_key_atom_test() ->
    {arizona_js, [?JS_ON_KEY, [enter], [?JS_PUSH_EVENT, ~"x"]]} =
        on_key(enter, push_event(~"x")).

on_key_list_test() ->
    {arizona_js, [?JS_ON_KEY, [enter, escape], [?JS_PUSH_EVENT, ~"x"]]} =
        on_key([enter, escape], push_event(~"x")).

on_key_regex_test() ->
    {arizona_js, [?JS_ON_KEY, ~"^[a-z]$", [?JS_PUSH_EVENT, ~"x"]]} =
        on_key(~"^[a-z]$", push_event(~"x")).

on_key_multiple_cmds_test() ->
    {arizona_js, [?JS_ON_KEY, [enter], [[?JS_PUSH_EVENT, ~"x"], [?JS_TOGGLE, ~"#m"]]]} =
        on_key(enter, [push_event(~"x"), toggle(~"#m")]).

on_key_empty_list_test() ->
    {arizona_js, [?JS_ON_KEY, [], [?JS_PUSH_EVENT, ~"x"]]} =
        on_key([], push_event(~"x")).

builders_test() ->
    %% Each builder returns `{arizona_js, [OpCode, ...Args]}` unchanged.
    ?assertEqual({arizona_js, [?JS_SHOW, ~"#m"]}, show(~"#m")),
    ?assertEqual({arizona_js, [?JS_HIDE, ~"#m"]}, hide(~"#m")),
    ?assertEqual({arizona_js, [?JS_ADD_CLASS, ~"#m", ~"c"]}, add_class(~"#m", ~"c")),
    ?assertEqual({arizona_js, [?JS_REMOVE_CLASS, ~"#m", ~"c"]}, remove_class(~"#m", ~"c")),
    ?assertEqual({arizona_js, [?JS_TOGGLE_CLASS, ~"#m", ~"c"]}, toggle_class(~"#m", ~"c")),
    ?assertEqual({arizona_js, [?JS_SET_ATTR, ~"#m", ~"a", ~"v"]}, set_attr(~"#m", ~"a", ~"v")),
    ?assertEqual({arizona_js, [?JS_REMOVE_ATTR, ~"#m", ~"a"]}, remove_attr(~"#m", ~"a")),
    ?assertEqual({arizona_js, [?JS_NAVIGATE, ~"/p"]}, navigate(~"/p")),
    ?assertEqual(
        {arizona_js, [?JS_NAVIGATE, ~"/p", #{replace => true}]},
        navigate(~"/p", #{replace => true})
    ),
    ?assertEqual({arizona_js, [?JS_FOCUS, ~"#m"]}, focus(~"#m")),
    ?assertEqual({arizona_js, [?JS_BLUR, ~"#m"]}, blur(~"#m")),
    ?assertEqual({arizona_js, [?JS_SCROLL_TO, ~"#m"]}, scroll_to(~"#m")),
    ?assertEqual(
        {arizona_js, [?JS_SCROLL_TO, ~"#m", #{behavior => smooth}]},
        scroll_to(~"#m", #{behavior => smooth})
    ).

on_key_nested_test() ->
    Inner = on_key(enter, push_event(~"x")),
    {arizona_js, [?JS_ON_KEY, [escape], [?JS_ON_KEY, [enter], [?JS_PUSH_EVENT, ~"x"]]]} =
        on_key(escape, Inner).

on_key_encode_atom_test() ->
    Bin = encode(on_key(enter, push_event(~"x"))),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"[16")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"enter")).

on_key_encode_list_test() ->
    Bin = encode(on_key([enter, escape], push_event(~"x"))),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"enter")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"escape")).

on_key_encode_regex_test() ->
    Bin = encode(on_key(~"^[a-z]$", push_event(~"x"))),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"^[a-z]$")).

-endif.
