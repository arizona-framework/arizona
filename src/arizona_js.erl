-module(arizona_js).
-include("arizona_js.hrl").

-export([
    push_event/1, push_event/2,
    toggle/1,
    show/1,
    hide/1,
    add_class/2,
    remove_class/2,
    toggle_class/2,
    set_attr/3,
    remove_attr/2,
    dispatch_event/2,
    navigate/1, navigate/2,
    focus/1,
    blur/1,
    scroll_to/1, scroll_to/2,
    set_title/1,
    reload/0,
    on_key/2,
    encode/1
]).
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

-nominal cmd() :: {?MODULE, list()}.

-export_type([cmd/0]).

-spec push_event(binary()) -> cmd().
push_event(Event) -> {?MODULE, [?JS_PUSH_EVENT, Event]}.

-spec push_event(binary(), map()) -> cmd().
push_event(Event, Payload) -> {?MODULE, [?JS_PUSH_EVENT, Event, Payload]}.

-spec toggle(binary()) -> cmd().
toggle(Sel) -> {?MODULE, [?JS_TOGGLE, Sel]}.

-spec show(binary()) -> cmd().
show(Sel) -> {?MODULE, [?JS_SHOW, Sel]}.

-spec hide(binary()) -> cmd().
hide(Sel) -> {?MODULE, [?JS_HIDE, Sel]}.

-spec add_class(binary(), binary()) -> cmd().
add_class(Sel, Cls) -> {?MODULE, [?JS_ADD_CLASS, Sel, Cls]}.

-spec remove_class(binary(), binary()) -> cmd().
remove_class(Sel, Cls) -> {?MODULE, [?JS_REMOVE_CLASS, Sel, Cls]}.

-spec toggle_class(binary(), binary()) -> cmd().
toggle_class(Sel, Cls) -> {?MODULE, [?JS_TOGGLE_CLASS, Sel, Cls]}.

-spec set_attr(binary(), binary(), binary()) -> cmd().
set_attr(Sel, Attr, Val) -> {?MODULE, [?JS_SET_ATTR, Sel, Attr, Val]}.

-spec remove_attr(binary(), binary()) -> cmd().
remove_attr(Sel, Attr) -> {?MODULE, [?JS_REMOVE_ATTR, Sel, Attr]}.

-spec dispatch_event(binary(), map()) -> cmd().
dispatch_event(Name, Payload) -> {?MODULE, [?JS_DISPATCH_EVENT, Name, Payload]}.

-spec navigate(binary()) -> cmd().
navigate(Path) -> {?MODULE, [?JS_NAVIGATE, Path]}.

-spec navigate(binary(), map()) -> cmd().
navigate(Path, Opts) -> {?MODULE, [?JS_NAVIGATE, Path, Opts]}.

-spec focus(binary()) -> cmd().
focus(Sel) -> {?MODULE, [?JS_FOCUS, Sel]}.

-spec blur(binary()) -> cmd().
blur(Sel) -> {?MODULE, [?JS_BLUR, Sel]}.

-spec scroll_to(binary()) -> cmd().
scroll_to(Sel) -> {?MODULE, [?JS_SCROLL_TO, Sel]}.

-spec scroll_to(binary(), map()) -> cmd().
scroll_to(Sel, Opts) -> {?MODULE, [?JS_SCROLL_TO, Sel, Opts]}.

-spec set_title(binary()) -> cmd().
set_title(Title) -> {?MODULE, [?JS_SET_TITLE, Title]}.

-spec reload() -> cmd().
reload() -> {?MODULE, [?JS_RELOAD]}.

-spec on_key(atom() | [atom()] | binary(), cmd() | [cmd()]) -> cmd().
on_key(Key, {?MODULE, Inner}) ->
    {?MODULE, [?JS_ON_KEY, encode_key(Key), Inner]};
on_key(Key, [_ | _] = Cmds) ->
    {?MODULE, [?JS_ON_KEY, encode_key(Key), [C || {?MODULE, C} <:- Cmds]]}.

encode_key(Key) when is_atom(Key) -> [Key];
encode_key(Keys) when is_list(Keys) -> Keys;
encode_key(Pattern) when is_binary(Pattern) -> Pattern.

-spec encode(cmd() | [cmd()]) -> binary().
encode({?MODULE, Cmd}) ->
    escape_attr(iolist_to_binary(json:encode(Cmd)));
encode([{?MODULE, _} | _] = Cmds) ->
    escape_attr(iolist_to_binary(json:encode([C || {?MODULE, C} <:- Cmds]))).

escape_attr(Bin) ->
    B1 = binary:replace(Bin, <<"&">>, <<"&amp;">>, [global]),
    B2 = binary:replace(B1, <<"\"">>, <<"&quot;">>, [global]),
    binary:replace(B2, <<"<">>, <<"&lt;">>, [global]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

push_event_test() ->
    {arizona_js, [?JS_PUSH_EVENT, <<"inc">>]} = push_event(<<"inc">>).

push_event_with_payload_test() ->
    {arizona_js, [?JS_PUSH_EVENT, <<"ev">>, #{<<"k">> := <<"v">>}]} =
        push_event(<<"ev">>, #{<<"k">> => <<"v">>}).

toggle_test() ->
    {arizona_js, [?JS_TOGGLE, <<"#m">>]} = toggle(<<"#m">>).

dispatch_event_test() ->
    {arizona_js, [?JS_DISPATCH_EVENT, <<"ev">>, #{<<"x">> := 1}]} =
        dispatch_event(<<"ev">>, #{<<"x">> => 1}).

set_title_test() ->
    {arizona_js, [?JS_SET_TITLE, <<"T">>]} = set_title(<<"T">>).

reload_test() ->
    {arizona_js, [?JS_RELOAD]} = reload().

encode_single_test() ->
    Bin = encode(push_event(<<"inc">>)),
    ?assertEqual(<<"[0,&quot;inc&quot;]">>, Bin).

encode_multiple_test() ->
    Bin = encode([push_event(<<"inc">>), toggle(<<"#m">>)]),
    ?assertEqual(<<"[[0,&quot;inc&quot;],[1,&quot;#m&quot;]]">>, Bin).

encode_escapes_quotes_test() ->
    Bin = encode(push_event(<<"a\"b">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"&quot;">>)).

encode_escapes_ampersand_test() ->
    Bin = encode(push_event(<<"a&b">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"&amp;">>)).

encode_escapes_lt_test() ->
    Bin = encode(push_event(<<"<script>">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"&lt;">>)),
    ?assertEqual(nomatch, binary:match(Bin, <<"<script>">>)).

on_key_atom_test() ->
    {arizona_js, [?JS_ON_KEY, [enter], [?JS_PUSH_EVENT, <<"x">>]]} =
        on_key(enter, push_event(<<"x">>)).

on_key_list_test() ->
    {arizona_js, [?JS_ON_KEY, [enter, escape], [?JS_PUSH_EVENT, <<"x">>]]} =
        on_key([enter, escape], push_event(<<"x">>)).

on_key_regex_test() ->
    {arizona_js, [?JS_ON_KEY, <<"^[a-z]$">>, [?JS_PUSH_EVENT, <<"x">>]]} =
        on_key(<<"^[a-z]$">>, push_event(<<"x">>)).

on_key_multiple_cmds_test() ->
    {arizona_js, [?JS_ON_KEY, [enter], [[?JS_PUSH_EVENT, <<"x">>], [?JS_TOGGLE, <<"#m">>]]]} =
        on_key(enter, [push_event(<<"x">>), toggle(<<"#m">>)]).

on_key_empty_list_test() ->
    {arizona_js, [?JS_ON_KEY, [], [?JS_PUSH_EVENT, <<"x">>]]} =
        on_key([], push_event(<<"x">>)).

on_key_nested_test() ->
    Inner = on_key(enter, push_event(<<"x">>)),
    {arizona_js, [?JS_ON_KEY, [escape], [?JS_ON_KEY, [enter], [?JS_PUSH_EVENT, <<"x">>]]]} =
        on_key(escape, Inner).

on_key_encode_atom_test() ->
    Bin = encode(on_key(enter, push_event(<<"x">>))),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"[16">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"enter">>)).

on_key_encode_list_test() ->
    Bin = encode(on_key([enter, escape], push_event(<<"x">>))),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"enter">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"escape">>)).

on_key_encode_regex_test() ->
    Bin = encode(on_key(<<"^[a-z]$">>, push_event(<<"x">>))),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"^[a-z]$">>)).

-endif.
