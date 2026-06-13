-module(arizona_js).
-moduledoc """
Builders for **web** (browser/JavaScript) client effects.

Each function returns an `t:arizona_effect:cmd/0` tuple
`{arizona_effect, [OpCode | Args]}` -- the shared wire format every client
command module produces (op codes in `include/arizona_effect.hrl`), encoded by
`arizona_effect:encode/1`. Use them inside web event attributes like `az-click`
or as effects from `handle_event/3`.

Most of these are browser/DOM-only (`toggle`, the class ops, `set_attr`,
`dispatch_event`, ...): they address the DOM by CSS selector or touch
document/window state. `push_event` and `navigate` have portable counterparts
in the per-platform modules (e.g. `arizona_android`) for `?native` views.

## Example

```erlang
{'button', [{az_click, [
    arizona_js:push_event(~"inc"),
    arizona_js:toggle(~"#modal")
]}], [~"Both"]}
```
""".

-include("arizona_effect.hrl").

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
-export([set/2]).
-export([set/3]).
-export([set_all/2]).
-export([request_pip/1]).
-export([exit_pip/1]).
-export([transition/0]).
-export([transition/1]).

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
    on_key/2,
    set/2,
    set/3,
    set_all/2,
    request_pip/1,
    exit_pip/1
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
%% API Functions
%% --------------------------------------------------------------------

-doc """
Pushes a named event to the server. Auto-collects payload from
sibling inputs/forms when used inside one.
""".
-spec push_event(Event) -> arizona_effect:cmd() when
    Event :: binary().
push_event(Event) -> {arizona_effect, [?EFFECT_PUSH_EVENT, Event]}.

-doc """
Pushes a named event with an explicit payload, merged on top of any
auto-collected form data.
""".
-spec push_event(Event, Payload) -> arizona_effect:cmd() when
    Event :: binary(),
    Payload :: map().
push_event(Event, Payload) -> {arizona_effect, [?EFFECT_PUSH_EVENT, Event, Payload]}.

-doc "Toggles the visibility of elements matching the CSS selector.".
-spec toggle(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
toggle(Sel) -> {arizona_effect, [?EFFECT_TOGGLE, Sel]}.

-doc "Shows elements matching the CSS selector.".
-spec show(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
show(Sel) -> {arizona_effect, [?EFFECT_SHOW, Sel]}.

-doc "Hides elements matching the CSS selector.".
-spec hide(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
hide(Sel) -> {arizona_effect, [?EFFECT_HIDE, Sel]}.

-doc "Adds a CSS class to elements matching the selector.".
-spec add_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
add_class(Sel, Cls) -> {arizona_effect, [?EFFECT_ADD_CLASS, Sel, Cls]}.

-doc "Removes a CSS class from elements matching the selector.".
-spec remove_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
remove_class(Sel, Cls) -> {arizona_effect, [?EFFECT_REMOVE_CLASS, Sel, Cls]}.

-doc "Toggles a CSS class on elements matching the selector.".
-spec toggle_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
toggle_class(Sel, Cls) -> {arizona_effect, [?EFFECT_TOGGLE_CLASS, Sel, Cls]}.

-doc "Sets an attribute on elements matching the selector.".
-spec set_attr(Selector, Attr, Value) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary(),
    Value :: binary().
set_attr(Sel, Attr, Val) -> {arizona_effect, [?EFFECT_SET_ATTR, Sel, Attr, Val]}.

-doc "Removes an attribute from elements matching the selector.".
-spec remove_attr(Selector, Attr) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary().
remove_attr(Sel, Attr) -> {arizona_effect, [?EFFECT_REMOVE_ATTR, Sel, Attr]}.

-doc "Dispatches a custom DOM event with a payload.".
-spec dispatch_event(Name, Payload) -> arizona_effect:cmd() when
    Name :: binary(),
    Payload :: map().
dispatch_event(Name, Payload) -> {arizona_effect, [?EFFECT_DISPATCH_EVENT, Name, Payload]}.

-doc "Triggers a SPA navigation to `Path`.".
-spec navigate(Path) -> arizona_effect:cmd() when
    Path :: binary().
navigate(Path) -> {arizona_effect, [?EFFECT_NAVIGATE, Path]}.

-doc "Triggers a SPA navigation to `Path` with options (e.g. `replace`).".
-spec navigate(Path, Opts) -> arizona_effect:cmd() when
    Path :: binary(),
    Opts :: map().
navigate(Path, Opts) -> {arizona_effect, [?EFFECT_NAVIGATE, Path, Opts]}.

-doc "Focuses the first element matching the selector.".
-spec focus(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
focus(Sel) -> {arizona_effect, [?EFFECT_FOCUS, Sel]}.

-doc "Blurs (removes focus from) the first element matching the selector.".
-spec blur(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
blur(Sel) -> {arizona_effect, [?EFFECT_BLUR, Sel]}.

-doc "Scrolls to the first element matching the selector.".
-spec scroll_to(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
scroll_to(Sel) -> {arizona_effect, [?EFFECT_SCROLL_TO, Sel]}.

-doc """
Scrolls to the first element matching the selector with options
(e.g. `behavior: smooth`).
""".
-spec scroll_to(Selector, Opts) -> arizona_effect:cmd() when
    Selector :: binary(),
    Opts :: map().
scroll_to(Sel, Opts) -> {arizona_effect, [?EFFECT_SCROLL_TO, Sel, Opts]}.

-doc "Sets the document title.".
-spec set_title(Title) -> arizona_effect:cmd() when
    Title :: binary().
set_title(Title) -> {arizona_effect, [?EFFECT_SET_TITLE, Title]}.

-doc "Reloads the current page.".
-spec reload() -> arizona_effect:cmd().
reload() -> {arizona_effect, [?EFFECT_RELOAD]}.

-doc """
Wraps a command (or list of commands) so it only fires when a key
matching `Key` is pressed. `Key` can be an atom (e.g. `enter`), a
list of atoms (matches any), or a regex pattern as a binary.
""".
-spec on_key(Key, Cmd) -> arizona_effect:cmd() when
    Key :: atom() | [atom()] | binary(),
    Cmd :: arizona_effect:cmd() | [arizona_effect:cmd()].
on_key(Key, {arizona_effect, Inner}) ->
    {arizona_effect, [?EFFECT_ON_KEY, encode_key(Key), Inner]};
on_key(Key, [_ | _] = Cmds) ->
    {arizona_effect, [?EFFECT_ON_KEY, encode_key(Key), [C || {arizona_effect, C} <:- Cmds]]}.

-doc """
Sets a client-owned slot (`?local`) to `Value` in the **closest view** of the
triggering element, updating every slot with `Key` locally with no server
round-trip. Use inside web event attributes like `az-click`.
""".
-spec set(Key, Value) -> arizona_effect:cmd() when
    Key :: binary() | atom(),
    Value :: binary() | boolean() | number().
set(Key, Value) -> {arizona_effect, [?EFFECT_SET_LOCAL, Key, Value]}.

-doc """
Like `set/2` but targets the slot in the view identified by `ViewId`.
""".
-spec set(ViewId, Key, Value) -> arizona_effect:cmd() when
    ViewId :: binary(),
    Key :: binary() | atom(),
    Value :: binary() | boolean() | number().
set(ViewId, Key, Value) -> {arizona_effect, [?EFFECT_SET_LOCAL, Key, Value, ViewId]}.

-doc """
Like `set/2` but updates the slot in **every** view on the page (document-wide).
""".
-spec set_all(Key, Value) -> arizona_effect:cmd() when
    Key :: binary() | atom(),
    Value :: binary() | boolean() | number().
set_all(Key, Value) -> {arizona_effect, [?EFFECT_SET_LOCAL, Key, Value, true]}.

-doc """
Moves the view identified by `ViewId` into a floating Document Picture-in-Picture
window, kept live by the server diff stream. Use inside a web event attribute
(e.g. `az-click`) -- Document PiP requires a user gesture, so it won't open from
a server-pushed handler effect.
""".
-spec request_pip(ViewId) -> arizona_effect:cmd() when
    ViewId :: binary().
request_pip(ViewId) -> {arizona_effect, [?EFFECT_REQUEST_PIP, ViewId]}.

-doc """
Closes the floating Picture-in-Picture window for `ViewId`, moving the view back
inline. No-op if the view isn't popped out.
""".
-spec exit_pip(ViewId) -> arizona_effect:cmd() when
    ViewId :: binary().
exit_pip(ViewId) -> {arizona_effect, [?EFFECT_EXIT_PIP, ViewId]}.

-doc """
Starts a view transition (`document.startViewTransition`) for the next DOM change
in this interaction. Compose it before a `navigate/1,2` to animate an SPA
navigation, or before a client-side effect (e.g. `toggle/1`) in an `az-click`
list to animate that change. Equivalent to `transition/1` with no types.
""".
-spec transition() -> arizona_effect:cmd().
transition() -> {arizona_effect, [?EFFECT_TRANSITION]}.

-doc """
Like `transition/0` but with options. `types` is a list of view-transition type
names activated for the transition (matched by `:active-view-transition-type(...)`
CSS), letting one stylesheet pick different animations per navigation.
""".
-spec transition(Opts) -> arizona_effect:cmd() when
    Opts :: #{types => [binary()]}.
transition(Opts) -> {arizona_effect, [?EFFECT_TRANSITION, Opts]}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

encode_key(Key) when is_atom(Key) -> [Key];
encode_key(Keys) when is_list(Keys) -> Keys;
encode_key(Pattern) when is_binary(Pattern) -> Pattern.

-ifdef(TEST).

push_event_test() ->
    {arizona_effect, [?EFFECT_PUSH_EVENT, ~"inc"]} = push_event(~"inc").

push_event_with_payload_test() ->
    {arizona_effect, [?EFFECT_PUSH_EVENT, ~"ev", #{~"k" := ~"v"}]} =
        push_event(~"ev", #{~"k" => ~"v"}).

toggle_test() ->
    {arizona_effect, [?EFFECT_TOGGLE, ~"#m"]} = toggle(~"#m").

dispatch_event_test() ->
    {arizona_effect, [?EFFECT_DISPATCH_EVENT, ~"ev", #{~"x" := 1}]} =
        dispatch_event(~"ev", #{~"x" => 1}).

set_title_test() ->
    {arizona_effect, [?EFFECT_SET_TITLE, ~"T"]} = set_title(~"T").

reload_test() ->
    {arizona_effect, [?EFFECT_RELOAD]} = reload().

set_test() ->
    {arizona_effect, [?EFFECT_SET_LOCAL, ~"k", ~"v"]} = set(~"k", ~"v"),
    %% An atom key is accepted and passed through (json stringifies it on the wire).
    {arizona_effect, [?EFFECT_SET_LOCAL, foo, ~"v"]} = set(foo, ~"v").

set_view_test() ->
    {arizona_effect, [?EFFECT_SET_LOCAL, ~"k", true, ~"view1"]} = set(~"view1", ~"k", true).

set_all_test() ->
    {arizona_effect, [?EFFECT_SET_LOCAL, ~"k", ~"v", true]} = set_all(~"k", ~"v").

request_pip_test() ->
    {arizona_effect, [?EFFECT_REQUEST_PIP, ~"v"]} = request_pip(~"v").

exit_pip_test() ->
    {arizona_effect, [?EFFECT_EXIT_PIP, ~"v"]} = exit_pip(~"v").

transition_test() ->
    {arizona_effect, [?EFFECT_TRANSITION]} = transition().

transition_with_opts_test() ->
    {arizona_effect, [?EFFECT_TRANSITION, #{types := [~"slide"]}]} =
        transition(#{types => [~"slide"]}).

builders_test() ->
    ?assertEqual({arizona_effect, [?EFFECT_SHOW, ~"#m"]}, show(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_HIDE, ~"#m"]}, hide(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_ADD_CLASS, ~"#m", ~"c"]}, add_class(~"#m", ~"c")),
    ?assertEqual({arizona_effect, [?EFFECT_REMOVE_CLASS, ~"#m", ~"c"]}, remove_class(~"#m", ~"c")),
    ?assertEqual({arizona_effect, [?EFFECT_TOGGLE_CLASS, ~"#m", ~"c"]}, toggle_class(~"#m", ~"c")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_SET_ATTR, ~"#m", ~"a", ~"v"]}, set_attr(~"#m", ~"a", ~"v")
    ),
    ?assertEqual({arizona_effect, [?EFFECT_REMOVE_ATTR, ~"#m", ~"a"]}, remove_attr(~"#m", ~"a")),
    ?assertEqual({arizona_effect, [?EFFECT_NAVIGATE, ~"/p"]}, navigate(~"/p")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_NAVIGATE, ~"/p", #{replace => true}]},
        navigate(~"/p", #{replace => true})
    ),
    ?assertEqual({arizona_effect, [?EFFECT_FOCUS, ~"#m"]}, focus(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_BLUR, ~"#m"]}, blur(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_SCROLL_TO, ~"#m"]}, scroll_to(~"#m")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_SCROLL_TO, ~"#m", #{behavior => smooth}]},
        scroll_to(~"#m", #{behavior => smooth})
    ).

on_key_atom_test() ->
    {arizona_effect, [?EFFECT_ON_KEY, [enter], [?EFFECT_PUSH_EVENT, ~"x"]]} =
        on_key(enter, push_event(~"x")).

on_key_list_test() ->
    {arizona_effect, [?EFFECT_ON_KEY, [enter, escape], [?EFFECT_PUSH_EVENT, ~"x"]]} =
        on_key([enter, escape], push_event(~"x")).

on_key_regex_test() ->
    {arizona_effect, [?EFFECT_ON_KEY, ~"^[a-z]$", [?EFFECT_PUSH_EVENT, ~"x"]]} =
        on_key(~"^[a-z]$", push_event(~"x")).

on_key_multiple_cmds_test() ->
    {arizona_effect, [
        ?EFFECT_ON_KEY, [enter], [[?EFFECT_PUSH_EVENT, ~"x"], [?EFFECT_TOGGLE, ~"#m"]]
    ]} =
        on_key(enter, [push_event(~"x"), toggle(~"#m")]).

on_key_empty_list_test() ->
    {arizona_effect, [?EFFECT_ON_KEY, [], [?EFFECT_PUSH_EVENT, ~"x"]]} =
        on_key([], push_event(~"x")).

on_key_nested_test() ->
    Inner = on_key(enter, push_event(~"x")),
    {arizona_effect, [
        ?EFFECT_ON_KEY, [escape], [?EFFECT_ON_KEY, [enter], [?EFFECT_PUSH_EVENT, ~"x"]]
    ]} =
        on_key(escape, Inner).

on_key_encode_atom_test() ->
    Bin = arizona_effect:encode(on_key(enter, push_event(~"x"))),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"[16")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"enter")).

-endif.
