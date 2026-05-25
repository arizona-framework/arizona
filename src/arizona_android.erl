-module(arizona_android).
-moduledoc """
Builders for **Android** client effects, used inside `?native` views.

Each function returns an `t:arizona_effect:cmd/0` tuple
`{arizona_effect, [OpCode | Args]}` -- the shared wire format every client
command module produces (op codes in `include/arizona_effect.hrl`), encoded by
`arizona_effect:encode_json/1`. Use them as event props (`on_tap`, ...) or as
effects from `handle_event/3`.

Commands are per platform on purpose: a native view reaches for `arizona_android`
(not the browser's `arizona_js`), and this module is the home for Android-specific
commands as they're added. `push_event`/`navigate` mirror their web counterparts.

```erlang
{'Button', [{on_tap, arizona_android:push_event(~"inc")}], [~"+"]}
```
""".

-include("arizona_effect.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([push_event/1]).
-export([push_event/2]).
-export([navigate/1]).
-export([navigate/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    push_event/1,
    push_event/2,
    navigate/1,
    navigate/2
]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Pushes a named event to the server.".
-spec push_event(Event) -> arizona_effect:cmd() when
    Event :: binary().
push_event(Event) -> {arizona_effect, [?EFFECT_PUSH_EVENT, Event]}.

-doc "Pushes a named event with an explicit payload.".
-spec push_event(Event, Payload) -> arizona_effect:cmd() when
    Event :: binary(),
    Payload :: map().
push_event(Event, Payload) -> {arizona_effect, [?EFFECT_PUSH_EVENT, Event, Payload]}.

-doc "Navigates the client to the server route `Path`.".
-spec navigate(Path) -> arizona_effect:cmd() when
    Path :: binary().
navigate(Path) -> {arizona_effect, [?EFFECT_NAVIGATE, Path]}.

-doc "Navigates to `Path` with options (e.g. `replace`).".
-spec navigate(Path, Opts) -> arizona_effect:cmd() when
    Path :: binary(),
    Opts :: map().
navigate(Path, Opts) -> {arizona_effect, [?EFFECT_NAVIGATE, Path, Opts]}.
