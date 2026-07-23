-module(arizona_os).
-moduledoc """
Builders for **native-shell** (OS) capability commands.

When an Arizona app runs inside a native shell (Electron, Tauri, a webview
wrapper, ...) rather than a plain browser, the server can drive OS-level
capabilities the browser sandbox forbids -- window control, native
notifications, screen-capture protection, and so on. Each builder returns an
`t:arizona_effect:cmd/0` tuple `{arizona_effect, [OpCode | Args]}` -- the shared
wire format every client-command module produces (op codes in
`include/arizona_effect.hrl`), encoded by `arizona_effect:encode/1` (the same web
path as `arizona_js`: HTML-attribute-safe in event props, raw JSON on the WS
frame). Use them as effects from `handle_event/3`/`handle_info/2` or as event
props.

Unlike `arizona_js` (one typed op per browser action), shell capabilities are
**shell-implemented and open-ended**: the engine is a pure pass-through and does
not know what any capability does. So every command funnels through a single
generic op (`?EFFECT_OS`) carrying a capability **name** plus args; the shell
owns the vocabulary. New capabilities are new names, not new op codes. A command
whose capability the shell does not implement (or any command in a plain browser,
where there is no shell) is a **safe no-op**.

The typed sugars below are the documented path; `command/2` is the unchecked
escape hatch for a capability without a sugar (a misspelled name is a silent
no-op, so prefer a sugar).

```erlang
handle_event(~"rename", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_os:set_title(~"New title")]}.
```

Whether the running shell offers a capability is read with `?capability(Name)`
(`arizona_live:capability/1`); it reflects an unauthenticated client claim, so it
is a UI/effect hint only and must never gate a server-side authorization
decision.
""".

-include("arizona_effect.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([command/1]).
-export([command/2]).
-export([set_title/1]).
-export([focus/0]).
-export([minimize/0]).
-export([maximize/0]).
-export([fullscreen/1]).
-export([notify/1]).
-export([notify/2]).
-export([capture_protection/1]).
-export([open_window/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    command/1,
    command/2,
    set_title/1,
    focus/0,
    minimize/0,
    maximize/0,
    fullscreen/1,
    notify/1,
    notify/2,
    capture_protection/1,
    open_window/2
]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Issues a bare shell command by capability name (no args).".
-spec command(Name) -> arizona_effect:cmd() when
    Name :: binary().
command(Name) -> {arizona_effect, [?EFFECT_OS, Name]}.

-doc """
Issues a shell command by capability name with `Args` spliced after the name.
The escape hatch for a capability without a typed sugar; a misspelled `Name` is
a silent no-op, so prefer a sugar.
""".
-spec command(Name, Args) -> arizona_effect:cmd() when
    Name :: binary(),
    Args :: list().
command(Name, Args) when is_list(Args) -> {arizona_effect, [?EFFECT_OS, Name | Args]}.

-doc "Sets the shell window title.".
-spec set_title(Title) -> arizona_effect:cmd() when
    Title :: binary().
set_title(Title) -> command(~"window_title", [Title]).

-doc "Focuses (raises) the shell window.".
-spec focus() -> arizona_effect:cmd().
focus() -> command(~"window_focus").

-doc "Minimizes the shell window.".
-spec minimize() -> arizona_effect:cmd().
minimize() -> command(~"window_minimize").

-doc "Maximizes the shell window.".
-spec maximize() -> arizona_effect:cmd().
maximize() -> command(~"window_maximize").

-doc "Enters (`true`) or leaves (`false`) fullscreen.".
-spec fullscreen(Enabled) -> arizona_effect:cmd() when
    Enabled :: boolean().
fullscreen(Enabled) -> command(~"window_fullscreen", [Enabled]).

-doc "Shows a native notification with `Title`.".
-spec notify(Title) -> arizona_effect:cmd() when
    Title :: binary().
notify(Title) -> command(~"notify", [Title]).

-doc "Shows a native notification with `Title` and options (e.g. `body`).".
-spec notify(Title, Opts) -> arizona_effect:cmd() when
    Title :: binary(),
    Opts :: map().
notify(Title, Opts) -> command(~"notify", [Title, Opts]).

-doc """
Excludes (`true`) or restores (`false`) the shell window from OS screen capture
and live shares. Idempotent: safe to re-assert from server state on reconnect.
""".
-spec capture_protection(Enabled) -> arizona_effect:cmd() when
    Enabled :: boolean().
capture_protection(Enabled) -> command(~"screen_capture_protection", [Enabled]).

-doc """
Opens a secondary shell window at `Url`. `Opts` is a map of window hints:
`width` / `height` (integers) and `always_on_top` (boolean). Fire-and-forget,
like every OS command; a plain browser (or a shell without the capability) is a
safe no-op.
""".
-spec open_window(Url, Opts) -> arizona_effect:cmd() when
    Url :: binary(),
    Opts :: map().
open_window(Url, Opts) -> command(~"open_window", [Url, Opts]).
