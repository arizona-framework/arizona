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
-export([toggle_attr/2]).
-export([toggle_attr/4]).
-export([dispatch_event/2]).
-export([navigate/1]).
-export([navigate/2]).
-export([patch/1]).
-export([patch/2]).
-export([fetch/2]).
-export([focus/1]).
-export([blur/1]).
-export([reset_form/1]).
-export([select/1]).
-export([copy_to_clipboard/1]).
-export([show_modal/1]).
-export([close_modal/1]).
-export([scroll_to/1]).
-export([scroll_to/2]).
-export([set_title/1]).
-export([reload/0]).
-export([on_key/2]).
-export([set/2]).
-export([set/3]).
-export([set_all/2]).
-export([request_pip/1]).
-export([request_pip/2]).
-export([exit_pip/1]).
-export([transition/1]).
-export([transition/2]).

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
    toggle_attr/2,
    toggle_attr/4,
    dispatch_event/2,
    navigate/1,
    navigate/2,
    patch/1,
    patch/2,
    fetch/2,
    focus/1,
    blur/1,
    reset_form/1,
    select/1,
    copy_to_clipboard/1,
    show_modal/1,
    close_modal/1,
    scroll_to/1,
    scroll_to/2,
    set_title/1,
    reload/0,
    on_key/2,
    set/2,
    set/3,
    set_all/2,
    request_pip/1,
    request_pip/2,
    exit_pip/1,
    transition/1,
    transition/2
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
Pushes a named event to the server. The payload is auto-collected from the
**triggering element** itself, not its siblings: a form trigger contributes its
fields (a form submit passes the form), an input/select/textarea trigger its
`value`; any other trigger (e.g. a plain button inside a form) sends an empty
payload. Use `push_event/2` to attach data explicitly.
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

-doc "Toggles the visibility of all elements matching the CSS selector.".
-spec toggle(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
toggle(Sel) -> {arizona_effect, [?EFFECT_TOGGLE, Sel]}.

-doc "Shows all elements matching the CSS selector.".
-spec show(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
show(Sel) -> {arizona_effect, [?EFFECT_SHOW, Sel]}.

-doc "Hides all elements matching the CSS selector.".
-spec hide(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
hide(Sel) -> {arizona_effect, [?EFFECT_HIDE, Sel]}.

-doc "Adds a CSS class to all elements matching the selector.".
-spec add_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
add_class(Sel, Cls) -> {arizona_effect, [?EFFECT_ADD_CLASS, Sel, Cls]}.

-doc "Removes a CSS class from all elements matching the selector.".
-spec remove_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
remove_class(Sel, Cls) -> {arizona_effect, [?EFFECT_REMOVE_CLASS, Sel, Cls]}.

-doc "Toggles a CSS class on all elements matching the selector.".
-spec toggle_class(Selector, Class) -> arizona_effect:cmd() when
    Selector :: binary(),
    Class :: binary().
toggle_class(Sel, Cls) -> {arizona_effect, [?EFFECT_TOGGLE_CLASS, Sel, Cls]}.

-doc "Sets an attribute on all elements matching the selector.".
-spec set_attr(Selector, Attr, Value) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary(),
    Value :: binary().
set_attr(Sel, Attr, Val) -> {arizona_effect, [?EFFECT_SET_ATTR, Sel, Attr, Val]}.

-doc "Removes an attribute from all elements matching the selector.".
-spec remove_attr(Selector, Attr) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary().
remove_attr(Sel, Attr) -> {arizona_effect, [?EFFECT_REMOVE_ATTR, Sel, Attr]}.

-doc """
Toggles the presence of a bare boolean attribute on all elements matching the
selector: removes it if present, otherwise sets it (e.g. `disabled`, `readonly`,
`hidden`, `open`). For toggling between two values, use `toggle_attr/4`.
""".
-spec toggle_attr(Selector, Attr) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary().
toggle_attr(Sel, Attr) -> {arizona_effect, [?EFFECT_TOGGLE_ATTR, Sel, Attr]}.

-doc """
Toggles an attribute between two values on all elements matching the selector: sets
`ValueB` when the current value is `ValueA`, otherwise sets `ValueA` (so any
other current value resolves to `ValueA`). Useful for one-button toggles like a
password field's `type` between `password` and `text`.
""".
-spec toggle_attr(Selector, Attr, ValueA, ValueB) -> arizona_effect:cmd() when
    Selector :: binary(),
    Attr :: binary(),
    ValueA :: binary(),
    ValueB :: binary().
toggle_attr(Sel, Attr, A, B) -> {arizona_effect, [?EFFECT_TOGGLE_ATTR, Sel, Attr, A, B]}.

-doc "Dispatches a custom DOM event with a payload.".
-spec dispatch_event(Name, Payload) -> arizona_effect:cmd() when
    Name :: binary(),
    Payload :: map().
dispatch_event(Name, Payload) -> {arizona_effect, [?EFFECT_DISPATCH_EVENT, Name, Payload]}.

-doc "Triggers a SPA navigation to `Path`.".
-spec navigate(Path) -> arizona_effect:cmd() when
    Path :: binary().
navigate(Path) -> {arizona_effect, [?EFFECT_NAVIGATE, Path]}.

-doc """
Triggers a SPA navigation to `Path` with options.

- `replace` -- use `replaceState` instead of `pushState`.
- `flash` -- a flash map (`#{binary() => term()}`) to show on the page navigated
  to, the live counterpart of `arizona_req:put_flash/3` before a redirect. The
  server strips it from the effect (the browser never sees it) and carries it
  in-process to the target's `flash` binding, so an in-view Post/Redirect/Get
  (`submit -> flash -> navigate`) shows its message just like an HTTP redirect flash.
- `full` -- do a real full-page navigation (`location.assign`) instead of a SPA
  navigation. For a target that is not a live route (a controller/asset path, a
  404); the framework itself emits this to degrade a navigate/patch whose path
  does not resolve to a live view.
""".
-spec navigate(Path, Opts) -> arizona_effect:cmd() when
    Path :: binary(),
    Opts :: map().
navigate(Path, Opts) -> {arizona_effect, [?EFFECT_NAVIGATE, Path, Opts]}.

-doc """
Triggers an in-place SPA navigation (`patch`) to `Path`: keeps the current
live view mounted and re-renders it via `handle_update/3` instead of
replacing it. The counterpart of `navigate/1`; use it between routes that
share a root view so live chrome survives the navigation.
""".
-spec patch(Path) -> arizona_effect:cmd() when
    Path :: binary().
patch(Path) -> {arizona_effect, [?EFFECT_PATCH, Path]}.

-doc """
Triggers an in-place SPA navigation (`patch`) to `Path` with options. Accepts the
same `flash` opt as `navigate/2` (carried in-process to the patched route's `flash`
binding, stripped from the client effect).
""".
-spec patch(Path, Opts) -> arizona_effect:cmd() when
    Path :: binary(),
    Opts :: map().
patch(Path, Opts) -> {arizona_effect, [?EFFECT_PATCH, Path, Opts]}.

-doc """
Issues an HTTP request to `Url` via the browser `fetch()` API, **without** a page
reload, and applies the response. Unlike `push_event` (WebSocket, can't set
cookies) and a plain form POST (full reload), a `fetch` response can carry a real
`Set-Cookie` -- HttpOnly honored, applied natively by the browser -- while the
page stays put. Use it for flows that must rotate a session cookie yet keep the
typed form fields and show inline validation (password change, login, logout).

Composes as a command: as an `az_submit` command the trigger form's fields are the
request body; as an `az_click` command on a non-form element, `body` is sent
instead.

The endpoint is a controller route (e.g. `{post, ...}`) returning the `{"e": [...]}`
effects wire payload -- `arizona_controller:reply_effects/1` for the success leg
(200; a form with `az-form-reset` clears) or `reply_effects/2` with a non-2xx
(e.g. `422`) for a validation error (the typed fields survive). The effects are
applied on **any** status, against the submitting view's element. `on_error` (and
an `arizona:fetch-error` DOM event) runs only when there is no usable effects body
-- a non-JSON page, an empty non-2xx, or a network failure.

To re-render the live view, return an `arizona_js:push_event` in the response: the
client relays it over the existing WebSocket and the view re-renders through its
normal `handle_event/3` (no subscription; it targets the submitting view and does
not echo the form fields). Use `arizona_pubsub` from the controller instead to reach
*other* views (broadcast). To send the user elsewhere, return `arizona_js:navigate/1`
(e.g. via `reply_redirect/1`), not an HTTP 3xx -- a fetch-followed redirect can't
drive a SPA navigation.

**Identity changes need a reload.** The WebSocket is not re-handshaked when the
cookie changes, so a fetch that changes *who* the user is (login/logout) must
`arizona_js:reload/0` (or do a real navigation) for the socket to pick up the new
session -- `push_event`/`navigate` would re-render over the stale one. Rotating the
cookie for the *same* identity (e.g. password change) needs no reload.

`Opts`:

- `method` -- HTTP method. Default: the trigger form's `method`, else `post`.
- `body` -- request body for a non-form trigger (JSON-encoded). Default: the
  trigger form's fields as `application/x-www-form-urlencoded` (a GET carries them
  in the query string instead).
- `headers` -- extra request headers, merged on top.
- `credentials` -- cookie/credential mode. Default `same_origin`.
- `on_error` -- a command (or list) run on a non-2xx with no effects body, or a
  network error.
- `keep_alive` -- when `true`, the request completes even if a navigation starts
  right after it (use case: a POST fired just before navigating away); maps to the
  browser's `fetch(url, { keepalive: true })`. Absent or `false` is the current
  behaviour (the navigation cancels an in-flight fetch). The browser caps a
  keepalive request's inflight body at ~64KB.

To send the user elsewhere, return an `arizona_js:navigate/1` effect from the
controller (e.g. via `arizona_controller:reply_redirect/1`) rather than an HTTP
3xx -- a fetch-followed redirect can't drive a SPA navigation.
""".
-spec fetch(Url, Opts) -> arizona_effect:cmd() when
    Url :: binary(),
    Opts :: #{
        method => get | post | put | patch | delete,
        body => term(),
        headers => #{binary() => binary()},
        credentials => same_origin | include | omit,
        on_error => arizona_effect:cmd() | [arizona_effect:cmd()],
        keep_alive => boolean()
    }.
fetch(Url, Opts) -> {arizona_effect, [?EFFECT_FETCH, Url, unwrap_on_error(Opts)]}.

-doc "Focuses the first element matching the selector.".
-spec focus(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
focus(Sel) -> {arizona_effect, [?EFFECT_FOCUS, Sel]}.

-doc "Blurs (removes focus from) the first element matching the selector.".
-spec blur(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
blur(Sel) -> {arizona_effect, [?EFFECT_BLUR, Sel]}.

-doc """
Resets **all** forms matching the selector, clearing the fields the user typed
back to their initial values (`HTMLFormElement.reset`).

Unlike `focus/1`/`blur/1` (first match only), this broadcasts to every match,
like the DOM-mutating selector effects (`toggle`, the class ops, `set_attr`). A
match without a `reset()` method (a non-form element) is a safe no-op.

Where `set_attr(value, ~"")` only rewrites the `value` **attribute** (leaving the
user-typed value **property** untouched, and doing nothing at all for a
`<textarea>` that has no value attribute), this drives the native form reset, so
it clears text inputs and textareas alike. Unlike `az-form-reset` (which fires
only on a successful submit/fetch), it's imperative: usable both as a web event
command (e.g. `{az_click, arizona_js:reset_form(~"#signup")}`) and as a handler
effect from `handle_event/3`.
""".
-spec reset_form(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
reset_form(Sel) -> {arizona_effect, [?EFFECT_RESET_FORM, Sel]}.

-doc """
Selects (highlights) the text of the first element matching the selector, an
`<input>` or `<textarea>`, via `HTMLInputElement.select` -- so a subsequent copy
or overtype acts on the whole field.

Like `focus/1`/`blur/1`/`scroll_to/1` (first match only). A match without a
`select()` method is a safe no-op. Usable both as a web event command
(e.g. `{az_click, arizona_js:select(~"#token")}`) and as a handler effect from
`handle_event/3`.
""".
-spec select(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
select(Sel) -> {arizona_effect, [?EFFECT_SELECT, Sel]}.

-doc """
Copies to the clipboard the text of the first element matching the selector: the
matched element's `value` (a form control) or, failing that, its `textContent`,
written via `navigator.clipboard.writeText`.

Like `focus/1`/`blur/1`/`scroll_to/1` (first match only). The Clipboard API
requires a **secure context** and a **user gesture**, so this is meaningful as an
**event command only** (e.g. `{az_click, arizona_js:copy_to_clipboard(~"#token")}`);
a server-pushed handler effect can't satisfy the gesture requirement. A
missing/blocked clipboard is a safe no-op.
""".
-spec copy_to_clipboard(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
copy_to_clipboard(Sel) -> {arizona_effect, [?EFFECT_COPY_TO_CLIPBOARD, Sel]}.

-doc """
Opens the first `<dialog>` matching the selector as a true modal via
`HTMLDialogElement.showModal` -- placed in the top layer, with a `::backdrop`
and ESC-to-close -- unlike merely setting the `open` attribute (a non-modal
dialog). A non-dialog match is a safe no-op.

Like `focus/1`/`blur/1`/`scroll_to/1` (first match only). Usable both as a web
event command (e.g. `{az_click, arizona_js:show_modal(~"#confirm")}`) and as a
handler effect from `handle_event/3`.
""".
-spec show_modal(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
show_modal(Sel) -> {arizona_effect, [?EFFECT_SHOW_MODAL, Sel]}.

-doc """
Closes the first `<dialog>` matching the selector via `HTMLDialogElement.close`.
A non-dialog match is a safe no-op.

Like `focus/1`/`blur/1`/`scroll_to/1` (first match only). Usable both as a web
event command (e.g. `{az_click, arizona_js:close_modal(~"#confirm")}`) and as a
handler effect from `handle_event/3`.
""".
-spec close_modal(Selector) -> arizona_effect:cmd() when
    Selector :: binary().
close_modal(Sel) -> {arizona_effect, [?EFFECT_CLOSE_MODAL, Sel]}.

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

Matching is **case-insensitive**: the client lowercases the browser's
`event.key` before comparing, so give keys and patterns in lowercase
(`enter`, `escape`, `arrowdown`). An uppercase name or pattern (`'Enter'`,
`~"^[A-Z]$"`) never matches.
""".
-spec on_key(Key, Cmd) -> arizona_effect:cmd() when
    Key :: atom() | [atom()] | binary(),
    Cmd :: arizona_effect:cmd() | [arizona_effect:cmd()].
on_key(Key, Cmds) ->
    {arizona_effect, [?EFFECT_ON_KEY, encode_key(Key), unwrap_cmds(Cmds)]}.

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
a server-pushed handler effect. The window opens at the browser's default size;
use `request_pip/2` to choose a size.
""".
-spec request_pip(ViewId) -> arizona_effect:cmd() when
    ViewId :: binary().
request_pip(ViewId) -> {arizona_effect, [?EFFECT_REQUEST_PIP, ViewId]}.

-doc """
Like `request_pip/1` but with window options. `width`/`height` set the floating
window's initial size in pixels; when omitted the browser picks a default size.
""".
-spec request_pip(ViewId, Opts) -> arizona_effect:cmd() when
    ViewId :: binary(),
    Opts :: #{width => pos_integer(), height => pos_integer()}.
request_pip(ViewId, Opts) -> {arizona_effect, [?EFFECT_REQUEST_PIP, ViewId, Opts]}.

-doc """
Closes the floating Picture-in-Picture window for `ViewId`, moving the view back
inline. No-op if the view isn't popped out.
""".
-spec exit_pip(ViewId) -> arizona_effect:cmd() when
    ViewId :: binary().
exit_pip(ViewId) -> {arizona_effect, [?EFFECT_EXIT_PIP, ViewId]}.

-doc """
Wraps a command (or list of commands) so the DOM change it causes plays inside a
view transition (`document.startViewTransition`). Works for a client-side effect
(e.g. `toggle/1`, animated immediately), a `navigate/1,2` (the page swap
animates), or a `push_event` (the resulting server diff animates). A view
transition is not tied to navigation -- it wraps any DOM change. Equivalent to
`transition/2` with no options.
""".
-spec transition(Cmd) -> arizona_effect:cmd() when
    Cmd :: arizona_effect:cmd() | [arizona_effect:cmd()].
transition(Cmd) -> transition(Cmd, #{}).

-doc """
Like `transition/1` but with options. `types` is a list of view-transition type
names activated for the transition (matched by `:active-view-transition-type(...)`
CSS), letting one stylesheet pick a different animation per call.
""".
-spec transition(Cmd, Opts) -> arizona_effect:cmd() when
    Cmd :: arizona_effect:cmd() | [arizona_effect:cmd()],
    Opts :: #{types => [binary()]}.
transition(Cmds, Opts) ->
    {arizona_effect, [?EFFECT_TRANSITION, Opts, unwrap_cmds(Cmds)]}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

encode_key(Key) when is_atom(Key) -> [Key];
encode_key(Keys) when is_list(Keys) -> Keys;
encode_key(Pattern) when is_binary(Pattern) -> Pattern.

%% Unwrap nested cmd tuple(s) to bare op-array(s) so the result is JSON-encodable.
%% A single `{arizona_effect, Inner}` yields its bare `Inner`; a list of cmd tuples
%% yields a list of bare op-arrays. Shared by on_key/2, transition/2, and
%% unwrap_on_error/1.
unwrap_cmds({arizona_effect, Inner}) -> Inner;
unwrap_cmds([_ | _] = Cmds) -> [C || {arizona_effect, C} <:- Cmds].

%% fetch/2's on_error carries cmd tuple(s) inside the Opts map; arizona_effect:encode/1
%% does not recurse into map values, so unwrap them here before embedding Opts.
unwrap_on_error(#{on_error := Cmds} = Opts) -> Opts#{on_error => unwrap_cmds(Cmds)};
unwrap_on_error(Opts) -> Opts.

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
    {arizona_effect, [?EFFECT_REQUEST_PIP, ~"v"]} = request_pip(~"v"),
    {arizona_effect, [?EFFECT_REQUEST_PIP, ~"v", #{width := 440, height := 940}]} =
        request_pip(~"v", #{width => 440, height => 940}).

exit_pip_test() ->
    {arizona_effect, [?EFFECT_EXIT_PIP, ~"v"]} = exit_pip(~"v").

transition_test() ->
    {arizona_effect, [?EFFECT_TRANSITION, #{}, [?EFFECT_TOGGLE, ~"#m"]]} =
        transition(toggle(~"#m")).

transition_with_opts_test() ->
    {arizona_effect, [?EFFECT_TRANSITION, #{types := [~"slide"]}, [?EFFECT_NAVIGATE, ~"/p"]]} =
        transition(navigate(~"/p"), #{types => [~"slide"]}).

transition_list_test() ->
    {arizona_effect, [
        ?EFFECT_TRANSITION, #{}, [[?EFFECT_ADD_CLASS, ~"#m", ~"on"], [?EFFECT_TOGGLE, ~"#n"]]
    ]} =
        transition([add_class(~"#m", ~"on"), toggle(~"#n")]).

fetch_test() ->
    {arizona_effect, [?EFFECT_FETCH, ~"/x", #{method := post}]} =
        fetch(~"/x", #{method => post}).

fetch_keep_alive_test() ->
    %% keep_alive is a plain boolean -- it rides through the Opts map untouched.
    {arizona_effect, [?EFFECT_FETCH, ~"/x", #{keep_alive := true}]} =
        fetch(~"/x", #{keep_alive => true}).

fetch_encode_test() ->
    %% The Opts map rides through json:encode/1 like navigate/2's opts -- no
    %% special encoding in arizona_effect.
    Bin = arizona_effect:encode(fetch(~"/account", #{method => post})),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"[22")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"/account")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"method")).

reset_form_test() ->
    {arizona_effect, [?EFFECT_RESET_FORM, ~"#signup"]} = reset_form(~"#signup").

reset_form_encode_test() ->
    %% Round-trips through arizona_effect:encode/1 like any other selector effect.
    Bin = arizona_effect:encode(reset_form(~"#signup")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"[25")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"#signup")).

select_test() ->
    {arizona_effect, [?EFFECT_SELECT, ~"#token"]} = select(~"#token").

copy_to_clipboard_test() ->
    {arizona_effect, [?EFFECT_COPY_TO_CLIPBOARD, ~"#token"]} = copy_to_clipboard(~"#token").

copy_to_clipboard_encode_test() ->
    %% Round-trips through arizona_effect:encode/1 like any other selector effect.
    Bin = arizona_effect:encode(copy_to_clipboard(~"#token")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"[27")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"#token")).

show_modal_test() ->
    {arizona_effect, [?EFFECT_SHOW_MODAL, ~"#confirm"]} = show_modal(~"#confirm").

close_modal_test() ->
    {arizona_effect, [?EFFECT_CLOSE_MODAL, ~"#confirm"]} = close_modal(~"#confirm").

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
    ?assertEqual(
        {arizona_effect, [?EFFECT_TOGGLE_ATTR, ~"#m", ~"disabled"]}, toggle_attr(~"#m", ~"disabled")
    ),
    ?assertEqual(
        {arizona_effect, [?EFFECT_TOGGLE_ATTR, ~"#m", ~"type", ~"password", ~"text"]},
        toggle_attr(~"#m", ~"type", ~"password", ~"text")
    ),
    ?assertEqual({arizona_effect, [?EFFECT_NAVIGATE, ~"/p"]}, navigate(~"/p")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_NAVIGATE, ~"/p", #{replace => true}]},
        navigate(~"/p", #{replace => true})
    ),
    ?assertEqual({arizona_effect, [?EFFECT_PATCH, ~"/p"]}, patch(~"/p")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_PATCH, ~"/p", #{replace => true}]},
        patch(~"/p", #{replace => true})
    ),
    ?assertEqual({arizona_effect, [?EFFECT_FOCUS, ~"#m"]}, focus(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_BLUR, ~"#m"]}, blur(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_RESET_FORM, ~"#m"]}, reset_form(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_SELECT, ~"#m"]}, select(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_COPY_TO_CLIPBOARD, ~"#m"]}, copy_to_clipboard(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_SHOW_MODAL, ~"#m"]}, show_modal(~"#m")),
    ?assertEqual({arizona_effect, [?EFFECT_CLOSE_MODAL, ~"#m"]}, close_modal(~"#m")),
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
