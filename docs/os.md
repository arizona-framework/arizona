# Native-shell (OS) capabilities

When an Arizona app runs inside a **native shell** (Tauri, Electron, a webview
wrapper, ...) instead of a plain browser, the server can drive OS-level
capabilities the browser sandbox forbids: window control, native notifications,
screen-capture protection, and so on. The renderer is still the **same** web
(`?html`) client over the **same** WebSocket -- the shell only adds a native
side-channel. So an existing app runs in a shell **unchanged**, and the seam is
**purely additive**: in a plain browser everything below is a safe no-op.

This is **not** a new render target (that is [`docs/native.md`](native.md), the
`?native` JSON-widget target for iOS/Android). A desktop shell's renderer is a
browser, so it uses the web path; this page is only about the native capability
side-channel.

## The contract -- `globalThis.__arizona_os__`

The seam is defined entirely at one boundary: a JS object the shell installs on
the page **before the page's scripts run**. Arizona has zero shell-specific code;
supporting a new shell means writing an adapter that provides this object.

```js
globalThis.__arizona_os__ = {
  // Advertised at connect -> _az_caps -> the server's ?capability(Name).
  capabilities: { window_title: true, window_focus: true /* ... */ },

  // Server/client OS command -> the shell's native layer (allowlisted there).
  // Returns a Promise. The arguments are the capability name and the spliced
  // arg list the server sent after it.
  invoke(name, args) { /* ... */ },

  // Register ONE callback for inbound OS events (window focus/blur, ...). The
  // shell calls cb(name, payload); Arizona relays it as an ordinary event.
  onEvent(cb) { /* ... */ },
};
```

Two requirements on **any** adapter:

- **Ordering.** The shell MUST set `globalThis.__arizona_os__` **before** Arizona's
  `connect()` runs, so the capabilities are advertised at the WebSocket handshake.
  (If no shell is present by then, the app runs as a plain browser -- a safe
  no-op, never an error.)
- **Allowlist in native code.** Because the renderer loads remote app content,
  `invoke` must dispatch only known capability names to typed native actions --
  never a generic native call.

## Capability negotiation -- `_az_caps` and `?capability`

At connect, the client serializes `__arizona_os__.capabilities` into the
`_az_caps` WebSocket query param. The live process exposes it through
`arizona_live:capability/1`, with the `?capability(Name)` macro:

```erlang
?capability(~"window_title")   %% boolean(): did the shell advertise it?
?capabilities                   %% the whole map
```

`?capability` mirrors `?connected`: it is `false` during SSR and reflects the
negotiated capabilities in the connected live process. So gate **rendering** the
same way you gate on `?connected` -- via a binding flipped on connect, **not**
raw `?capability` in `render/1` (which would freeze at SSR's `false`):

```erlang
mount(Bindings0) ->
    Bindings = #{id => ~"win", can_window => false /* ... */},
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

handle_info(arizona_connected, Bindings) ->
    {Bindings#{can_window => ?capability(~"window_title")}, #{}, []}.

render(Bindings) ->
    ?html({'div', [], [
        case ?get(can_window) of
            true -> {button, [{az_click, arizona_os:set_title(~"Hi")}], [~"Rename"]};
            false -> <<>>
        end
    ]}).
```

> **Security invariant.** `?capability` reflects an **unauthenticated, client-
> advertised** claim (any client can put anything in `_az_caps`). It is a UI/effect
> hint only -- **never** branch a server-side authorization decision on it. It
> exists so a view can skip rendering a control whose effect would no-op anyway.
> Advertising a capability you do not have is self-defeating: a non-cooperating
> client will not execute the command.

## Issuing OS commands -- `arizona_os`

`arizona_os` is the per-target command builder (beside `arizona_js` for the
browser and `arizona_android` for `?native`). Unlike those, **every** command
funnels through one generic op (`?EFFECT_OS`) carrying a capability **name** plus
args -- the engine is a pass-through; the shell owns the vocabulary. New
capabilities are new names, never new op codes.

```erlang
%% Client-triggered (an event attribute): runs in the browser on click, no
%% round-trip, straight to the shell.
{button, [{az_click, arizona_os:set_title(~"New title")}], [~"Rename"]}

%% Server-emitted (a handler effect): pushed over the WebSocket, then to the shell.
handle_event(~"rename", _P, B) ->
    {B, #{}, [arizona_os:set_title(~"New title")]}.
```

Typed sugars are the documented path; `command/2` is the unchecked escape hatch
(a misspelled name is a silent no-op):

| Builder | Capability name |
| --- | --- |
| `set_title/1` | `~"window_title"` |
| `focus/0` | `~"window_focus"` |
| `minimize/0` | `~"window_minimize"` |
| `maximize/0` | `~"window_maximize"` |
| `fullscreen/1` | `~"window_fullscreen"` |
| `notify/1,2` | `~"notify"` |
| `capture_protection/1` | `~"screen_capture_protection"` |
| `command/1,2` | (any name) |

**Declarative vs one-shot on reconnect.** On reconnect the live process re-mounts
fresh while the OS window keeps its real state. Re-assert **declarative/idempotent**
capabilities from server state on the connect self-cast (`set_title`, `fullscreen`,
`screen_capture_protection`). Do **not** re-fire **one-shot** capabilities
(`notify`, `focus`) on reconnect, or you double-fire.

## Inbound OS events

The shell injects OS events (window focus/blur/resize, capture-state change, ...)
through `onEvent`; Arizona relays each as an ordinary `pushEvent` to the root
view, so it lands in `handle_event/3` like any other event -- with its payload:

```erlang
handle_event(~"window_state", #{~"state" := State}, Bindings) ->
    {Bindings#{last_event => State}, #{}, []}.
```

## Shell-neutrality (Tauri, Electron, ...)

The contract is transport-agnostic; the same `__arizona_os__` shape is satisfied
by each shell's native primitives:

| Contract piece | Tauri v2 | Electron |
| --- | --- | --- |
| install `__arizona_os__` before page JS | `WebviewWindowBuilder::initialization_script` | `preload.js` + `contextBridge.exposeInMainWorld` |
| `invoke(name, args)` -> native | `window.__TAURI__.core.invoke('arizona_invoke', ...)` -> `#[tauri::command]` | `ipcRenderer.invoke` -> `ipcMain.handle` |
| `onEvent(cb)` <- OS events | `window.__TAURI__.event.listen` + Rust `app.emit` | `ipcRenderer.on` + main `webContents.send` |
| window control / capture protection | `WebviewWindow::set_title` / `set_content_protected` | `BrowserWindow.setTitle` / `setContentProtection` |

`set_content_protected` / `setContentProtection` is Windows 10 2004+/macOS only
(a no-op on Linux). Arizona's `?capability` is unrelated to Tauri's own
capability/ACL permission system.

The **reference shell** is `clients/tauri/` (Tauri v2): one
worked implementor of this contract, run opt-in via `make build-tauri` /
`make test-tauri` (not part of `make ci`, like the iOS/Android clients). It loads
the in-repo `/os` demo by default (`ARIZONA_URL` overrides). An Electron adapter
implements the same contract with `preload.js` + `ipcMain`.

## Deployment shapes

- **Thin (primary).** The shell's renderer loads a **remote** Arizona URL; the
  BEAM stays on the server. The page is same-origin with its own WebSocket, so the
  Origin check passes untouched. This is what `clients/tauri` does.
- **Fat (deferred).** The OTP release is bundled and run locally; the renderer
  connects to localhost. Adds process-lifecycle/readiness/port concerns; not built
  yet, but the seam does not foreclose it.

## Testing

The seam is proven in CI without any shell by a real-browser e2e
(`e2e/parallel/arizona_os.spec.js`) that installs a fake `window.__arizona_os__`
via `page.addInitScript` (the preload equivalent) and drives the `/os` demo
(`test/support/arizona_os_demo.erl`) through the **real** client: caps negotiated,
server + client OS commands reaching `invoke`, and an inbound OS event reaching
`handle_event/3`. The control case (no shell) asserts the OS UI is absent and the
commands no-op.
