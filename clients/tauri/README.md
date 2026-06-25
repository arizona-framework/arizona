# Arizona Tauri shell

A reference [Tauri](https://v2.tauri.app) (v2) desktop shell that implements the
Arizona **native-shell (OS) capability contract**. It is one implementor of the
shell-neutral `globalThis.__arizona_os__` contract -- the same contract an
Electron preload implements -- so it changes **nothing** in Arizona: the window
loads the remote Arizona app over the existing web path, and the shell only adds
the native capabilities the browser sandbox forbids.

See [`docs/os.md`](../../docs/os.md) for the full contract.

## How it maps to the contract

| Contract piece | This shell (Tauri v2) |
| --- | --- |
| expose `__arizona_os__` before page JS | `WebviewWindowBuilder::initialization_script` (runs before the remote page's scripts) |
| `capabilities` advertised | the object literal in the init script (`-> _az_caps -> ?capability`) |
| `invoke(name, args)` -> native | `window.__TAURI__.core.invoke('arizona_invoke', ...)` -> the `arizona_invoke` `#[tauri::command]` allowlist |
| `onEvent(cb)` <- OS events | `window.__TAURI__.event.listen('arizona-event', ...)`; Rust `app.emit('arizona-event', ...)` on window events |
| window control / capture protection | `WebviewWindow::set_title` / `set_focus` / `minimize` / `maximize` / `set_fullscreen` / `set_content_protected` |

`app.withGlobalTauri` is enabled so the remote page (which bundles no Tauri code)
can reach `window.__TAURI__`. Arizona's `?capability` is unrelated to Tauri's own
capability/ACL permission system; the latter (`src-tauri/capabilities/`) only
grants `core:default` to the window -- the app's own `arizona_invoke` command
needs no permission entry.

## Run it

Prerequisites: the **Rust toolchain** and **Node** -- both pinned in the repo's
root `.tool-versions`, so `mise install` provisions them (mise auto-installs
rustup). mise does **not** manage the platform **webview deps**, so install those
from your OS package manager (on Arch: `webkit2gtk-4.1` + `libsoup` etc.; see the
[Tauri prerequisites](https://v2.tauri.app/start/prerequisites/)):

```bash
mise install                                      # Rust + Node from .tool-versions
sudo pacman -S --needed webkit2gtk-4.1 base-devel curl wget file openssl \
  appmenu-gtk-module libappindicator-gtk3 librsvg # system webview deps (Arch)
```

1. Start the Arizona server with the OS demo route (`/os`) from the repo root:

   ```bash
   make start            # serves http://localhost:4040 (incl. /os)
   ```

2. Launch the shell against it:

   ```bash
   cd clients/tauri
   npm install
   npm run dev           # loads http://localhost:4040/os
   ```

   Point it elsewhere with `ARIZONA_URL=https://your-app.example.com npm run dev`.

You should see: the window-control button appears once connected (capability
negotiated), clicking it renames the window (a client-triggered OS command), the
title is re-asserted on connect (a server-emitted OS command), and focusing /
blurring the window updates the view (an inbound OS event). The same app in a
plain browser simply omits the button -- the commands are safe no-ops.

## Security

The renderer loads remote app content, so the native surface is deliberately
minimal: `arizona_invoke` dispatches only the allowlisted capability names to
typed window methods -- never an arbitrary native call. The Origin check already
gates the WebSocket that carries OS commands, and the command source is the app's
own trusted server.

## Tests

`make test-tauri` (from the repo root) compiles the shell and runs its Rust unit
tests -- no display required. It is **opt-in** and not part of `make ci` (like the
Android/iOS clients). The full UI flow is exercised manually with `npm run dev`
above; an automated UI e2e (WebdriverIO + `tauri-driver`, Linux/Windows only) is a
future addition.

A tiny placeholder `src-tauri/icons/icon.png` is committed so the shell compiles
out of the box. Replace it with your own branding -- `npm run tauri icon
path/to/icon.png` regenerates the full multi-resolution set used by `tauri build`
installers.
