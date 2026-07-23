// Reference Tauri (v2) shell for the Arizona native-shell (OS) capability seam.
//
// This is one implementor of the shell-neutral `globalThis.__arizona_os__`
// contract -- the SAME contract an Electron preload implements. It changes
// nothing in Arizona: the window loads the remote Arizona app over the existing
// web path, and the shell only provides the native capabilities the browser
// sandbox forbids. See clients/tauri/README.md and docs/os.md.

// Hide the console window on a release Windows build (the app is a GUI, not a
// console program). No-op on other platforms and in debug builds.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::webview::WebviewWindowBuilder;
use tauri::{Emitter, WebviewUrl, WindowEvent};

// Capabilities this shell offers. Advertised to the page (via the init script
// below -> `_az_caps` -> `?capability`) and dispatched by the init script's
// `invoke` switch. Documents the contract and is checked against the script in
// the test below.
#[allow(dead_code)]
const CAPABILITIES: &[&str] = &[
    "window_title",
    "window_focus",
    "window_minimize",
    "window_maximize",
    "window_fullscreen",
    "screen_capture_protection",
];

// Injected into the remote page BEFORE its own scripts run -- the Tauri
// equivalent of an Electron preload, satisfying the contract's "shell present
// before connect()" ordering requirement. It defines the contract object using
// the global Tauri API (`app.withGlobalTauri`), since the remote page bundles no
// Tauri code. The methods reference `window.__TAURI__` lazily (called at connect
// / click time), so it need not exist when this runs.
const INIT_SCRIPT: &str = r#"
(function () {
  if (globalThis.__arizona_os__) return;
  globalThis.__arizona_os__ = {
    capabilities: {
      window_title: true,
      window_focus: true,
      window_minimize: true,
      window_maximize: true,
      window_fullscreen: true,
      screen_capture_protection: true
    },
    invoke: function (name, args) {
      // Drive the window via Tauri's CORE window commands (permission-gated, so
      // they work from a remote page). A custom app command is "not allowed"
      // for remote content ("Plugin not found").
      var w = window.__TAURI__.window.getCurrentWindow();
      switch (name) {
        case 'window_title': return w.setTitle(args[0]);
        case 'window_focus': return w.setFocus();
        case 'window_minimize': return w.minimize();
        // Maximize, never toggle: the seam documents this capability as
        // "maximizes", and a server may re-assert it, so it must be idempotent.
        case 'window_maximize': return w.maximize();
        case 'window_fullscreen': return w.setFullscreen(!!args[0]);
        case 'screen_capture_protection': return w.setContentProtected(!!args[0]);
        default: return Promise.resolve();
      }
    },
    onEvent: function (cb) {
      window.__TAURI__.event.listen('arizona-event', function (e) {
        cb(e.payload.name, e.payload.payload);
      });
    }
  };
})();
"#;

fn main() {
    // The Arizona app this shell wraps. Defaults to the in-repo OS demo; override
    // with ARIZONA_URL to point at your deployment.
    let url =
        std::env::var("ARIZONA_URL").unwrap_or_else(|_| "http://localhost:4040/os".to_string());

    tauri::Builder::default()
        .setup(move |app| {
            let parsed: tauri::Url = url.parse().expect("invalid ARIZONA_URL");
            let handle = app.handle().clone();
            let window = WebviewWindowBuilder::new(app, "main", WebviewUrl::External(parsed))
                .title("Arizona (Tauri)")
                .inner_size(1024.0, 768.0)
                .initialization_script(INIT_SCRIPT)
                .build()?;
            // Forward window OS events to the page; the init-script listener
            // relays them into the view's handle_event/3.
            window.on_window_event(move |event| {
                if let WindowEvent::Focused(focused) = event {
                    let state = if *focused { "focused" } else { "blurred" };
                    let _ = handle.emit(
                        "arizona-event",
                        serde_json::json!({
                            "name": "window_state",
                            "payload": { "state": state }
                        }),
                    );
                }
            });
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running the Tauri application");
}

#[cfg(test)]
mod tests {
    use super::{CAPABILITIES, INIT_SCRIPT};

    #[test]
    fn advertised_capabilities_appear_in_the_page_contract() {
        for cap in CAPABILITIES {
            assert!(
                INIT_SCRIPT.contains(cap),
                "capability `{cap}` is advertised but missing from the page contract script",
            );
        }
    }

    #[test]
    fn window_maximize_is_idempotent_not_a_toggle() {
        // `arizona_os:maximize/0` maximizes the window; a shell that toggles would
        // restore an already-maximized window when the server re-asserts it.
        assert!(
            !INIT_SCRIPT.contains("toggleMaximize"),
            "`window_maximize` must maximize, not toggle",
        );
    }
}
