// Reference Tauri (v2) shell for the Arizona native-shell (OS) capability seam.
//
// This is one implementor of the shell-neutral `globalThis.__arizona_os__`
// contract -- the SAME contract an Electron preload implements. It changes
// nothing in Arizona: the window loads the remote Arizona app over the existing
// web path, and the shell only provides the native capabilities the browser
// sandbox forbids. See clients/tauri/README.md and docs/os.md.

use tauri::webview::WebviewWindowBuilder;
use tauri::{Emitter, WebviewUrl, WindowEvent};

// Capabilities this shell offers. Advertised to the page (via the init script
// below -> `_az_caps` -> `?capability`) and enforced by `arizona_invoke`'s
// allowlist match. Documents the contract and is checked against the script in
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
      return window.__TAURI__.core.invoke('arizona_invoke', { name: name, args: args });
    },
    onEvent: function (cb) {
      window.__TAURI__.event.listen('arizona-event', function (e) {
        cb(e.payload.name, e.payload.payload);
      });
    }
  };
})();
"#;

// Renderer -> shell: run an allowlisted native command. `args` is the spliced
// arg list the server sent after the capability name (see `arizona_os`). Unknown
// names are ignored (defense in depth -- the page can reach nothing else).
#[tauri::command]
fn arizona_invoke(
    window: tauri::WebviewWindow,
    name: String,
    args: Vec<serde_json::Value>,
) -> Result<(), String> {
    let bool_arg = |i: usize| args.get(i).and_then(serde_json::Value::as_bool).unwrap_or(false);
    let str_arg = |i: usize| args.get(i).and_then(serde_json::Value::as_str).unwrap_or("");
    let result = match name.as_str() {
        "window_title" => window.set_title(str_arg(0)),
        "window_focus" => window.set_focus(),
        "window_minimize" => window.minimize(),
        "window_maximize" => window.maximize(),
        "window_fullscreen" => window.set_fullscreen(bool_arg(0)),
        // Windows 10 2004+/macOS only; a harmless no-op on Linux.
        "screen_capture_protection" => window.set_content_protected(bool_arg(0)),
        _ => Ok(()),
    };
    result.map_err(|e| e.to_string())
}

fn main() {
    // The Arizona app this shell wraps. Defaults to the in-repo OS demo; override
    // with ARIZONA_URL to point at your deployment.
    let url =
        std::env::var("ARIZONA_URL").unwrap_or_else(|_| "http://localhost:4040/os".to_string());

    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![arizona_invoke])
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
}
