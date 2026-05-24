# Arizona Android client (`clients/android`)

The **Android client** for Arizona's `?native` (JSON) render target — the native
counterpart of the browser client in `assets/js/`, kept in this repo so it
evolves atomically with the wire protocol. Built separately from the Erlang/JS
toolchain (its own Gradle build; not part of `make ci`).

Architecturally this is **React Native's *renderer* layer, fed by the server
over a WebSocket** instead of by on-device JS: the server holds all state and
logic and ships a cached-statics + diff stream; this thin client reconstructs a
**Jetpack Compose** widget tree and relays events back. No app logic runs on the
device, and nothing is compiled to native ahead of time — the UI is real Compose
widgets built at runtime from the server's JSON.

It mirrors the proven JS reference client (`e2e/utils/native_client.js` in the
Arizona repo): connect → cache statics by fingerprint → interleave statics+dynamics
(JSON-encoding each value) → flatten `#slot` fragments → apply ops against an
`az → node` registry → send events.

## Modules

- **`:arizona`** — the client library:
  - `Wire.kt` — op codes + `FingerprintCache` + `Interleaver` (pure, JVM-testable).
  - `Tree.kt` — the reactive `Node` model (Compose snapshot state) + parse + `az` index.
  - `AzClient.kt` — OkHttp WebSocket, op application, `pushEvent`.
  - `Renderer.kt` — `WidgetRegistry` + `RenderContext` + `ArizonaView` (vocabulary-agnostic
    Compose dispatch).
- **`:sample`** — a demo app: registers a `Column`/`Row`/`Text`/`Button` vocabulary and
  renders the server's `/native/counter` view.

## Status & caveats

Authored against a known-good late-2024/early-2025 toolchain (see `gradle/libs.versions.toml`);
**align versions with your installed Android SDK/AGP/Kotlin**. The package names
(`dev.arizona.*`) are placeholders — rename freely. No Gradle wrapper is committed:
build via Android Studio (it bundles its own Gradle) or a system `gradle` install —
the CLI commands below assume `gradle` on `PATH`. CI provisions Gradle with
`gradle/actions/setup-gradle`.

## Verify

**Pure logic (no emulator)** — validates the interleaver against the real captured frame:

```bash
gradle :arizona:testDebugUnitTest
```

**Run the demo on an emulator or device** — start the server, tunnel the device's
`localhost:4040` to it with `adb reverse` (works for the emulator and a real
device alike, over USB; no LAN IP), then launch the app:

```bash
# From the repo root (server binds 0.0.0.0:4040):
PORT=4040 ERLANG_EXTRA_ARGS=-noshell ./scripts/start_test_server.sh

# With a running emulator/device (re-run after replugging the cable):
adb reverse tcp:4040 tcp:4040

# From clients/android (or just hit Run in Studio):
gradle :sample:installDebug   # then open the app
```

The app opens a **menu**; tap a button to navigate to each example
(counter, list, tabs, ticker, multi), and **☰ Menu** to go back. The app connects
to `http://localhost:4040` (the `adb reverse` tunnel).

**On-device e2e** (the Playwright analogue) — with the server running:

```bash
adb reverse tcp:4040 tcp:4040 && gradle :sample:connectedCheck
```

CI that wires the server + emulator together lives in
`.github/workflows/android-e2e.yml`.
