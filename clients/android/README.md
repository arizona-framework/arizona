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
(`dev.arizona.*`) are placeholders — rename freely. The Gradle wrapper jar isn't
committed: open the project in Android Studio (it provisions Gradle), or run
`gradle wrapper` once to generate `./gradlew`.

## Verify

**Pure logic (no emulator)** — validates the interleaver against the real captured frame:

```bash
./gradlew :arizona:testDebugUnitTest
```

**Run the demo on an emulator** — start the Arizona server from the repo root,
then launch the app:

```bash
# From the repo root (server on the host; emulator reaches it at 10.0.2.2):
PORT=4040 ERLANG_EXTRA_ARGS=-noshell ./scripts/start_test_server.sh

# From clients/android, on a running emulator/device:
./gradlew :sample:installDebug   # then open the app
```

The counter renders `Count: 0`; tapping **+**/**−** round-trips through the server.

**On-device e2e** (the Playwright analogue) — the Arizona server must be running
and reachable at `10.0.2.2:4040`:

```bash
./gradlew :sample:connectedCheck
```

CI that wires the server + emulator together lives in
`.github/workflows/android-e2e.yml`.
