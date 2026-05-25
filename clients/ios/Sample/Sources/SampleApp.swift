import SwiftUI

/// Multi-example launcher for the Arizona native iOS client. Opens the
/// server-driven `/native/menu` view (override with the `-az-path <path>` launch
/// argument); its buttons navigate to each example on the same socket. The app
/// supplies the widget vocabulary; the AzClientUI library supplies transport,
/// diff, dispatch, and navigation. The Android counterpart is MainActivity.
@main
struct SampleApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}
