package dev.arizona.sample

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.safeDrawingPadding
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import dev.arizona.client.ArizonaView
import dev.arizona.client.AzClient
import dev.arizona.client.WidgetRegistry

/**
 * Multi-example launcher: opens the server-driven `/native/menu` view, whose
 * buttons navigate to each example on the same socket; the chrome "Menu" button
 * navigates back. The app supplies the widget vocabulary (Column/Row/Text/Button);
 * the library supplies transport, diff, dispatch, and navigation. Launch a
 * specific view directly with the `az_path` intent extra (e.g. for instrumented
 * tests or `adb shell am start ... --es az_path /native/tabs`).
 */
class MainActivity : ComponentActivity() {
    private lateinit var client: AzClient

    private val registry = WidgetRegistry()
        .register("Column") { node -> Column(Modifier.padding(16.dp)) { Children(node) } }
        .register("Row") { node -> Row { Children(node) } }
        .register("Text") { node -> Text(text(node)) }
        .register("Button") { node -> Button(onClick = { tap(node) }) { Text(text(node)) } }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val path = intent.getStringExtra("az_path") ?: "/native/menu"
        client = AzClient(SERVER, path)
        setContent {
            MaterialTheme {
                Surface {
                    // targetSdk 35 forces edge-to-edge; inset below the status bar.
                    Box(Modifier.safeDrawingPadding()) {
                        Column {
                            Button(onClick = { client.navigate("/native/menu") }) { Text("☰ Menu") }
                            ArizonaView(client, registry)
                        }
                    }
                }
            }
        }
        client.connect()
    }

    override fun onDestroy() {
        client.close()
        super.onDestroy()
    }

    companion object {
        // `adb reverse tcp:4040 tcp:4040` tunnels the device's localhost:4040 to the
        // host server over USB -- the same address for the emulator and a real
        // device, with no LAN IP to configure (and lower latency than wifi).
        private const val SERVER = "http://localhost:4040"
    }
}
