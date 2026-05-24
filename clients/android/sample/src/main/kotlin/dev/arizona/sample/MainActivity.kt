package dev.arizona.sample

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
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
 * Demo app: connects to the Arizona server's /native/counter view and renders
 * its JSON widget tree with Compose. The app supplies the widget *vocabulary*
 * (Column/Row/Text/Button); the library supplies the transport, diff, and
 * dispatch. Tapping a Button fires its on_tap push_event back to the server,
 * which diffs and pushes an OP_TEXT that updates the count.
 */
class MainActivity : ComponentActivity() {
    private val client by lazy { AzClient(SERVER, "/native/counter") }

    private val registry = WidgetRegistry()
        .register("Column") { node -> Column(Modifier.padding(16.dp)) { Children(node) } }
        .register("Row") { node -> Row { Children(node) } }
        .register("Text") { node -> Text(text(node)) }
        .register("Button") { node -> Button(onClick = { tap(node) }) { Text(text(node)) } }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme {
                Surface {
                    ArizonaView(client, registry)
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
        // The Android emulator reaches the host machine at 10.0.2.2.
        private const val SERVER = "http://10.0.2.2:4040"
    }
}
