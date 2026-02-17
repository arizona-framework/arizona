# JavaScript Client Setup

- [npm Install](#npm-install)
- [Import Options](#import-options)
- [Client Instance](#client-instance)
- [Connecting](#connecting)

## npm Install

Install the Arizona JavaScript client from npm:

```bash
npm install @arizona-framework/client
```

The package includes the client library, logger modules, and TypeScript type declarations. The
single runtime dependency is [morphdom](https://github.com/patrick-steele-idem/morphdom) for
efficient DOM patching. The package is built with Vite and ships ready for use in modern bundler
setups or directly in the browser via a `<script type="module">` tag.

## Import Options

The package exports several entry points for different parts of the client:

```javascript
// Main client
import { Arizona } from '@arizona-framework/client';

// Console logger (recommended for development)
import { ArizonaConsoleLogger } from '@arizona-framework/client/logger/console';

// Base logger class (for custom loggers)
import { ArizonaLogger } from '@arizona-framework/client/logger';
```

TypeScript declarations are included automatically with the package — no separate `@types` package
is needed. Your editor should pick up type information for autocompletion and type checking out of
the box.

## Client Instance

Create an Arizona client instance by calling the `Arizona` constructor:

```javascript
import { Arizona } from '@arizona-framework/client';
import { ArizonaConsoleLogger } from '@arizona-framework/client/logger/console';

// Without logging
const arizona = new Arizona();

// With console logging (recommended during development)
const arizona = new Arizona({
    logger: new ArizonaConsoleLogger()
});
```

The constructor accepts an optional configuration object. The only option is `logger`, which takes
an instance of `ArizonaLogger` (or any subclass such as `ArizonaConsoleLogger`).

Under the hood, the client manages the WebSocket connection in a **Web Worker**, keeping the main
thread free for rendering and DOM updates. This means that WebSocket message parsing and
serialization happen off the main thread, so the UI stays responsive even under heavy message
traffic.

## Connecting

Connect to the Arizona server's WebSocket endpoint by calling `connect` with the endpoint path:

```javascript
arizona.connect("/ws");
```

The endpoint path should match a WebSocket-capable route on your Arizona server. The client
automatically handles reconnection, message queuing for messages sent before the connection is fully
established, and the initial render handshake with the server.

You can check the connection status at any time:

```javascript
if (arizona.isConnected()) {
    // Connected and ready
}
```

To disconnect manually:

```javascript
arizona.disconnect();
```

A typical setup in a layout template looks like this:

```html
<script type="module">
    import { Arizona } from '/assets/arizona.js';
    const arizona = new Arizona();
    arizona.connect("/ws");
    window.arizona = arizona;
</script>
```

Assigning the instance to `window.arizona` makes it accessible to inline event handlers and other
scripts on the page, which is useful during development and for interacting with the client from
templates.

---

Cross-references: [Logging](logging.md), [Layouts](../components/layouts.md),
[Installation](../getting-started/installation.md)
