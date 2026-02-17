# Logging

- [Built-in Logger](#built-in-logger)
  - [ArizonaConsoleLogger](#arizonaconsolelogger)
  - [Log Levels](#log-levels)
- [Custom Logger](#custom-logger)
- [Prod vs Dev](#prod-vs-dev)

## Built-in Logger

Arizona's JavaScript client includes a logging system with log levels aligned to Erlang's `logger`
module. This alignment means that the log level semantics are consistent between the server and the
client, making it easier to reason about what each level means across the full stack.

### ArizonaConsoleLogger

`ArizonaConsoleLogger` is a ready-to-use logger that outputs to the browser console. Every message
is prefixed with `[Arizona]` so you can easily distinguish framework logs from your own application
output:

```javascript
import { Arizona } from '@arizona-framework/client';
import { ArizonaConsoleLogger } from '@arizona-framework/client/logger/console';

const logger = new ArizonaConsoleLogger();
const arizona = new Arizona({ logger });
```

The logger maps each log level to the appropriate browser console method:

- `error` messages go to `console.error`
- `warning` messages go to `console.warn`
- `info` messages go to `console.log`
- `debug` messages go to `console.log`

This means you can use your browser's built-in console filtering to show or hide Arizona logs by
severity.

### Log Levels

Arizona defines four log levels, with numeric values that match Erlang's `logger` module:

| Level   | Value | Console Method  |
| ------- | ----- | --------------- |
| error   | 3     | `console.error` |
| warning | 4     | `console.warn`  |
| info    | 6     | `console.log`   |
| debug   | 7     | `console.log`   |

Set the log level on the logger instance to control verbosity. Messages below the configured level
are suppressed. For example, setting the level to `warning` will show `error` and `warning` messages
but suppress `info` and `debug`.

## Custom Logger

To implement custom logging behavior, extend the `ArizonaLogger` base class and override the
`handleLog` method:

```javascript
import { ArizonaLogger, LOG_LEVELS } from '@arizona-framework/client/logger';

class MyRemoteLogger extends ArizonaLogger {
    handleLog(level, message, ...args) {
        // Send logs to a remote monitoring service
        fetch('/api/logs', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ level, message, args })
        });
    }
}

const arizona = new Arizona({ logger: new MyRemoteLogger() });
```

The `handleLog(level, message, ...args)` method is called for each log entry that passes the level
filter. The `level` is one of the numeric `LOG_LEVELS` values (see the [Log Levels](#log-levels)
table above), `message` is a human-readable string, and `...args` captures any additional arguments
passed to the log call.

You can also combine loggers by calling both the console and a remote service:

```javascript
class CombinedLogger extends ArizonaLogger {
    constructor() {
        super();
        this.console = new ArizonaConsoleLogger();
    }

    handleLog(level, message, ...args) {
        // Log to console
        this.console.handleLog(level, message, ...args);

        // Also send errors to a remote service
        if (level === LOG_LEVELS.error) {
            fetch('/api/logs', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ level, message, args })
            });
        }
    }
}
```

## Prod vs Dev

During development, use `ArizonaConsoleLogger` for full visibility into WebSocket connection events,
DOM patches, and errors:

```javascript
// Development — verbose console logging
const arizona = new Arizona({
    logger: new ArizonaConsoleLogger()
});
```

In production, either omit the logger entirely for silent operation, or use a custom logger that
sends only critical errors to a monitoring service:

```javascript
// Production — no logger (silent)
const arizona = new Arizona();
```

```javascript
// Production — remote error reporting only
class ProductionLogger extends ArizonaLogger {
    handleLog(level, message, ...args) {
        if (level === LOG_LEVELS.error) {
            navigator.sendBeacon('/api/errors',
                JSON.stringify({ level, message, args, url: location.href })
            );
        }
    }
}

const arizona = new Arizona({ logger: new ProductionLogger() });
```

A common pattern is to select the logger based on an environment variable or build flag, so the same
codebase works for both environments without manual changes.

---

Cross-references: [JavaScript Client Setup](setup.md)
