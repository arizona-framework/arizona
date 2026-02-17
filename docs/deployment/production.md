# Production

- [HTTPS/SSL](#httpsssl)
- [Transport/Protocol Options](#transportprotocol-options)
- [Disable Reloader](#disable-reloader)
- [Performance](#performance)

## HTTPS/SSL

Enable HTTPS by setting the scheme to `https` and providing certificate paths in `transport_opts`:

```erlang
[{arizona, #{
    server => #{
        scheme => https,
        transport_opts => [{port, 443},
                           {certfile, "/etc/ssl/certs/my_app.pem"},
                           {keyfile, "/etc/ssl/private/my_app.key"}],
        routes => [...]
    }
}}].
```

For Let's Encrypt certificates, point `certfile` and `keyfile` to the live certificate files
(typically under `/etc/letsencrypt/live/yourdomain/`). The paths should reference the actual PEM
files: `fullchain.pem` for the certificate and `privkey.pem` for the key.

In more complex deployments, consider using a reverse proxy (nginx, Caddy, HAProxy) for TLS
termination. This approach offloads certificate management and renewal to a dedicated layer, while
Arizona handles HTTP traffic behind the proxy. When using a reverse proxy, configure Arizona to
listen on HTTP (the default scheme) on a local port, and let the proxy handle the HTTPS-to-HTTP
forwarding.

## Transport/Protocol Options

Tune Cowboy's transport and protocol settings for production workloads:

```erlang
[{arizona, #{
    server => #{
        transport_opts => [{port, 8080},
                           {ip, {0, 0, 0, 0}},
                           {num_acceptors, 100}],
        proto_opts => #{
            idle_timeout => 60000,
            request_timeout => 30000
        },
        routes => [...]
    }
}}].
```

`transport_opts` controls the Ranch listener -- the layer responsible for accepting TCP connections.
Key options include:

| Option | Description |
| -------- | ------------- |
| `port` | Port number to listen on |
| `ip` | IP address to bind to (use `{0, 0, 0, 0}` for all interfaces) |
| `num_acceptors` | Number of acceptor processes in the pool |
| `certfile` | Path to the SSL certificate file (HTTPS only) |
| `keyfile` | Path to the SSL private key file (HTTPS only) |

`proto_opts` controls Cowboy's HTTP handler -- the layer responsible for processing requests. Key
options include:

| Option | Description |
| -------- | ------------- |
| `idle_timeout` | Time in milliseconds before an idle connection is closed |
| `request_timeout` | Maximum time in milliseconds to wait for a request |

These options are passed directly to Cowboy, so any option supported by Cowboy's `ranch` and
`cowboy_http`/`cowboy_http2` modules can be used.

## Disable Reloader

The file watcher and reloader must be disabled in production. These are development-only features
that introduce unnecessary overhead and potential security concerns in a deployed environment:

```erlang
[{arizona, #{
    reloader => #{enabled => false},
    server => #{routes => [...]}
}}].
```

When the reloader is disabled, no file system watchers are started, no reload-related PubSub
subscriptions are created, and the WebSocket handler skips the reload topic subscription. This
reduces the application's process count and eliminates any file system monitoring overhead.

A common pattern is to use separate `sys.config` files for development and production, or to use
Erlang's `config/sys.config.src` with environment variable substitution to toggle the reloader based
on the deployment environment.

## Performance

Arizona is designed for production performance at every layer of the stack:

- **Compile-time templates**: Templates are parsed and optimized at compile time. The template
  syntax is analyzed during compilation, and the resulting render functions contain only the logic
  needed to produce output -- there is zero parsing overhead at runtime.

- **Persistent term config**: Application configuration is cached in `persistent_term`, a shared,
  read-optimized storage in the BEAM VM. This provides near-zero lookup cost for configuration
  values that are read on every request, without the overhead of ETS or process dictionary lookups.

- **Minimal diffs**: The variable dependency tracker ensures only changed bindings trigger
  re-evaluation, and only the affected DOM elements are sent to the client. When a single binding
  changes, Arizona computes the minimal set of DOM patches required and sends only those patches
  over the WebSocket -- not the entire page or component tree.

- **Web Worker**: The JavaScript client runs WebSocket communication in a Web Worker, keeping the
  main browser thread responsive. Message parsing, diff application scheduling, and reconnection
  logic all happen off the main thread, so the UI remains smooth even during heavy server
  communication.

- **Process-per-connection**: Each live WebSocket connection runs in its own `arizona_live`
  GenServer process, leveraging the BEAM's lightweight process model. The BEAM can efficiently
  manage hundreds of thousands of concurrent processes, making Arizona well-suited for applications
  with many simultaneous connections.

These optimizations work together to deliver low-latency, high-throughput performance without
requiring manual tuning for most workloads.

See also: [Configuration](../server/configuration.md), [File
Watching](../development/file-watching.md)
