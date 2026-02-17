# Configuration

- [sys.config](#sysconfig)
- [Programmatic](#programmatic)
- [Options](#options)
  - [scheme](#scheme)
  - [transport\_opts](#transport_opts)
  - [proto\_opts](#proto_opts)

## sys.config

The primary way to configure Arizona is through the standard Erlang `sys.config` file. The
configuration lives under the `arizona` application key as a map with a `server` key containing
routes and server options.

```erlang
%% sys.config
[{arizona, #{
    server => #{
        routes => [
            {view, ~"/", my_home_view, #{}, []},
            {view, ~"/counter", my_counter_view, #{}, []},
            {asset, ~"/assets/[...]",
                {priv_dir, my_app, "static/assets"}, []}
        ]
    }
}}].
```

The config is loaded at application startup and cached in a `persistent_term` for fast access.
Because `persistent_term` lookups are extremely fast, reading the config on every request introduces
no measurable overhead.

## Programmatic

Access and reload configuration at runtime using the `arizona_config` module:

```erlang
%% Get current config (cached in persistent_term)
Config = arizona_config:get().

%% Force reload (clears the persistent_term cache and reloads from app env)
Config = arizona_config:reload().
```

Plugins are applied automatically when the config is loaded or reloaded. See [Plugins](plugins.md)
for details on how plugins transform the config.

## Options

Server options live inside the `server` key of the Arizona config map. They control how Cowboy
listens for connections and handles requests.

### scheme

Either `http` or `https`. Defaults to `http`.

```erlang
[{arizona, #{
    server => #{
        scheme => https,
        routes => [...]
    }
}}].
```

For HTTPS, you must also provide SSL certificate paths in `transport_opts`. See
[Production](../deployment/production.md) for a full HTTPS setup guide.

### transport\_opts

Cowboy/Ranch transport options. Controls the listening socket -- port, IP address, SSL certificates,
and other socket-level settings. The default port is 1912.

`transport_opts` accepts either a proplist or a map with a `socket_opts` key. When using a map,
top-level keys like `port` and `ip` are not read directly -- they must be inside `socket_opts`.

**Proplist format:**

```erlang
[{arizona, #{
    server => #{
        transport_opts => [{port, 8080}, {ip, {0, 0, 0, 0}}],
        routes => [...]
    }
}}].
```

**Map format** (options must be inside `socket_opts`):

```erlang
[{arizona, #{
    server => #{
        transport_opts => #{socket_opts => [{port, 8080}, {ip, {0, 0, 0, 0}}]},
        routes => [...]
    }
}}].
```

For HTTPS, include the certificate and key file paths:

```erlang
transport_opts => [{port, 443},
                   {certfile, "/path/to/cert.pem"},
                   {keyfile, "/path/to/key.pem"}]
```

### proto\_opts

Cowboy protocol options. Controls HTTP/2 settings, idle timeouts, and other protocol-level behavior.
These options are passed directly to Cowboy's protocol handler.

```erlang
[{arizona, #{
    server => #{
        proto_opts => #{
            idle_timeout => 60000
        },
        routes => [...]
    }
}}].
```

See also: [Routing](routing.md), [Plugins](plugins.md), [Production](../deployment/production.md)
