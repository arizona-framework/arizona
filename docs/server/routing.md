# Routing

- [Route Types](#route-types)
- [Tuple Format](#tuple-format)
- [Mount Arguments](#mount-arguments)
- [Asset Route](#asset-route)

## Route Types

Arizona supports four route types, each mapping a URL pattern to a specific kind of handler:

- **view** — Server-side rendered pages with live WebSocket updates. This is the primary route type
  for building interactive applications. When a client requests a view route, Arizona renders the
  page on the server and then upgrades the connection to a WebSocket for real-time updates.
- **controller** — REST-style handlers for API endpoints or non-interactive pages. Use controllers
  when you need traditional request/response handling without a persistent WebSocket connection.
- **websocket** — Raw WebSocket connections for custom protocols. This gives you direct control over
  the WebSocket lifecycle without Arizona's view rendering layer.
- **asset** — Static file serving for JavaScript, CSS, images, and other assets. Asset routes map
  URL prefixes to files on disk or in the application's `priv` directory.

## Tuple Format

Routes are defined as tuples in the server configuration under `server => #{routes => [...]}`. View
and controller routes use a five-element tuple, while websocket and asset routes use a four-element
tuple:

```erlang
{view | controller, Path, Module, MountArgOrState, Middlewares}
{websocket, Path, WebSocketOpts, Middlewares}
{asset, Path, AssetConfig, Middlewares}
```

Here are examples of each route type:

```erlang
%% View route — renders a page and opens a live WebSocket connection
{view, ~"/", my_home_view, #{}, []}
{view, ~"/posts/:id", my_post_view, #{}, []}

%% Controller route — handles REST requests
{controller, ~"/api/users", my_users_controller, #{}, []}

%% WebSocket route — raw WebSocket connection
{websocket, ~"/ws/chat", #{}, []}

%% Asset route — serves static files
{asset, ~"/assets/[...]", {priv_dir, my_app, "static/assets"}, []}
```

Paths use Cowboy's pattern syntax:

- **Literal segments** — `/about`, `/api/users`
- **`:name` bindings** — `/posts/:id` captures the segment as a named binding (e.g., `:id`)
- **`[...]` wildcard** — `/assets/[...]` matches any number of trailing segments, useful for serving
  directory trees of static files

The fifth element, `Middlewares`, is a list of `{Module, Opts}` tuples that run before the handler.
Pass an empty list `[]` when no middleware is needed.

## Mount Arguments

The fourth element of a view route tuple is the `MountArg`. It can be any Erlang term and is passed
as the first argument to the view's `mount/2` callback. Use it to pass route-level configuration to
the view:

```erlang
%% Route definition with a mount argument
{view, ~"/blog", my_blog_view, #{category => ~"tech"}, []}
```

```erlang
%% In the view module
mount(#{category := Category}, _Request) ->
    arizona_view:new(?MODULE, #{category => Category, posts => []}, none).
```

For dynamic values captured from the URL path (such as `:id` bindings), use
`arizona_request:get_bindings/1` on the request instead of the mount argument:

```erlang
%% Route with a path binding
{view, ~"/posts/:id", my_post_view, #{}, []}
```

```erlang
%% In the view module
mount(_MountArg, Request) ->
    {Bindings, _Req} = arizona_request:get_bindings(Request),
    PostId = maps:get(id, Bindings),
    arizona_view:new(?MODULE, #{post_id => PostId}, none).
```

The mount argument is best suited for static, route-level configuration (defaults, feature flags,
categories), while request bindings carry the dynamic, per-request values from the URL.

## Asset Route

The asset route serves static files from the filesystem or from the application's `priv` directory.
It is a four-element tuple where the third element is an `AssetConfig` — a `cowboy_static:opts()`
term:

```erlang
{asset, Path, AssetConfig, Middlewares}
```

The `AssetConfig` specifies where to find the files on disk. Common forms include `{priv_dir, App,
Dir}` for a directory of files, `{priv_file, App, File}` for a single file, `{dir, Path}` for an
absolute directory, and `{file, Path}` for an absolute file path:

```erlang
{asset, ~"/assets/[...]", {priv_dir, my_app, "static/assets"}, []}
```

This configuration serves files from `my_app/priv/static/assets/` under the `/assets/` URL prefix.
For example, a request to `/assets/app.js` resolves to `my_app/priv/static/assets/app.js`.

A typical application includes at least one asset route for the compiled JavaScript and CSS bundles:

```erlang
#{
    server => #{
        routes => [
            {view, ~"/", my_home_view, #{}, []},
            {asset, ~"/assets/[...]",
                {priv_dir, my_app, "static/assets"}, []}
        ]
    }
}
```

---

Cross-references: [Configuration](configuration.md), [Middleware](middleware.md),
[Views](../components/views.md)
