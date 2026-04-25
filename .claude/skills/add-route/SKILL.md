---
name: add-route
description: Add a new route to the Arizona router configuration. Use when creating new pages or endpoints.
argument-hint: [path]
allowed-tools: Read, Edit, Grep, Glob
---

Add a route for path `$ARGUMENTS` to the router configuration.

## 1. Find the route config

Search for `compile_routes` calls to locate where routes are defined (typically in the app module or config).

## 2. Route types

Choose the appropriate route type based on the user's intent:

**Live page (most common):**
```erlang
{live, Path, Handler, Opts}
```
- `Path` -- Cowboy route pattern, e.g. `<<"/">>`  or `<<"/users/:id">>`
- `Handler` -- a view module that includes `arizona_view.hrl` and exports `mount/2`, `render/1`
- `Opts` -- map with optional keys:
  - `bindings => map()` -- initial bindings passed to `mount/2` (default `#{}`)
  - `layout => {LayoutMod, LayoutFun}` -- layout to wrap the page (optional)
  - `on_mount => [Hook]` -- `on_mount` hooks run before the handler's `mount/2`
  - `middlewares => [fun((az:request(), map()) -> {cont, az:request(), map()} | {halt, az:request()})]`

**WebSocket endpoint:**
```erlang
{ws, Path, Opts}
```

**Static assets:**
```erlang
{asset, Path, {priv_dir, App, SubDir}}
{asset, Path, {dir, AbsoluteDir}}
```

**Generic Cowboy handler:**
```erlang
{controller, Path, Handler, State}
```

## 3. If creating a new live page

Also scaffold the view module using the `/new-handler` skill pattern (choose "route-level view"):
- Include `arizona_view.hrl`
- `mount/2` takes `(Bindings, az:request())` and must set `id => <<"page">>` (or appropriate view id)
- `render/1` with `?html(...)`
- `handle_event/3` if interactive

## 4. Verify

After adding the route, check that:
- The handler module exists
- The path doesn't conflict with existing routes
- The layout module exists if `layout` is specified in opts
