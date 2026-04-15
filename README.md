# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a real-time web framework for Erlang/OTP. It renders HTML on the server, diffs changes at the template level, and pushes minimal updates to the browser over WebSocket.

Templates are plain Erlang terms compiled via parse transform. The server owns the state; the client is a thin DOM patcher.

## Status

Arizona is in `0.x`. The core is functional and covered by tests, but
the API may change between minor versions. Pin a version in your deps.

## Features

- **Server-side rendering + live updates**: SSR for the first page load, WebSocket for real-time changes
- **Erlang-native templates**: `{Tag, Attrs, Children}` tuples compiled at build time, no separate template language
- **Compile-time diffing**: fingerprinted statics sent once, only dynamic values cross the wire
- **Stateful handlers**: `mount/1`, `render/1`, `handle_event/3`, `handle_info/2`, `unmount/1` lifecycle
- **Per-view messaging**: `?send`/`?send_after` route messages to any stateful handler (root or child)
- **Streams**: keyed collections with insert, delete, update, move, sort, and limit operations
- **SPA navigation**: client-side routing with `az-navigate`, server renders the new page via WebSocket
- **Connection context**: `?connected` macro distinguishes SSR from live mount
- **PubSub**: `?subscribe`/`?unsubscribe` for cross-view and cross-tab messaging
- **Route middlewares**: `fun(Req, Bindings) -> {cont, Req, Bindings} | {halt, Req}` for auth and sessions
- **On-mount hooks**: per-route `on_mount` pipeline runs before every mount including navigate
- **Connection params**: URL query params + custom JS params merged into handler bindings
- **Element hooks**: client-side `mounted`, `updated`, `destroyed` callbacks via `az-hook`
- **Framework-agnostic transport**: `arizona_socket` + adapter behaviour, cowboy is optional

## Requirements

- Erlang/OTP 28 or later

## Installation

Add `arizona` to your `rebar.config` dependencies:

```erlang
{deps, [
    {arizona, "0.1.0"},
    {cowboy, "2.14.2"}
]}.
```

Cowboy is required for the built-in HTTP/WebSocket transport. If you
write your own `arizona_adapter`, you can skip it.

## Quick start

This walks through a minimal page with an embedded counter: a parent page,
a child stateful counter, a layout, and the Cowboy server wiring.

### 1. Create the counter component

The counter is a child component. Its `id` and initial `count` come from
the parent via the `?stateful` macro (see step 2), so `mount/1` just
passes them through:

```erlang
%% src/my_counter.erl
-module(my_counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{az_click, arizona_js:push_event(~"dec")}], [~"-"]},
            {span, [], [~" Count: ", ?get(count), ~" "]},
            {button, [{az_click, arizona_js:push_event(~"inc")}], [~"+"]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []}.
```

### 2. Create the parent page

The page is the route's root handler. It receives its own `id` from the
route's `bindings` (see step 4) and mounts the counter as a child:

```erlang
%% src/my_page.erl
-module(my_page).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1]).

mount(Bindings) ->
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Counter demo"]},
            ?stateful(my_counter, #{id => ~"counter", count => 0})
        ]}
    ).
```

### 3. Create the layout

The layout wraps every page with the HTML shell and loads the client
runtime that connects over WebSocket:

```erlang
%% src/my_layout.erl
-module(my_layout).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([render/1]).

render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [az_nodiff], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}]},
                {title, [], [?get(title, ~"Arizona")]}
            ]},
            {body, [], [
                ?inner_content,
                {script, [{type, ~"module"}], [
                    ~"""
                    import { connect } from '/assets/arizona.min.js';
                    connect('/ws');
                    """
                ]}
            ]}
        ]}
    ]).
```

### 4. Wire up routes and start Cowboy

Start the listener from your application's `start/2` callback (or from a
`rebar3 shell` one-liner for experimentation). The page's `id` is supplied
here via the route's `bindings`:

```erlang
%% src/my_app.erl
-module(my_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Routes = [
        {live, ~"/", my_page, #{
            layout => {my_layout, render},
            bindings => #{id => ~"page", title => ~"Counter demo"}
        }},
        {ws, ~"/ws", #{}},
        {asset, ~"/assets", {priv_dir, arizona, "static/assets/js"}}
    ],
    {ok, _} = arizona_cowboy_server:start(http, #{
        transport_opts => [{port, 4040}],
        routes => Routes
    }),
    my_sup:start_link().

stop(_State) ->
    ok = arizona_cowboy_server:stop(http).
```

### 5. Run it

```bash
rebar3 shell
```

Open <http://localhost:4040> and click the buttons -- the server renders
the initial HTML, then pushes minimal diffs over WebSocket as the count
changes.

## Documentation

See [docs/architecture.md](docs/architecture.md) for the full architecture reference.

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for development setup,
testing guidelines, and contribution workflow.

### Contributors

<a href="https://github.com/arizona-framework/arizona/graphs/contributors">
  <img
    src="https://contrib.rocks/image?repo=arizona-framework/arizona&max=100&columns=10"
    width="15%"
    alt="Contributors"
  />
</a>

## Star History

<a href="https://star-history.com/#arizona-framework/arizona">
  <picture>
    <source
      media="(prefers-color-scheme: dark)"
      srcset="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date&theme=dark"
    />
    <source
      media="(prefers-color-scheme: light)"
      srcset="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date"
    />
    <img
      src="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date"
      alt="Star History Chart"
      width="100%"
    />
  </picture>
</a>

## License

Copyright (c) 2023-2026 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.
