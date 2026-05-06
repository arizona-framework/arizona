# Arizona

[![Erlang CI](https://github.com/arizona-framework/arizona/actions/workflows/erlang.yml/badge.svg?branch=main)](https://github.com/arizona-framework/arizona/actions/workflows/erlang.yml)
[![Node.js CI](https://github.com/arizona-framework/arizona/actions/workflows/node-js.yml/badge.svg?branch=main)](https://github.com/arizona-framework/arizona/actions/workflows/node-js.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/arizona.svg)](https://hex.pm/packages/arizona)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/arizona/)
[![npm version](https://img.shields.io/npm/v/@arizona-framework/client.svg)](https://www.npmjs.com/package/@arizona-framework/client)
[![License](https://img.shields.io/hexpm/l/arizona.svg)](https://github.com/arizona-framework/arizona/blob/main/LICENSE.md)

![arizona logo](https://raw.githubusercontent.com/arizona-framework/arizona/main/assets/logo.jpg)

Arizona is a real-time web framework for Erlang/OTP. It renders HTML on the server, diffs changes at
the template level, and pushes minimal updates to the browser over WebSocket.

Templates are plain Erlang terms compiled via parse transform. The server owns the state; the client
is a thin DOM patcher.

## 🚧 Status

Arizona is in `0.x`. The core is functional and covered by tests, but the API may change between
minor versions. Pin an exact version in your deps (e.g. `{arizona, "0.1.0"}`) if you need stability
across upgrades.

## Features

- **SSR + live updates** -- HTML on first load, WebSocket diffs after
- **Erlang-native templates** -- `{Tag, Attrs, Children}` tuples compiled by parse transform
- **Compile-time static/dynamic split** -- statics sent once, only dynamics cross the wire
- **Three handler kinds** -- `arizona_view` (route pages), `arizona_stateful` (components),
  `arizona_stateless` (pure templates)
- **Streams** -- keyed collections with insert/delete/update/move/sort/limit
- **SPA navigation** -- `az_navigate` links, server renders the next page over WebSocket
- **PubSub** -- cross-view, cross-tab messaging via `arizona_pubsub`
- **Route middlewares** -- gate or rewrite requests before mount (auth, sessions, URL projection)
- **On-mount hooks** -- per-route pipeline that runs before every mount, including navigate
- **Element hooks** -- client-side `mounted`/`updated`/`destroyed` callbacks via `az_hook`
- **Dev-mode hot reload** -- `fs` watcher recompiles changed `.erl` files and pushes reload events
- **Pluggable transport** -- cowboy adapter built-in; write your own to swap it out

## Requirements

- Erlang/OTP 28 or later

## Installation

Add Arizona to your `rebar.config` dependencies. Cowboy is required for the built-in
HTTP/WebSocket transport; skip it only if you write your own `arizona_req` adapter.

```erlang
{deps, [
    {arizona, "~> 0.1"},
    cowboy
]}.
```

To track unreleased changes, swap the version for a git ref:

```erlang
{arizona, {git, "https://github.com/arizona-framework/arizona.git", {branch, "main"}}}
```

The client JavaScript ships baked into the rebar3 build (`priv/static/assets/js/*.min.js`). If
you need to bundle it yourself or consume it from a non-Erlang backend, install via npm:

```bash
npm install @arizona-framework/client
```

```js
import { connect } from '@arizona-framework/client';

connect('/ws');
```

## Quick start

A page with an embedded counter.

### 1. The counter component

`id` and initial `count` come from the parent via `?stateful` (step 2), so `mount/1` just passes
them through:

```erlang
%% src/my_counter.erl
-module(my_counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        %% id must be on the root element -- if it changes, the component is remounted
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

`?get(count)` registers `count` as a dependency of that template slot. When `handle_event` returns
new bindings, only slots whose tracked keys changed re-render -- the `<span>` patches; the buttons
don't.

The tuples carry more than just bindings: `mount/1` returns `{Bindings, Resets}` (an explicit
slot-reset map -- usually `#{}`), and `handle_event/3` returns `{Bindings, Resets, Effects}` where
`Effects` is a list of `arizona_js` commands (`set_title`, `navigate`, …) executed on the client.

### 2. The parent page

A **view** is the route's root handler. It receives initial bindings plus the request:

```erlang
%% src/my_page.erl
-module(my_page).
-include_lib("arizona/include/arizona_view.hrl").
-export([mount/2, render/1]).

mount(Bindings, _Req) ->
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Counter demo"]},
            %% id is required -- it's how the diff engine routes patches to this component
            ?stateful(my_counter, #{id => ~"counter", count => 0})
        ]}
    ).
```

### 3. The layout

The HTML shell. Loads the client runtime that connects over WebSocket:

```erlang
%% src/my_layout.erl
-module(my_layout).
-include_lib("arizona/include/arizona_stateless.hrl").
-export([render/1]).

render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
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

### 4. Configure the server

Add `arizona` to your app's `applications` list in `.app.src`:

```erlang
{applications, [kernel, stdlib, cowboy, arizona]}
```

Ensure `rebar3 shell` loads the config and starts your app by adding
to `rebar.config`:

```erlang
{shell, [
    {config, "config/sys.config"},
    {apps, [yourapp]}
]}.
```

Replace `yourapp` with your project's app name; `cowboy` and `arizona` are started
transitively from the `applications` list in `.app.src`.
Then declare routes in `config/sys.config`:

```erlang
[{arizona, [
    {server, #{
        routes => [
            {live, ~"/", my_page, #{
                layouts => [{my_layout, render}],
                bindings => #{id => ~"page", title => ~"Counter demo"}
            }},
            {ws, ~"/ws", #{}},
            {asset, ~"/assets", {priv_dir, arizona, "static/assets/js"}}
        ]
    }}
]}].
```

### 5. Run it

```bash
rebar3 shell
```

Open <http://localhost:4040> and click the buttons -- the server renders the initial HTML, then
pushes minimal diffs over WebSocket as the count changes.

## Documentation

See [docs/architecture.md](docs/architecture.md) for the full architecture reference -- module
breakdown, op codes, dev-mode file watchers, custom schemes/proto_opts, and imperative startup.

## Sponsors

If you like Arizona, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support ❤️

I also accept coffees ☕

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

Arizona is open-source under the Apache 2.0 License on
[GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.
