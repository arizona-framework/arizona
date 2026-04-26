# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a real-time web framework for Erlang/OTP. It renders HTML on the server, diffs changes at
the template level, and pushes minimal updates to the browser over WebSocket.

Templates are plain Erlang terms compiled via parse transform. The server owns the state; the client
is a thin DOM patcher.

## Status

Arizona is in `0.x`. The core is functional and covered by tests, but the API may change between
minor versions. Pin a version in your deps.

## Features

- **SSR + live updates** -- HTML on first load, WebSocket diffs after
- **Erlang-native templates** -- `{Tag, Attrs, Children}` tuples compiled by parse transform
- **Compile-time diffing** -- statics sent once, only dynamics cross the wire
- **Three handler kinds** -- `arizona_view` (route pages), `arizona_stateful` (components),
  `arizona_stateless` (pure templates)
- **Streams** -- keyed collections with insert/delete/update/move/sort/limit
- **SPA navigation** -- `az-navigate` links, server renders the next page over WebSocket
- **PubSub, middlewares, on-mount hooks, element hooks** -- the usual extension points
- **Framework-agnostic transport** -- cowboy is the default adapter, but optional

## Requirements

- Erlang/OTP 28 or later

## Installation

Arizona isn't on Hex yet -- pull it from GitHub's `main` branch. Add it to your `rebar.config`
dependencies:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona.git", {branch, "main"}}},
    cowboy
]}.
```

Cowboy is required for the built-in HTTP/WebSocket transport. If you write your own
`arizona_req` adapter, you can skip it.

## Quick start

A page with an embedded counter -- five files.

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

Arizona is 100% open-source and community-driven. All components are available under the Apache 2
License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.
