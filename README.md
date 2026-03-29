# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a real-time web framework for Erlang/OTP. It renders HTML on the server, diffs changes at the template level, and pushes minimal updates to the browser over WebSocket.

Templates are plain Erlang terms compiled via parse transform. The server owns the state; the client is a thin DOM patcher.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

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

## Quick start

```erlang
-module(my_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings0) ->
    Bindings = maps:merge(#{id => ~"counter", count => 0}, Bindings0),
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{az_click, arizona_js:push_event(~"inc")}], [~"+"]},
            {p, [], [~"Count: ", ?get(count)]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.
```

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
