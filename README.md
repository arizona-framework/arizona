# Arizona

Arizona is a Web Framework for Erlang.

> ⚠️ Work in progress.

## In a Nutshell

> **NOTE:** The below is a very brief description of Arizona. More
> info upcoming soon.

```erlang
-module(view).
-behaviour(arizona_live_view).

% 'mount/1', 'render/1', and 'handle_event/3' are callbacks of
% 'arizona_live_view' behavior. 'handle_event/3' is optional.
-export([mount/1, render/1, handle_event/3, counter/1]).

% This header includes the the `?LV` macro.
-include_lib("arizona/include/arizona_live_view.hrl").

% Called once before the first render.
mount(Socket) ->
    {ok, arizona_socket:assign(count, 0, Socket)}.

% render/1 is called by the route to render the page.
render(Macros0) ->
    % Variables defined in 'Macros' are substituted in compile time.
    Macros = Macros0#{
        title => maps:get(title, Macros0, ~"Arizona")
    },
    ?LV(~"""
    <html lang="en">
    <head>
        <meta charset="UTF-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        {% This is a normal Erlang code written between curly braces. }
        {% The '_@title' is a variable and must be assigned, otherwise }
        {% the template render will fail with a bad key: title error. }
        <title>{_@title}</title>
        <script src="assets/js/morphdom.min.js"></script>
        <script src="assets/js/arizona.js"></script>
        <script src="assets/js/example.js"></script>
    </head>
    <body>
        {% The <.module:function/> tag renders a block/component. }
        <.arizona_handler:counter
            count={_@count}
            btn_text="Increment #1"
        />

        {% Blocks/components are isolated and have their own state. }
        <.arizona_handler:counter
            count={_@count}
            btn_text="Increment #2"
        />
    </body>
    </html>
    """).

counter(Macros) ->
    ?LV(~"""
    {% :stateful is a directive. Declaring it isolates the block state. }
    {% Stateful blocks receive an auto-generated arz-id attribute.      }
    <div :stateful>
        <div>Count: {_@count}</div>
        {% :on* are directives to send events to the server.            }
        {% Events are handled in the handle_event/3 callback.           }
        {% The block view is called if no :target directive is defined. }
        <button type="button" :onclick="incr">
            {_@btn_text}
        </button>
    </div>
    """).

handle_event(<<"incr">>, _Payload = #{}, Socket) ->
    {noreply, arizona_socket:assign(count, increment(Socket), Socket)}.

increment(#{assigns := Assigns}) ->
    maps:get(count, Assigns) + 1.
```

![showcase](/assets/showcase.gif)

## Roadmap

- [ ] Reduce the size of the tree by combining the neighboring texts.
This is the first optimization that should be implemented to enhance performance;
- [ ] Declare macros in HTML attributes;
- [ ] Declare event payloads in HTML attributes;
- [ ] A slot system for blocks;
- [ ] :if, :for and :case directives;
- [ ] JS Hooks;
- [ ] Hot reload;
- [ ] Communication between connected clients;
- [ ] A bundler plugin to reduce Javascript files. Probably esbuild;
- [ ] A Tailwind plugin;
- [ ] ...and much more.

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feel free to [submit an issue on Github](https://github.com/williamthome/arizona/issues/new).

## License

Copyright (c) 2023-2024 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/williamthome/arizona).

See [LICENSE.md](LICENSE.md) for more information.

