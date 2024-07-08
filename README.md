# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a web framework for Erlang.

> ⚠️ Work in progress.

## In a Nutshell

> **NOTE:**
> The below is a very brief description of Arizona by just copying some code of the `./arzex`
example project.
> Please see the project folder for the complete codebase.
> More info upcoming soon.

```erlang
% config/sys.config
[{arizona, [
    {endpoint, #{
        % Routes are plain Cowboy routes.
        routes => [
            % Static
            {"/favicon.ico", cowboy_static, {priv_file, arzex, "static/favicon.ico"}},
            {"/robots.txt", cowboy_static, {priv_file, arzex, "static/robots.txt"}},
            {"/assets/[...]", cowboy_static, {priv_dir, arzex, "static/assets"}},

            % This is the route rendered in the example. It will call
            % `arzex_live_counter:render(_Macros = #{})` once to compile the
            % route template and store it as a persistent_term and call
            % `arzex_live_counter:mount/1` on the first render of the page
            % and when the client connects to the server via WebSocket.
            {"/", arizona_live_handler, {arzex_live_counter, render, #{}}}
        ],
        % Recompile the code and refresh the page of the connected WebSocket clients.
        live_reload => true
    }}
]}].
```

```erlang
% src/arzex_live_counter.erl
-module(arzex_live_counter).
-behaviour(arizona_live_view).

%% arizona_live_view callbacks.
%% mount/1 and render/1 are required, and handle_event/3 is optional.
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Component functions.
-export([counter/1]).
-export([button/1]).

%% Libs.
%% `arizona.hrl` contains the ?ARIZONA_LIVEVIEW macro.
-include_lib("arizona/include/arizona.hrl").

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

% mount/1 is called on the first render of the page and when the client
% connects to the server via WebSocket.
mount(#{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns, 0),
    {ok, arizona_socket:put_assign(count, Count, Socket)}.

% render/1 is called by the route to compile the template.
% Macros substitutes variables.
% render/1 and Macros are resolved once in the compile time.
% The goal of Arizona is to have the most compact and performant
% template as possible. What could be compiled, would be compiled.
render(Macros0) ->
    Macros = Macros0#{
        title => maps:get(title, Macros0, ~"Arizona")
    },
    ?ARIZONA_LIVEVIEW(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">

        {% Erlang code is defined between curly braces `{}`.            }
        {% Variables are atoms prefixed with `_@`. That's a merl syntax }
        {% and a valid Erlang code. Arizona uses merl under the hood.   }
        <title>{_@title}</title>

        <script src="assets/js/main.js"></script>
    </head>
    <body>
        {% <.module:function/> tags renders a component.            }
        {% The attributes are passed as 'Assigns' to the component. }
        <.arzex_live_counter:counter
            count={_@count}
            btn_text="Increment #1"
            event="incr"
        />

        {% Local functions can be declared as <.function>.  }
        {% IMPORTANT: Component functions must be exported. }
        <.counter
            count={99}
            btn_text="Increment #2"
            event="decr"
        >
            {% No inner content is rendered by now, but }
            {% a slot system is on the Arizona roadmap. }
        </.counter>
    </body>
    </html>
    """).

% Handle client events.
handle_event(<<"incr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) + 1,
    {noreply, arizona_socket:put_assign(count, Count, Socket)};
handle_event(<<"decr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) - 1,
    {noreply, arizona_socket:put_assign(count, Count, Socket)}.

%% --------------------------------------------------------------------
%% Component functions.
%% --------------------------------------------------------------------

counter(Macros) ->
    ?ARIZONA_LIVEVIEW(~s"""
    <div :stateful>
        <div>Count: {_@count}</div>
        <.button event={_@event} text={_@btn_text} />
    </div>
    """).

button(Macros) ->
    ?ARIZONA_LIVEVIEW(~s"""
    {% NOTE: On this example, :onclick is and expression to be }
    {%       dynamic. It could be just, e.g., :onclick="incr". }
    <button type="button" :onclick={arizona_js:send(_@event)}>
        {_@text}
    </button>
    """).
```

```js
// priv/static/assets/js/main.js
"use strict"

const connectParams = { }

arizona.connect(connectParams, () => {
    console.info("[Client] I'm connected!")
})
```

```console
$ rebar3 shell
Arizona is running at http://0.0.0.0:8080
```

> **NOTE:** Only the minimal data is passed to patch changes.

![showcase](/assets/showcase.gif)

## Roadmap

- [ ] Improve tests and documentation;
- [ ] Declare macros in HTML attributes;
- [ ] Declare event payloads in HTML attributes;
- [ ] A slot system for components;
- [ ] :if, :for and :case directives;
- [ ] JS Hooks;
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

Copyright (c) 2023 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/williamthome/arizona).

See [LICENSE.md](LICENSE.md) for more information.
