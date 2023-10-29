# Arizona

Arizona is a Web Framework for Erlang.

## Table of contents

- [Goal](#goal)
- [Motivation](#motivation)
- [Roadmap](#roadmap)
- [Components](#components)
- [How it works](#how-it-works)
    - [Message transfer](#message-transfer)
- [Notes](#notes)
- [Template](#template)
- [Dependencies](#dependencies)
    - [Maintained by me](#maintained-by-me)
    - [Third-party](#third-party)
- [Event notes](#event-notes)
- [License](#license)

## Goal

The goal of this project is to have a web framework based on the [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html), where users connect to the server and send and receive events in realtime via WebSocket.

## Motivation

Phoenix has an important role in the Elixir's popularity and community growth, and when the community grows, the language also grows. Erlang has a wonderful community and a strong and rock-solid structure, but I wanna see Erlang also growing fast and becoming more and more popular to make this world a better and peaceful place. This is the reason and motivation for this project.

## Roadmap

- [X] Server side
    - [X] Server
    - [X] Request handler
    - [X] WebSocket
    - [X] JSON parser
    - [X] Template compiler
    - [X] Router
- [X] Client side
    - [X] Broker
    - [X] WebWorker
    - [X] Event bus
    - [X] Real-time updates
    - [X] Patch only the diff on real-time updates
    - [X] Persist state on reconnect
- [X] Example project
- [X] Create a rebar3 template

## Components

The components should be pluggable and do not rely directly to any library. The pluggable components are:

- Server
- JSON parser and generator
- Router
- Template compiler

All of them can be configured via a config file or setting them via `arizona` environment variables. A config file example:

```erlang
[{arizona, [
    {app, myarizona}, % your app name. This is used to expose the priv files.
    {server, #{
        adapter => arizona_server_adapter_cowboy,
        args => #{
            url => #{
                schema => http, % this is the only schema supported yet.
                ip => {127, 0, 0, 1},
                port => 8080
            }
        }
    }},
    {router, #{
        adapter => myarizona_web_router_example,
        args => #{
            % no optional args defined for this config yet.
        }
    }},
    {template, #{
        adapter => arizona_template_adapter_eel,
        args => #{
            % no optional args defined for this config yet.
        }
    }},
    {json, #{
        adapter => arizona_json_adapter_euneus,
        args => #{
            % no optional args defined for this config yet.
        }
    }}
]}]
```

> **Note**
>
> The config file is usually located at `./config/sys.config`.

## How it works

Let's look at the `arizona_example` folder and make it up and running via:

```
$ cd arizona_template
$ make serve
```

If all is good, the console will indicate that Arizona is running at http://127.0.0.1:8080.

> **Warning**
>
> There is a bug visiting the link for the first time after the server is up and running. You must reload the page 2 or 3 times for the page content appear. I need to investigate this. See the GIF below:

![Up and running bug](/assets/up-and-running-bug.gif)

Visiting the link you can see a counter label and a +1 button. Clicking the button, the counter number will be increased by one, like this:

![Up and running](/assets/up-and-running.gif)

Nothing special here, but let's see what is happening under the hood.

> **Note**
> The explanation below is very simplistic due to the event timeout.

The [server](/src/arizona_server.erl) receives the request, call the [handler](/src/arizona_handler.erl) that calls the [router plug](/arizona_example/src/web/arizona_example_web_router.erl) to resolve this request. In the example project, the router is configured to call the [arizona_example_web_live_home controller](/arizona_example/src/web/live/arizona_example_web_live_home.erl) in the home page (aka `/`). This controller is a live controller, which needs to respect the [arizona_live_view behavior](/src/arizona_live_view.erl). First, the mount function is called to resolve the initial setup of the [socket](/src/arizona_socket.erl), and then then request process to the client.

The client receives the rendered page and connects to the server via WebSocket, the server renders the page again only in the server side and store the information in a process. If the connection is lost, the client sends some information that the server knows how to restore the state.

In a nutshell, this is the project setup:

- Router: [arizona_example_web_router](/arizona_example/src/web/arizona_example_web_router.erl)
    ```erlang
    match(get, [ ]) ->
        LiveOpts = #{template => arizona_example_web_template_root},
        {{live, arizona_example_web_live_home, LiveOpts }, #{}}.
    % Note: get is the method and [ ] is the "/" path. The path needs to be improved, but it is the path split globally by slashes, e.g. <<"foo/bar/123">> =:= [ <<"foo">>, <<"bar">>, <<"123">> ]
    ```
- Controller: [arizona_example_web_live_home](/arizona_example/src/web/live/arizona_example_web_live_home.erl)
    ```erlang
    mount(_Params, Socket0) ->
        Socket = arizona_socket:bind(count, 0, Socket0),
        {ok, Socket}.

    render(Bindings) ->
        ?LV(<<"
        <div>Count: <span id=\"counter\"><%= @count .%></span></div>
        <button type=\"button\" arz-click=\"+1\">+1</button>
        ">>).

    handle_event(<<"+1">>, _Payload, Socket0) ->
        #{count := Count} = arizona_socket:get_bindings(Socket0),
        Socket = arizona_socket:bind(count, Count+1, Socket0),
        {ok, Socket}.
    ```
    > **Note**
    >
    > - The template uses the [eel](https://github.com/williamthome/eel) lib convention;
    > - The button has the `arz-click` attribute, where `arz` is an acronym for `arizona`. This attribute automatically calls the event in the view via `handle_event/3` when the button is clicked.
- Template: [arizona_example_web_template_root](/arizona_example/src/web/template/arizona_example_web_template_root.erl)
    ```erlang
    mount(_Params, Socket0) ->
        Socket = arizona_socket:bind(#{
            title => <<"arizona_example">>
        }, Socket0),
        {ok, Socket}.

    render(Bindings) ->
        ?LV(<<"
        <!DOCTYPE html>
        <html lang=\"en\">
        <head>
            <meta charset=\"UTF-8\">
            <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
            <title><%= @title .%></title>
            <script src=\"assets/arizona/js/arizona.js\"></script>
            <script src=\"assets/arizona/js/morphdom.min.js\"></script>
            <script src=\"assets/js/main.js\"></script>
        </head>
        <body>
            <%= @inner_content .%>
        </body>
        </html>
        ">>).
    ```
    > **Note**
    > The [morphdom](https://github.com/patrick-steele-idem/morphdom) lib is the same lib used by Phoenix to apply diffs and update the page content/nodes.
- Client: [javascript](/arizona_example/priv/static/assets/js/main.js)
    ```js
    "use strict"

    const arizona = arizonaFactory()

    const params = { }

    arizona.connect(params, () => {
        console.log("Arizona is connected")
    })
    ```
    > **Note**
    >
    > To send an event from client to server, just call via JS:
    > ```
    > arizona.send("myevent", { foo: "bar" })
    > ```
    > On the server side, the view will receive this event on the `handle_event/3` callback, where the first argument is `<<"myevent">>`, the second `#{<<"foo">> => <<"bar">>}`, and third the [socket](/src/arizona_socket.erl).
    >
    > The client can subscribe/unsubscribe to any event, e.g.:
    > ```js
    > arizona.on("myevent", (payload) => {
    >     console.log("myevent received", payload)
    > })
    > ```
    > or once:
    > ```js
    > arizona.once("myevent", (payload) => {
    >     console.log("myevent received", payload)
    > })
    > ```

### Message transfer

The idea is to transfer the minimum amount of data via client and server. When connected, the full content of the page is received by the client:

![Connect data](/assets/connect-data.gif)

But then, only the data that is needed to update the page is sent/received:

![Patch data](/assets/patch-data.gif)

## Notes

There are a lot of debugs in Javascript and Erlang files that need to be removed, but they help to understand what's happening under the hood.

## Template

There is a `rebar3` template under the [arizona_template](/arizona_template/) directory. Please see the [README](/arizona_template/README.md) for instructions. After installed, just call the below command to create a pre-configured project:

```command
$ rebar3 new arizona name=myarizona
===> Writing myarizona/config/sys.config
===> Writing myarizona/priv/static/assets/js/main.js
===> Writing myarizona/priv/static/favicon.ico
===> Writing myarizona/priv/static/robots.txt
===> Writing myarizona/src/web/controller/myarizona_web_controller_error.erl
===> Writing myarizona/src/web/live/myarizona_web_live_home.erl
===> Writing myarizona/src/web/template/myarizona_web_template_root.erl
===> Writing myarizona/src/web/myarizona_web_router.erl
===> Writing myarizona/src/myarizona_app.erl
===> Writing myarizona/src/myarizona_sup.erl
===> Writing myarizona/src/myarizona.app.src
===> Writing myarizona/.gitignore
===> Writing myarizona/LICENSE.md
===> Writing myarizona/Makefile
===> Writing myarizona/README.md
===> Writing myarizona/rebar.config
$ cd myarizona
```

## Dependencies

### Maintained by me

- [eel](https://github.com/williamthome/eel): Template compiler;
- [euneus](https://github.com/williamthome/euneus): JSON parser and generator.

### Third-party

- [cowboy](https://github.com/ninenines/cowboy): Server;
- [sync](https://github.com/rustyio/sync.git): Hot code reload.

## Event notes

There are huge improvements/work to do and infinite possibilities for a web framework. This is just the beginning. I'll be very happy to see any progress after this 48 hours of [SpawnFest](https://spawnfest.org/) coding to this lib.

Feel free to contact me. Let's improve this lib Erlang Web Framework together 💪

My special thanks to all of the folks involved in this event.

Wish luck to all participants o/

See you all next time 🚀

## License

Copyright (c) 2023 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open source and community-driven. All components are available under the Apache 2 License on [GitHub](https://github.com/spawnfest/arizona).

See [LICENSE.md](LICENSE.md) for more information.
