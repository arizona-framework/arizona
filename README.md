# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a web framework for Erlang.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

## Template Syntax

Arizona utilizes a templating approach where Erlang code is embedded within HTML
using curly braces `{}`. This allows dynamic content generation by executing Erlang
functions directly within the HTML structure. For example:

```erlang
<ul>
    {arizona:render_list(fun(Item) ->
        arizona:render_nested_template(~"""
        <li>{Item}</li>
        """)
     end, arizona:get_binding(list, View))}
</ul>
```

No macros, no special syntaxes, just dynamic Erlang code embedded in static HTML.

## Basic Usage

> [!NOTE]
>
> The example below is a simplified version of the code from the [example repository](https://github.com/arizona-framework/arizona_example).
> Please refer to it for the complete code.

Create a new rebar3 app:

```bash
$ rebar3 new app arizona_example
===> Writing arizona_example/src/arizona_example_app.erl
===> Writing arizona_example/src/arizona_example_sup.erl
===> Writing arizona_example/src/arizona_example.app.src
===> Writing arizona_example/rebar.config
===> Writing arizona_example/.gitignore
===> Writing arizona_example/LICENSE.md
===> Writing arizona_example/README.md
```

Navigate to the project folder and compile it:

```bash
$ cd arizona_example && rebar3 compile
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling arizona_example
```

Add Arizona as a dependency in `rebar.config`:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

Include Arizona in the `src/arizona_example.app.src` file:

```erlang
{application, arizona_example, [
    % ...
    {applications, [
        kernel,
        stdlib,
        arizona
    ]},
    % ...
]}.
```

Update the dependencies:

```bash
$ rebar3 get-deps
===> Verifying dependencies...
```

Create a `config/sys.config` file:

```erlang
[
    {arizona, [
        {endpoint, #{
            % Routes are plain Cowboy routes for now.
            routes => [
                % Static files
                {"/assets/[...]", cowboy_static, {priv_dir, arizona_example, "assets"}},
                % Views are stateful and keep their state in memory.
                % Use the 'arizona_view_handler' to render Arizona views.
                % The 'arizona_example_page' will be mounted with the bindings 'title' and 'id'.
                % The layout is optional and wraps the view. It does not have a state; 
                % it simply places the view within its structure.
                {"/", arizona_view_handler,
                    {arizona_example_page, #{title => ~"Arizona Example", id => ~"app"}, #{
                        layout => arizona_example_layout
                    }}}
            ]
        }}
    ]}
].
```

Set the config file in `rebar.config`:

```erlang
{shell, [
    {config, "config/sys.config"},
    {apps, [arizona_example]}
]}.
```

Create the `src/arizona_example_page.erl` file:

```erlang
-module(arizona_example_page).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/4]).

mount(Bindings, _Socket) ->
    View = arizona:new_view(?MODULE, Bindings),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~"""
    <div id="{arizona:get_binding(id, View)}">
        {arizona:render_view(arizona_example_counter, #{
            id => ~"counter",
            count => 0
        })}
    </div>
    """).

handle_event(_Event, _Payload, _From, View) ->
    {noreply, View}.
```

Create the `src/arizona_example_counter.erl` view, which is defined in the render function of the page:

```erlang
-module(arizona_example_counter).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/4]).

mount(Bindings, _Socket) ->
    View = arizona:new_view(?MODULE, Bindings),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~"""
    <div id="{arizona:get_binding(id, View)}">
        <span>{integer_to_binary(arizona:get_binding(count, View))}</span>
        {arizona:render_component(arizona_example_components, button, #{
            handler => arizona:get_binding(id, View),
            event => ~"incr",
            payload => 1,
            text => ~"Increment"
         })}
    </div>
    """).

handle_event(~"incr", Incr, _From, View) ->
    Count = arizona:get_binding(count, View),
    arizona:put_binding(count, Count + Incr, View).
```

Create the button in `src/arizona_example_components.erl`, which is defined in the render
function of the view:

```erlang
-module(arizona_example_components).
-export([button/1]).

button(View) ->
    arizona:render_component_template(View, ~"""
    <button
        type="{arizona:get_binding(type, View, ~"button")}"
        onclick="{arizona:render_js_event(
            arizona:get_binding(handler, View),
            arizona:get_binding(event, View),
            arizona:get_binding(payload, View)
        )}"
    >
        {arizona:get_binding(text, View)}
    </button>
    """).
```

Create the optional layout `src/arizona_example_layout.erl`, which is defined in the config file:

```erlang
-module(arizona_example_layout).
-compile({parse_transform, arizona_transform}).
-behaviour(arizona_layout).

-export([mount/2]).
-export([render/1]).

mount(Bindings, _Socket) ->
    arizona:new_view(?MODULE, Bindings).

render(View) ->
    arizona:render_layout_template(View, ~""""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{arizona:get_binding(title, View)}</title>
        <script src="assets/arizona/main.js"></script>
        <script src="assets/main.js"></script>
    </head>
    <body>
        {% The 'inner_content' binding is auto-binded by Arizona in the view. }
        {arizona:get_binding(inner_content, View)}
    </body>
    </html>
    """").
```

Start the app:

```bash
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling arizona_example
Erlang/OTP 27 [erts-15.2.2] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit:ns]

Eshell V15.2.2 (press Ctrl+G to abort, type help(). for help)
===> Booted syntax_tools
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted arizona
===> Booted arizona_example
```

The server is up and running at <http://localhost:8080>, but it is not yet connected to the server.
To establish the connection, create `priv/assets/main.js` in your static assets directory (matching
the configured static route path in `config/sys.config` and matching the script added to the HTML of
the layout file previously) and add the connection initialization code to it:

```js
arizona.connect();
```

Open the browser again, and the button click will now increase the count value by one.

!["Counter Example"](./assets/counter_example.gif)

The value is updated in `arizona_example_counter:handle_event/4` via WebSocket, and the DOM patch
used the [morphdom library](https://github.com/patrick-steele-idem/morphdom) under the hood.
Note that only the changed part is sent as a small payload from the server to the client.

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feel free to [submit an issue on Github](https://github.com/williamthome/arizona/issues/new).

## License

Copyright (c) 2023-2025 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/williamthome/arizona).

See [LICENSE.md](LICENSE.md) for more information.
