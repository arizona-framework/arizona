-module(arizona_counter_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_html(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Arizona Test Counter</title>
    </head>
    <body>
        {arizona_template:render_stateless(arizona_test_components, render_menu, #{
            active_url => arizona_template:get_binding(active_url, Bindings)
        })}
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        <script type="module">
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
        </script>
    </body>
    </html>
    """).
