-module(arizona_counter_layout).
%-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).
-export([render/1]).

%% Layout for the counter test component
render(Socket) ->
    arizona_html:render_stateless(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Arizona Test Counter</title>
    </head>
    <body>
        {arizona_html:render_slot(main_content, Socket)}
        <script type="module">
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
        </script>
    </body>
    </html>
    """, Socket).
