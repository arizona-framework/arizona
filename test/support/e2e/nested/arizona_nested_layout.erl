-module(arizona_nested_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_html(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Nested Components Test</title>
        <style>
            body \{ font-family: Arial, sans-serif; margin: 20px; }
            .component \{
                border: 2px solid #ccc;
                padding: 15px;
                margin: 10px 0;
                border-radius: 5px;
            }
            .view \{ border-color: #4CAF50; background-color: #f1f8f4; }
            .stateful \{ border-color: #2196F3; background-color: #e3f2fd; }
            .stateless \{ border-color: #ff9800; background-color: #fff3e0; }
            .component-label \{
                font-weight: bold;
                margin-bottom: 10px;
                padding: 5px;
                background-color: rgba(0,0,0,0.1);
                border-radius: 3px;
            }
            button \{
                margin: 5px;
                padding: 8px 15px;
                cursor: pointer;
                border: none;
                border-radius: 3px;
                background-color: #2196F3;
                color: white;
            }
            button:hover \{ background-color: #1976D2; }
            .nested \{ margin-left: 20px; }
            .counter \{
                font-size: 1.2em;
                font-weight: bold;
                color: #1976D2;
                margin: 10px 0;
            }
        </style>
    </head>
    <body>
        {arizona_template:render_stateless(arizona_test_components, render_menu, #{
            active_url => arizona_template:get_binding(active_url, Bindings)
        })}
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        <script type="module">
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect('/live');
        </script>
    </body>
    </html>
    """).
