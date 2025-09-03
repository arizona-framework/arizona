-module(presence_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Arizona Presence Demo</title>
        <style>
            body \{ font-family: Arial, sans-serif; margin: 40px; }
            .container \{ max-width: 800px; margin: 0 auto; }
            .user-info \{ background: #f0f0f0; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
            .online-users \{ background: #e8f5e8; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
            .user-item \{ padding: 8px; margin: 4px 0; background: white; border-radius: 4px; }
            .controls \{ margin: 20px 0; }
            button \{ padding: 10px 20px; margin: 5px; border: none; border-radius: 4px; cursor: pointer; }
            button:disabled \{ background: #6c757d; cursor: not-allowed; opacity: 0.6; }
            .join-btn \{ background: #28a745; color: white; }
            .leave-btn \{ background: #dc3545; color: white; }
            .status \{ padding: 10px; margin: 10px 0; border-radius: 4px; display: none; }
            .success \{ background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }
            .error \{ background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
        </style>
        <script type="module" async>
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
        </script>
    </head>
    <body>
        {arizona_template:render_stateless(arizona_test_components, render_menu, #{
            active_url => arizona_template:get_binding(active_url, Bindings)
        })}
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
    </body>
    </html>
    """).
