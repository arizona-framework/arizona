-module(arizona_realtime_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Arizona Realtime Example</title>
        <style>
            body \{
                font-family: Arial, sans-serif;
                margin: 40px;
                background: #f5f5f5;
            \}
            .container \{
                max-width: 800px;
                margin: 0 auto;
                background: white;
                padding: 40px;
                border-radius: 8px;
                box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            \}
            h1 \{
                color: #333;
                margin-bottom: 30px;
            \}
            .clock \{
                font-size: 48px;
                font-weight: bold;
                color: #2563eb;
                text-align: center;
                margin: 20px 0;
            \}
            .info \{
                background: #e0f2fe;
                padding: 20px;
                border-radius: 6px;
                margin: 20px 0;
            \}
            .tabs \{
                color: #666;
                font-style: italic;
            \}
        </style>
    </head>
    <body>
        {arizona_template:render_stateless(arizona_test_components, render_menu, #{
            active_url => arizona_template:get_binding(active_url, Bindings)
        })}
        <div class="container">
            <h1>Realtime Clock Example</h1>
            <div class="info">
                <p><strong>How it works:</strong> The server updates the time every second and broadcasts to all connected tabs.</p>
                <p class="tabs">Open multiple tabs to see real-time synchronization!</p>
            </div>
            {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        </div>
        <script type="module">
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
        </script>
    </body>
    </html>
    """).
