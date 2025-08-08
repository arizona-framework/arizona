-module(arizona_modal_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Arizona Test Modal</title>
        <style>
            .control-btn \{
                margin: 5px;
                padding: 8px 16px;
                color: white;
                border: none;
                border-radius: 4px;
                cursor: pointer;
                font-size: 14px;
                font-weight: 500;
                transition: all 0.2s ease;
            }
            .control-btn:hover \{
                opacity: 0.9;
                transform: translateY(-1px);
                box-shadow: 0 2px 4px rgba(0,0,0,0.2);
            }
            .control-btn:active \{
                transform: translateY(0px);
                box-shadow: 0 1px 2px rgba(0,0,0,0.2);
            }
            .control-btn-success \{
                background: #10b981;
            }
            .control-btn-error \{
                background: #ef4444;
            }
            .control-btn-info \{
                background: #3b82f6;
            }
            .control-btn-action \{
                background: #6366f1;
            }
            .control-container \{
                margin-bottom: 20px;
                text-align: center;
            }
        </style>
    </head>
    <body>
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        <script type="module">
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
        </script>
    </body>
    </html>
    """).
