-module(arizona_todo_app_layout).
%-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).
-export([render/1]).

render(Socket) ->
    arizona_html:render_stateless(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Arizona Todo App - E2E Test</title>
        <style>
            .todo-app \{
                background: #fff;
                margin: 130px 0 40px 0;
                position: relative;
                box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2);
            }
            .header \{
                padding: 20px;
                text-align: center;
            }
            .header h1 \{
                font-size: 80px;
                font-weight: 100;
                color: rgba(175, 47, 47, 0.15);
                margin: 0;
            }
            .new-todo \{
                padding: 16px;
                border: none;
                font-size: 24px;
                width: 100%;
                box-sizing: border-box;
            }
            .main \{
                position: relative;
                z-index: 2;
                border-top: 1px solid #e6e6e6;
            }
            .todo-item \{
                position: relative;
                font-size: 24px;
                border-bottom: 1px solid #ededed;
                padding: 15px;
                display: flex;
                align-items: center;
            }
            .todo-item.completed .todo-text \{
                color: #d9d9d9;
                text-decoration: line-through;
            }
            .toggle \{
                width: 40px;
                height: 40px;
                margin-right: 15px;
            }
            .todo-text \{
                flex: 1;
                word-break: break-all;
                padding: 15px 15px 15px 60px;
                display: block;
                line-height: 1.2;
            }
            .destroy \{
                width: 40px;
                height: 40px;
                margin: auto 0;
                font-size: 30px;
                color: #cc9a9a;
                background: none;
                border: none;
                cursor: pointer;
            }
            .footer \{
                color: #777;
                padding: 10px 15px;
                height: 20px;
                text-align: center;
                border-top: 1px solid #e6e6e6;
                display: flex;
                justify-content: space-between;
                align-items: center;
            }
            .todo-count strong \{
                font-weight: 300;
            }
            .filters \{
                margin: 0;
                padding: 0;
                list-style: none;
                display: flex;
                gap: 10px;
            }
            .filters li \{
                display: inline;
            }
            .filters li a \{
                color: inherit;
                margin: 3px;
                padding: 3px 7px;
                text-decoration: none;
                border: 1px solid transparent;
                border-radius: 3px;
                cursor: pointer;
            }
            .filters li a.selected \{
                border-color: rgba(175, 47, 47, 0.2);
            }
            .clear-completed \{
                float: right;
                position: relative;
                line-height: 20px;
                text-decoration: none;
                cursor: pointer;
                background: none;
                border: none;
                color: inherit;
            }
        </style>
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
