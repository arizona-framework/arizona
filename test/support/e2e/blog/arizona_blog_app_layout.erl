-module(arizona_blog_app_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~""""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Arizona Blog - Markdown Demo</title>
        <style>
            main \{
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                line-height: 1.6;
                color: #333;
                max-width: 800px;
                margin: 0 auto;
                padding: 20px;
                background: #fafafa;
            }

            h2, h3, h4 \{
                color: #1976d2;
                margin-top: 2rem;
            }

            h2 \{
                border-bottom: 3px solid #1976d2;
                padding-bottom: 0.5rem;
            }

            h3 \{
                border-bottom: 1px solid #e0e0e0;
                padding-bottom: 0.3rem;
            }

            pre \{
                background: #f5f5f5;
                border: 1px solid #ddd;
                border-radius: 4px;
                padding: 1rem;
                overflow-x: auto;
            }

            code \{
                background: #f5f5f5;
                padding: 2px 4px;
                border-radius: 3px;
                font-family: 'Monaco', 'Menlo', monospace;
            }

            pre code \{
                background: none;
                padding: 0;
            }

            blockquote \{
                border-left: 4px solid #1976d2;
                margin: 1rem 0;
                padding: 0.5rem 1rem;
                background: #f8f9fa;
            }

            table \{
                border-collapse: collapse;
                width: 100%;
                margin: 1rem 0;
            }

            th, td \{
                border: 1px solid #ddd;
                padding: 8px 12px;
                text-align: left;
            }

            th \{
                background: #1976d2;
                color: white;
            }

            tr:nth-child(even) \{
                background: #f9f9f9;
            }

            .tag \{
                display: inline-block;
                background: #e3f2fd;
                color: #1976d2;
                padding: 2px 8px;
                margin: 0 4px 4px 0;
                border-radius: 4px;
                font-size: 0.8em;
                font-weight: 500;
            }

            button \{
                background: #1976d2;
                color: white;
                border: none;
                padding: 8px 16px;
                border-radius: 4px;
                cursor: pointer;
                font-size: 0.9em;
                transition: background 0.2s;
            }

            button:hover \{
                background: #1565c0;
            }

            hr \{
                border: none;
                border-top: 2px solid #e0e0e0;
                margin: 2rem 0;
            }

            .footer \{
                margin-top: 3rem;
                padding-top: 2rem;
                border-top: 1px solid #e0e0e0;
                text-align: center;
                color: #666;
                font-size: 0.9em;
            }
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
        <main>
            {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        </main>
        <footer class="footer">
            <p>
                Arizona Blog Demo - Showcasing markdown templates with dynamic content<br>
                Built with ❤️ using Arizona Framework
            </p>
        </footer>
    </body>
    </html>
    """").
