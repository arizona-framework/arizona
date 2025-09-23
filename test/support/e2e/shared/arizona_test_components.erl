-module(arizona_test_components).
-compile({parse_transform, arizona_parse_transform}).
-export([render_menu/1]).

render_menu(Bindings) ->
    arizona_template:from_string(~""""
    <nav class="test-nav">
        <div class="nav-container">
            <div class="nav-brand">
                <h1>🌵 Arizona Test Apps</h1>
            </div>
            <ul class="nav-menu">
                {
                    ActiveUrl = arizona_template:get_binding(active_url, Bindings),
                    arizona_template:render_list(fun(#{url := Url, name := Name, desc := Desc}) ->
                        Class = case Url of ActiveUrl -> ~"active"; _ -> ~"" end,
                        arizona_template:from_string(~"""
                        <li class="nav-item">
                            <a
                                href="{Url}"
                                class="nav-link {Class}"
                            >
                                <span class="nav-name">{Name}</span>
                                <small class="nav-desc">{Desc}</small>
                            </a>
                        </li>
                        """)
                    end, [
                        #{url => ~"/realtime", name => ~"Realtime", desc => ~"PubSub system"},
                        #{url => ~"/counter", name => ~"Counter", desc => ~"Simple state management"},
                        #{url => ~"/todo", name => ~"Todo App", desc => ~"CRUD operations & filtering"},
                        #{url => ~"/datagrid", name => ~"Data Grid", desc => ~"Interactive table with sorting"},
                        #{url => ~"/modal", name => ~"Modal", desc => ~"Dynamic slot updates"},
                        #{url => ~"/blog", name => ~"Blog", desc => ~"Markdown templates with dynamic content"},
                        #{url => ~"/presence", name => ~"Presence", desc => ~"REST API + real-time updates"}
                    ])
                }
            </ul>
        </div>
    </nav>

    <style>
        /* Global body reset for all test apps */
        body \{
            margin: 0;
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
        }

        .test-nav \{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            box-shadow: 0 2px 12px rgba(0,0,0,0.1);
            margin-bottom: 2rem;
        }

        .nav-container \{
            margin: 0;
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 1rem 2rem;
        }

        .nav-brand h1 \{
            color: white;
            margin: 0;
            font-size: 1.5rem;
            font-weight: 600;
        }

        .nav-menu \{
            display: flex;
            list-style: none;
            margin: 0;
            padding: 0;
            gap: 0.5rem;
        }

        .nav-item \{
            position: relative;
        }

        .nav-link \{
            display: block;
            padding: 0.75rem 1rem;
            color: rgba(255, 255, 255, 0.9);
            text-decoration: none;
            border-radius: 8px;
            transition: all 0.2s ease;
            text-align: center;
            min-width: 120px;
            box-shadow: 0 2px 8px transparent;
        }

        .nav-link:hover \{
            background: rgba(255, 255, 255, 0.1);
            color: white;
            transform: translateY(-1px);
        }

        .nav-link.active \{
            background: rgba(255, 255, 255, 0.2);
            color: white;
            box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        }

        .nav-name \{
            display: block;
            font-weight: 600;
            font-size: 0.9rem;
        }

        .nav-desc \{
            display: block;
            font-size: 0.75rem;
            opacity: 0.8;
            margin-top: 0.25rem;
            line-height: 1.2;
        }

        @media (max-width: 768px) \{
            .nav-container \{
                flex-direction: column;
                gap: 1rem;
                padding: 1rem;
            }

            .nav-menu \{
                flex-wrap: wrap;
                justify-content: center;
            }

            .nav-link \{
                min-width: auto;
                padding: 0.5rem 0.75rem;
            }

            .nav-desc \{
                display: none;
            }
        }
    </style>
    """").
