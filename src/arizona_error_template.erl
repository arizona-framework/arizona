-module(arizona_error_template).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

%% --------------------------------------------------------------------
%% Behaviour (arizona_view) exports
%% --------------------------------------------------------------------

-export([mount/2]).
-export([render/1]).

%% --------------------------------------------------------------------
%% Behaviour (arizona_view) callbacks
%% --------------------------------------------------------------------

-spec mount(MountArg, ArizonaRequest) -> View when
    MountArg :: {Error, Reason, StackTrace},
    Error :: atom(),
    Reason :: dynamic(),
    StackTrace :: erlang:stacktrace(),
    ArizonaRequest :: arizona_request:request(),
    View :: arizona_view:view().
mount({Error, Reason, StackTrace}, Req) ->
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"error",
            error => Error,
            reason => Reason,
            stacktrace => StackTrace,
            raw_request => arizona_request:get_raw_request(Req)
        },
        none
    ).

-spec render(Bindings) -> Template when
    Bindings :: map(),
    Template :: arizona_template:template().
render(Bindings) ->
    arizona_template:from_html(~""""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Arizona Framework - Error</title>
        <style>
            :root \{
                /* Arizona Brand Colors */
                --color-arizona-terracotta: #D2691E;
                --color-arizona-terracotta-dark: #B8460E;

                /* Dark Theme Colors */
                --color-bg-primary: #0F0F0F;
                --color-bg-secondary: #1A1A1A;
                --color-bg-tertiary: #262626;
                --color-surface-primary: rgba(255, 255, 255, 0.08);
                --color-surface-secondary: rgba(255, 255, 255, 0.12);
                --color-surface-hover: rgba(255, 255, 255, 0.16);

                /* Typography Colors */
                --color-text-primary: #F8FAFC;
                --color-text-secondary: #E2E8F0;
                --color-text-tertiary: #94A3B8;

                /* Border and Effects */
                --color-border-subtle: rgba(255, 255, 255, 0.08);
                --color-border-moderate: rgba(255, 255, 255, 0.16);
                --color-shadow-light: rgba(0, 0, 0, 0.25);

                /* Typography Scale */
                --font-size-sm: 0.875rem;
                --font-size-base: 1rem;
                --font-size-lg: 1.125rem;
                --font-size-xl: 1.25rem;
                --font-size-2xl: 1.5rem;

                /* Line Heights */
                --line-height-normal: 1.5;
                --line-height-relaxed: 1.75;

                /* Spacing Scale */
                --space-xs: 0.25rem;
                --space-lg: 1.5rem;
                --space-xl: 2rem;
                --space-2xl: 3rem;

                /* Radii */
                --radius-lg: 0.75rem;

                /* Transitions */
                --transition-normal: 250ms cubic-bezier(0.4, 0, 0.2, 1);

                /* Common shadows */
                --shadow-base: 0 4px 6px var(--color-shadow-light);
                --shadow-hover: 0 4px 12px var(--color-shadow-light);

                /* Common borders */
                --border-base: 1px solid var(--color-border-moderate);
            }

            * \{
                margin: 0;
                padding: 0;
                box-sizing: border-box;
            }

            body \{
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                    Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
                background: var(--color-bg-primary);
                min-height: 100vh;
                color: var(--color-text-primary);
                line-height: var(--line-height-normal);
                font-size: var(--font-size-base);
            }

            .error-container \{
                width: 100%;
                min-height: 100vh;
                display: flex;
                flex-direction: column;
                background: linear-gradient(
                    135deg,
                    var(--color-bg-primary) 0%,
                    var(--color-bg-secondary) 50%,
                    var(--color-bg-tertiary) 100%
                );
            }

            .error-header \{
                background: linear-gradient(
                    135deg,
                    var(--color-arizona-terracotta) 0%,
                    var(--color-arizona-terracotta-dark) 100%
                );
                color: white;
                padding: var(--space-xl) var(--space-2xl) var(--space-lg);
                display: flex;
                align-items: center;
                gap: var(--space-lg);
                box-shadow: var(--shadow-base);
            }

            .error-header .icon \{
                width: 3rem;
                height: 3rem;
                background: rgba(255,255,255,0.2);
                border-radius: var(--radius-lg);
                display: flex;
                align-items: center;
                justify-content: center;
                font-size: var(--font-size-xl);
                flex-shrink: 0;
            }

            .error-title \{
                font-size: var(--font-size-2xl);
                font-weight: 700;
                margin: 0;
            }

            .error-subtitle \{
                font-size: var(--font-size-lg);
                opacity: 0.95;
                margin-top: var(--space-xs);
                font-weight: 400;
                color: rgba(255,255,255,0.9);
            }

            .error-content \{
                padding: var(--space-2xl);
            }

            .main-section > * \{
                margin-bottom: var(--space-xl);
            }

            .main-section > *:last-child \{
                margin-bottom: 0;
            }

            .section \{
                border: var(--border-base);
                border-radius: var(--radius-lg);
                background: var(--color-surface-primary);
                transition: all var(--transition-normal);
                overflow: hidden;
            }

            .section[open] \{
                box-shadow: var(--shadow-hover);
            }

            .section-header \{
                background: var(--color-surface-secondary);
                padding: var(--space-lg);
                font-weight: 600;
                color: var(--color-text-primary);
                font-size: var(--font-size-base);
                cursor: pointer;
                transition: all var(--transition-normal);
                border-bottom: 1px solid var(--color-border-subtle);
            }

            .section-header:hover \{
                background: var(--color-surface-hover);
                color: var(--color-arizona-terracotta);
            }

            .section-content \{
                background: var(--color-bg-primary);
                border-top: 1px solid var(--color-border-subtle);
                position: relative;
            }

            .copy-button \{
                position: absolute;
                top: var(--space-lg);
                right: var(--space-lg);
                width: 2rem;
                height: 2rem;
                background: var(--color-surface-secondary);
                border: var(--border-base);
                border-radius: var(--radius-lg);
                color: var(--color-text-secondary);
                cursor: pointer;
                font-size: var(--font-size-sm);
                transition: all var(--transition-normal);
                opacity: 0;
                display: flex;
                align-items: center;
                justify-content: center;
            }

            .section-content:hover .copy-button \{
                opacity: 0.8;
            }

            .copy-button:hover, .copy-button:focus \{
                background: var(--color-surface-hover);
                color: var(--color-arizona-terracotta);
                opacity: 1;
                transform: translateY(-1px);
                outline: 2px solid var(--color-arizona-terracotta);
                outline-offset: 2px;
            }

            .copy-button.copied \{
                background: var(--color-arizona-terracotta);
                color: white;
                opacity: 1;
            }

            .section-content pre \{
                background: var(--color-bg-secondary);
                padding: var(--space-lg);
                font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
                font-size: var(--font-size-sm);
                color: var(--color-text-secondary);
                margin: 0;
                line-height: var(--line-height-relaxed);
                white-space: pre-wrap;
                word-wrap: break-word;
            }

            /* Responsive design */
            @media (max-width: 48rem) \{
                .error-header \{
                    padding: var(--space-lg) var(--space-xl);
                }

                .error-content \{
                    padding: var(--space-xl);
                }

                .error-title \{
                    font-size: var(--font-size-xl);
                }

                .error-subtitle \{
                    font-size: var(--font-size-base);
                }
            }

            @media (max-width: 30rem) \{
                .error-header \{
                    padding: var(--space-lg);
                    gap: var(--space-lg);
                }

                .error-header .icon \{
                    width: 2.5rem;
                    height: 2.5rem;
                }

                .error-content \{
                    padding: var(--space-lg);
                }

                .section-header \{
                    padding: var(--space-lg);
                }
            }
        </style>
    </head>
    <body>
        <div class="error-container">
            <div class="error-header">
                <div class="icon">⚠</div>
                <div>
                    <div class="error-title">
                        {io_lib:format("~tp", [arizona_template:get_binding(error, Bindings)])}
                    </div>
                    <div class="error-subtitle">
                        {
                            case arizona_template:get_binding(
                                reason, Bindings
                            ) of
                                {badkey, Key} ->
                                    io_lib:format(
                                        "Key '~tp' not found in bindings",
                                        [Key]
                                    );
                                function_clause -> ~"Function clause did not match arguments";
                                badarith -> ~"Arithmetic error";
                                {case_clause, Value} ->
                                    io_lib:format(
                                        "No case clause matched value: ~tp",
                                        [Value]
                                    );
                                Reason -> io_lib:format("~tp", [Reason])
                            end
                        }
                    </div>
                </div>
            </div>
            <div class="error-content">
                <div class="main-section">
                    {arizona_template:render_list(
                        fun({Title, Content, Open}) ->
                        arizona_template:from_html(~"""
                        <details class="section" {
                            case Open of
                                true -> ~"open";
                                false -> ~""
                            end
                        }>
                            <summary class="section-header">
                                {Title}
                            </summary>
                            <div class="section-content">
                                <button
                                    type="button"
                                    class="copy-button"
                                    onclick="copyContent(this)"
                                    title="Copy to clipboard"
                                >
                                    📋
                                </button>
                                <pre>{io_lib:format("~tp", [Content])}</pre>
                            </div>
                        </details>
                        """)
                    end, [
                        {
                            ~"Error Reason",
                            arizona_template:get_binding(reason, Bindings),
                            true
                        },
                        {
                            ~"Stack Trace",
                            arizona_template:get_binding(stacktrace, Bindings),
                            true
                        },
                        {
                            ~"Request Information",
                            arizona_template:get_binding(raw_request, Bindings),
                            false
                        }
                    ])
                    }
                </div>
            </div>
        </div>
        <script>
            function copyContent(button) \{
                const pre = button.nextElementSibling;
                const text = pre.textContent;

                navigator.clipboard.writeText(text).then(() => \{
                    // Show copied state
                    button.textContent = '✓';
                    button.classList.add('copied');
                    button.title = 'Copied!';

                    // Reset after 2 seconds
                    setTimeout(() => \{
                        button.textContent = '📋';
                        button.classList.remove('copied');
                        button.title = 'Copy to clipboard';
                    }, 2000);
                }).catch(() => \{
                    // Fallback for older browsers
                    const textArea = document.createElement('textarea');
                    textArea.value = text;
                    document.body.appendChild(textArea);
                    textArea.select();
                    document.execCommand('copy');
                    document.body.removeChild(textArea);

                    // Show copied state
                    button.textContent = '✓';
                    button.classList.add('copied');
                    button.title = 'Copied!';

                    setTimeout(() => \{
                        button.textContent = '📋';
                        button.classList.remove('copied');
                        button.title = 'Copy to clipboard';
                    }, 2000);
                });
            }
        </script>
    </body>
    </html>
    """").
