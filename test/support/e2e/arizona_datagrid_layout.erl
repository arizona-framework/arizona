-module(arizona_datagrid_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Arizona Test Datagrid</title>
        <style>
            /* Modern CSS Custom Properties */
            :root \{
                --color-primary: #2563eb;
                --color-success: #059669;
                --color-warning: #d97706;
                --color-danger: #dc2626;
                --color-info: #0891b2;
                --color-gray-50: #f9fafb;
                --color-gray-100: #f3f4f6;
                --color-gray-400: #9ca3af;
                --color-gray-600: #4b5563;
                --space-1: 0.25rem;
                --space-2: 0.5rem;
                --space-3: 0.75rem;
                --radius-sm: 0.375rem;
                --radius-full: 9999px;
                --transition-fast: 150ms ease;
            }

            /* Enhanced Table Styling */
            .table th.sortable \{
                user-select: none;
                cursor: pointer;
                transition: all var(--transition-fast);
                position: relative;
                padding-right: 2rem;
            }
            .table th.sortable:hover \{
                background-color: var(--color-gray-100);
            }
            .table th.sortable::after \{
                content: '';
                position: absolute;
                right: 0.875rem;
                top: 50%;
                transform: translateY(-50%) translateY(2px);
                width: 0;
                height: 0;
                border-left: 3px solid transparent;
                border-right: 3px solid transparent;
                border-top: 4px solid var(--color-gray-400);
            }
            .table th.sortable::before \{
                content: '';
                position: absolute;
                right: 0.875rem;
                top: 50%;
                transform: translateY(-50%) translateY(-2px);
                width: 0;
                height: 0;
                border-left: 3px solid transparent;
                border-right: 3px solid transparent;
                border-bottom: 4px solid var(--color-gray-400);
            }
            .table th.sortable:hover::after \{
                border-top-color: var(--color-gray-600);
            }
            .table th.sortable:hover::before \{
                border-bottom-color: var(--color-gray-600);
            }

            /* Modern Badge System */
            .status-badge \{
                display: inline-flex;
                align-items: center;
                gap: var(--space-1);
                padding: var(--space-1) var(--space-3);
                border-radius: var(--radius-full);
                font-size: 0.75rem;
                font-weight: 600;
                line-height: 1;
                border: 1px solid transparent;
            }
            .role-badge-admin \{
                background: rgba(220, 38, 38, 0.1);
                color: var(--color-danger);
                border-color: rgba(220, 38, 38, 0.2);
            }
            .role-badge-moderator \{
                background: rgba(217, 119, 6, 0.1);
                color: var(--color-warning);
                border-color: rgba(217, 119, 6, 0.2);
            }
            .role-badge-user \{
                background: rgba(8, 145, 178, 0.1);
                color: var(--color-info);
                border-color: rgba(8, 145, 178, 0.2);
            }
            .status-badge-active \{
                background: rgba(5, 150, 105, 0.1);
                color: var(--color-success);
                border-color: rgba(5, 150, 105, 0.2);
            }
            .status-badge-inactive \{
                background: rgba(107, 114, 128, 0.1);
                color: var(--color-gray-600);
                border-color: rgba(107, 114, 128, 0.2);
            }
            .status-badge-pending \{
                background: rgba(217, 119, 6, 0.1);
                color: var(--color-warning);
                border-color: rgba(217, 119, 6, 0.2);
            }

            /* Fixed Action Button System */
            .action-button-group \{
                display: flex;
                gap: var(--space-2);
                align-items: center;
            }
            .action-btn \{
                display: inline-flex;
                align-items: center;
                justify-content: center;
                gap: var(--space-1);
                padding: var(--space-2) var(--space-3);
                height: 32px;
                min-width: 64px;
                border-radius: var(--radius-sm);
                font-size: 0.75rem;
                font-weight: 500;
                line-height: 1;
                border: 1px solid;
                cursor: pointer;
                transition: all var(--transition-fast);
                box-sizing: border-box;
                white-space: nowrap;
                vertical-align: top;
                -webkit-appearance: none;
                -moz-appearance: none;
                appearance: none;
                background: none;
                margin: 0;
                font-family: inherit;
            }
            .action-btn-primary \{
                background: white;
                color: var(--color-primary);
                border-color: var(--color-primary);
            }
            .action-btn-primary:hover \{
                background: var(--color-primary);
                color: white;
                transform: translateY(-1px);
            }
            .action-btn-danger \{
                background: white;
                color: var(--color-danger);
                border-color: var(--color-danger);
            }
            .action-btn-danger:hover \{
                background: var(--color-danger);
                color: white;
                transform: translateY(-1px);
            }
            .user-row:hover \{
                background-color: var(--color-gray-50);
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
