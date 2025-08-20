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
            /* Reset and base styles */
            * \{
                box-sizing: border-box;
            }

            /* Control buttons for testing */
            .control-btn \{
                margin: 5px;
                padding: 8px 16px;
                color: white;
                border: none;
                border-radius: 6px;
                cursor: pointer;
                font-size: 14px;
                font-weight: 500;
                transition: all 0.2s ease;
            }
            .control-btn:hover \{
                opacity: 0.9;
                transform: translateY(-1px);
                box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            }
            .control-btn:active \{
                transform: translateY(0px);
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
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
                margin-bottom: 40px;
                text-align: center;
                padding: 20px;
                background: white;
                border-radius: 12px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            }

            /* Modal overlay */
            .modal-overlay \{
                position: fixed;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                background: rgba(0, 0, 0, 0.5);
                display: flex;
                align-items: center;
                justify-content: center;
                z-index: 1000;
                backdrop-filter: blur(4px);
                animation: fadeIn 0.2s ease-out;
            }

            @keyframes fadeIn \{
                from \{ opacity: 0; }
                to \{ opacity: 1; }
            }

            /* Modal container */
            .modal-container \{
                background: white;
                border-radius: 16px;
                box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
                max-width: 440px;
                width: 100%;
                margin: 20px;
                overflow: hidden;
                animation: modalSlideIn 0.3s ease-out;
                position: relative;
            }

            @keyframes modalSlideIn \{
                from \{
                    opacity: 0;
                    transform: scale(0.95) translateY(-20px);
                }
                to \{
                    opacity: 1;
                    transform: scale(1) translateY(0);
                }
            }

            /* Modal header */
            .modal-header \{
                padding: 32px 32px 0 32px;
                text-align: center;
            }

            .modal-header h1 \{
                margin: 0;
                font-size: 20px;
                font-weight: 600;
                line-height: 1.3;
            }

            /* Modal body */
            .modal-body \{
                padding: 24px 32px;
                text-align: center;
            }

            .modal-body .message \{
                font-size: 16px;
                color: #6b7280;
                margin: 16px 0 0 0;
                line-height: 1.5;
            }

            .modal-body .greeting \{
                font-size: 20px;
                font-weight: 600;
                margin: 16px 0 0 0;
                line-height: 1.3;
            }

            /* Modal footer */
            .modal-footer \{
                padding: 0 32px 32px 32px;
                display: flex;
                gap: 12px;
                justify-content: center;
                flex-wrap: wrap;
            }

            /* Button styles */
            .modal-btn \{
                padding: 12px 24px;
                border: none;
                border-radius: 8px;
                font-size: 16px;
                font-weight: 600;
                cursor: pointer;
                transition: all 0.2s ease;
                min-width: 120px;
                text-align: center;
                line-height: 1;
            }

            .modal-btn:hover \{
                transform: translateY(-1px);
                box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            }

            .modal-btn:active \{
                transform: translateY(0);
            }

            /* Button variants */
            .modal-btn-primary \{
                background: #3b82f6;
                color: white;
            }

            .modal-btn-primary:hover \{
                background: #2563eb;
            }

            .modal-btn-success \{
                background: #10b981;
                color: white;
            }

            .modal-btn-success:hover \{
                background: #059669;
            }

            .modal-btn-danger \{
                background: #ef4444;
                color: white;
            }

            .modal-btn-danger:hover \{
                background: #dc2626;
            }

            .modal-btn-secondary \{
                background: #f3f4f6;
                color: #374151;
                border: 1px solid #d1d5db;
            }

            .modal-btn-secondary:hover \{
                background: #e5e7eb;
                border-color: #9ca3af;
            }

            /* Icon styles */
            .modal-icon \{
                width: 64px;
                height: 64px;
                border-radius: 50%;
                display: flex;
                align-items: center;
                justify-content: center;
                margin: 0 auto 24px auto;
                position: relative;
            }

            .modal-icon-success \{
                background: #dcfce7;
                color: #16a34a;
            }

            .modal-icon-error \{
                background: #fef2f2;
                color: #dc2626;
            }

            .modal-icon-info \{
                background: #dbeafe;
                color: #2563eb;
            }

            /* Icon symbols using CSS */
            .icon-checkmark::after \{
                content: '';
                width: 24px;
                height: 12px;
                border: 3px solid currentColor;
                border-top: none;
                border-right: none;
                transform: rotate(-45deg);
                margin-top: -6px;
            }

            .icon-warning::after \{
                content: '!';
                font-size: 32px;
                font-weight: bold;
                line-height: 1;
            }

            .icon-info::after \{
                content: 'i';
                font-size: 32px;
                font-weight: bold;
                font-style: italic;
                line-height: 1;
            }

            /* Theme colors for text */
            .text-success \{
                color: #16a34a;
            }

            .text-error \{
                color: #dc2626;
            }

            .text-info \{
                color: #2563eb;
            }

            /* Responsive design */
            @media (max-width: 480px) \{
                .modal-container \{
                    margin: 10px;
                    max-width: none;
                }

                .modal-header,
                .modal-body,
                .modal-footer \{
                    padding-left: 24px;
                    padding-right: 24px;
                }

                .modal-footer \{
                    flex-direction: column;
                }

                .modal-btn \{
                    width: 100%;
                }
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
            arizona.connect();
        </script>
    </body>
    </html>
    """).
