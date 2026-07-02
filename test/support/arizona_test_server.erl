-module(arizona_test_server).
-export([start/0, stop/0, routes/0]).

start() ->
    %% The session fixtures (arizona_session_view/controller) encrypt the az_session
    %% cookie via arizona_crypto, which needs a signing key. The other fetch fixtures
    %% set plain cookies and don't read it.
    ok = application:set_env(arizona, secret_key, <<"e2e-test-secret-key-0123456789ab">>),
    {ok, _} = arizona_roadrunner_server:start(http, #{
        transport_opts => [{port, {env, "PORT", 4040}}],
        routes => routes()
    }),
    %% Set the server app env *after* the listener is up (arizona_app has already
    %% booted, so this won't double-start a server) so the dev MCP server's
    %% `list_routes` tool can read the live routes.
    ok = application:set_env(arizona, server, #{routes => routes()}),
    ok.

%% The dev server's routes. Exposed so the dev MCP server's `list_routes` tool
%% can introspect them (see arizona_dev_mcp).
routes() ->
    Layouts = [{arizona_layout, render}],
    [
        {live, <<"/">>, arizona_page, #{
            bindings => #{title => <<"Welcome">>},
            layouts => Layouts
        }},
        {live, <<"/about">>, arizona_about, #{
            bindings => #{title => <<"About">>},
            layouts => Layouts
        }},
        {live, <<"/datatable">>, arizona_datatable, #{
            bindings => #{title => <<"DataTable">>},
            layouts => Layouts
        }},
        {live, <<"/mixed">>, arizona_mixed_children, #{
            bindings => #{title => <<"Mixed">>},
            layouts => Layouts
        }},
        {live, <<"/inline">>, arizona_inline, #{
            bindings => #{title => <<"Inline">>},
            layouts => Layouts
        }},
        {live, <<"/chat">>, arizona_chat, #{
            bindings => #{title => <<"Chat">>},
            layouts => Layouts
        }},
        {live, <<"/local">>, arizona_local, #{
            bindings => #{title => <<"Local">>},
            layouts => Layouts
        }},
        {live, <<"/local-nested">>, arizona_local_nested, #{
            bindings => #{title => <<"Nested">>},
            layouts => Layouts
        }},
        {live, <<"/local-app">>, arizona_local_app, #{
            bindings => #{title => <<"App">>},
            layouts => Layouts
        }},
        %% Native-shell (OS) capability seam e2e -- a fake window.__arizona_os__
        %% (the Electron-preload equivalent) drives capability negotiation,
        %% server/client OS commands, and inbound OS events.
        {live, <<"/os">>, arizona_os_demo, #{
            bindings => #{title => <<"OS">>},
            layouts => Layouts
        }},
        {live, <<"/pip">>, arizona_pip, #{
            bindings => #{id => ~"pip_demo", title => <<"PiP">>},
            layouts => Layouts
        }},
        {live, <<"/transitions">>, arizona_transitions, #{
            bindings => #{title => <<"Transitions">>},
            layouts => Layouts
        }},
        %% In-place SPA navigation (az-patch) demo: az-patch links switch the
        %% :section while a chrome-owned counter survives (the view isn't
        %% remounted). Two routes sharing arizona_patch_demo -> same-handler patch.
        {live, <<"/patch-demo/:section">>, arizona_patch_demo, #{
            bindings => #{title => <<"Patch demo">>},
            middlewares => [arizona_middleware:extract([path_bindings])],
            layouts => Layouts
        }},
        {live, <<"/transitions/detail">>, arizona_transitions_detail, #{
            bindings => #{title => <<"Transition detail">>},
            layouts => Layouts
        }},
        {live, <<"/crashable">>, arizona_crashable, #{
            layouts => Layouts,
            middlewares => [arizona_middleware:extract([params])]
        }},
        {live, <<"/scroll-home">>, arizona_scroll_home, #{layouts => Layouts}},
        {live, <<"/scroll-about">>, arizona_scroll_about, #{layouts => Layouts}},
        {live, <<"/navigate-halt">>, arizona_navigate_halt, #{layouts => Layouts}},
        {live, <<"/login">>, arizona_login, #{
            layouts => Layouts,
            middlewares => [{arizona_middleware, fetch_flash}]
        }},
        {live, <<"/protected">>, arizona_navigate_halt, #{
            layouts => Layouts,
            middlewares => [
                fun(Req, _B) ->
                    %% Set a flash, then halt with a redirect: over a WS navigate this
                    %% flash rides the socket to /login (no Set-Cookie leg) and is
                    %% shown there -- the flash-over-navigate round-trip end-to-end.
                    Req1 = arizona_req:put_flash(Req, error, <<"Please sign in first.">>),
                    {halt, arizona_req:redirect(Req1, <<"/login">>)}
                end
            ]
        }},
        {live, <<"/drainable">>, arizona_drainable, #{
            bindings => #{drain_mode => stop},
            layouts => Layouts
        }},
        {get, <<"/_test/drain">>, arizona_drain_admin, #{}},
        %% arizona_js:fetch e2e -- a form posts to the controller (sets an HttpOnly
        %% cookie, broadcasts to the live view via pubsub, returns an effect); no reload.
        {live, <<"/fetch-account">>, arizona_fetch_account, #{
            bindings => #{title => <<"Account">>},
            layouts => Layouts
        }},
        {post, <<"/fetch-account/submit">>, arizona_fetch_account_controller, #{}},
        %% arizona_js:fetch + push_event -- the controller's response push_event re-renders
        %% the submitting view via handle_event (no pubsub); for refreshing only that view.
        {live, <<"/fetch-push">>, arizona_fetch_push, #{
            bindings => #{title => <<"Push">>},
            layouts => Layouts
        }},
        {post, <<"/fetch-push/submit">>, arizona_fetch_push_controller, #{}},
        %% arizona_js:fetch on_error -- the controller replies 500 with no effects body, so
        %% the client runs the fetch's on_error commands and fires arizona:fetch-error.
        {live, <<"/fetch-error">>, arizona_fetch_error, #{
            bindings => #{title => <<"Error">>},
            layouts => Layouts
        }},
        {post, <<"/fetch-error/submit">>, arizona_fetch_error_controller, #{}},
        %% arizona_session write loop -- a fetch posts to the controller, which rotates
        %% the encrypted az_session cookie and push_events the new name; fetch_session on
        %% the page route reads the persisted name back from the cookie on the next load.
        {live, <<"/session">>, arizona_session_view, #{
            bindings => #{title => <<"Session">>},
            layouts => Layouts,
            middlewares => [{arizona_middleware, fetch_session}]
        }},
        {post, <<"/session/save">>, arizona_session_controller, #{}},
        %% Native (JSON) view -- no layouts (native has no HTTP page); the first
        %% frame is mount_and_render over the WebSocket.
        {live, <<"/native/counter">>, arizona_native_counter_demo, #{}},
        {live, <<"/native/list">>, arizona_native_list, #{
            bindings => #{
                items => [
                    #{id => ~"1", text => ~"One"},
                    #{id => ~"2", text => ~"Two"},
                    #{id => ~"3", text => ~"Three"}
                ]
            }
        }},
        {live, <<"/native/tabs">>, arizona_native_tabs, #{}},
        {live, <<"/native/ticker">>, arizona_native_ticker, #{}},
        {live, <<"/native/multi">>, arizona_native_multi, #{}},
        {live, <<"/native/nested">>, arizona_native_nested, #{}},
        {live, <<"/native/removable">>, arizona_native_removable, #{}},
        {live, <<"/native/menu">>, arizona_native_menu, #{}},
        {ws, <<"/ws">>, #{}},
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}},
        %% Dev-introspection MCP server (see arizona_dev_mcp); the helper defaults
        %% to session mode so eval is a persistent REPL. A CLI agent connects with
        %% no Origin; origins stays opt-in.
        arizona_dev_mcp:route(<<"/mcp">>)
    ].

stop() ->
    arizona_roadrunner_server:stop(http).
