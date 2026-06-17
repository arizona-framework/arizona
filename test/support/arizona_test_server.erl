-module(arizona_test_server).
-export([start/0, stop/0, port/0, routes/0]).

start() ->
    {ok, _} = arizona_roadrunner_server:start(http, #{
        transport_opts => [{port, port()}],
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
        {live, <<"/pip">>, arizona_pip, #{
            bindings => #{id => ~"pip_demo", title => <<"PiP">>},
            layouts => Layouts
        }},
        {live, <<"/transitions">>, arizona_transitions, #{
            bindings => #{title => <<"Transitions">>},
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
        {live, <<"/login">>, arizona_login, #{layouts => Layouts}},
        {live, <<"/protected">>, arizona_navigate_halt, #{
            layouts => Layouts,
            middlewares => [
                fun(Req, _B) -> {halt, arizona_req:redirect(Req, <<"/login">>)} end
            ]
        }},
        {live, <<"/drainable">>, arizona_drainable, #{
            bindings => #{drain_mode => stop},
            layouts => Layouts
        }},
        {controller, <<"/_test/drain">>, arizona_drain_admin, #{}},
        %% arizona_js:fetch e2e -- a form posts to the controller (sets an HttpOnly
        %% cookie, broadcasts to the live view via pubsub, returns an effect); no reload.
        {live, <<"/fetch-account">>, arizona_fetch_account, #{
            bindings => #{title => <<"Account">>},
            layouts => Layouts
        }},
        {controller, <<"/fetch-account/submit">>, arizona_fetch_account_controller, #{}},
        %% arizona_js:fetch + push_event -- the controller's response push_event re-renders
        %% the submitting view via handle_event (no pubsub); for refreshing only that view.
        {live, <<"/fetch-push">>, arizona_fetch_push, #{
            bindings => #{title => <<"Push">>},
            layouts => Layouts
        }},
        {controller, <<"/fetch-push/submit">>, arizona_fetch_push_controller, #{}},
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

port() ->
    case os:getenv("PORT") of
        false -> 4040;
        Val -> list_to_integer(Val)
    end.
