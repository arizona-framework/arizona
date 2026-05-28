-module(arizona_test_server).
-export([start/0, stop/0, port/0]).

start() ->
    Layouts = [{arizona_layout, render}],
    Routes = [
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
        {live, <<"/crashable">>, arizona_crashable, #{layouts => Layouts}},
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
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}
    ],
    {ok, _} = arizona_roadrunner_server:start(http, #{
        transport_opts => [{port, port()}],
        routes => Routes
    }),
    ok.

stop() ->
    arizona_roadrunner_server:stop(http).

port() ->
    case os:getenv("PORT") of
        false -> 4040;
        Val -> list_to_integer(Val)
    end.
