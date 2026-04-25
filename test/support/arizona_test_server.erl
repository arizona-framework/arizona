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
        {ws, <<"/ws">>, #{}},
        {asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}
    ],
    {ok, _} = arizona_cowboy_server:start(http, #{
        transport_opts => [{port, port()}],
        proto_opts => #{stream_handlers => [cowboy_compress_h, cowboy_stream_h]},
        routes => Routes
    }),
    ok.

stop() ->
    arizona_cowboy_server:stop(http).

port() ->
    case os:getenv("PORT") of
        false -> 4040;
        Val -> list_to_integer(Val)
    end.
