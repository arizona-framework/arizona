-module(arizona_test_server).
-export([start/0, stop/0, port/0]).

start() ->
    Layout = {arizona_layout, render},
    Routes = [
        {live, <<"/">>, arizona_page, #{
            bindings => #{title => <<"Welcome">>},
            layout => Layout
        }},
        {live, <<"/about">>, arizona_about, #{
            bindings => #{title => <<"About">>},
            layout => Layout
        }},
        {live, <<"/datatable">>, arizona_datatable, #{
            bindings => #{title => <<"DataTable">>},
            layout => Layout
        }},
        {live, <<"/mixed">>, arizona_mixed_children, #{
            bindings => #{title => <<"Mixed">>},
            layout => Layout
        }},
        {live, <<"/chat">>, arizona_chat, #{
            bindings => #{title => <<"Chat">>},
            layout => Layout
        }},
        {live, <<"/crashable">>, arizona_crashable, #{layout => Layout}},
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
