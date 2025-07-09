#!/bin/bash

exec rebar3 as test shell --apps arizona --eval "
application:ensure_all_started(arizona),
Routes = [
    {live, ~\"/test/counter\", arizona_counter_live},
    {live, ~\"/test/todo\", arizona_todo_app_live},
    {live_websocket, ~\"/live/websocket\"},
    {static, ~\"/assets\", {priv_dir, arizona, ~\"static/assets\"}}
],
case arizona_server:start(#{port => 8080, routes => Routes}) of
    {ok, _} -> 
        io:format(\"Arizona test server started on port 8080~n\"),
        receive stop -> ok end;
    Error -> 
        io:format(\"Failed to start server: ~p~n\", [Error]),
        halt(1)
end.
"
