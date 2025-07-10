#!/bin/bash

# Use erl directly for CI environment instead of rebar3 shell
exec erl \
    -pa "_build/test/lib/arizona/ebin" \
    -pa "_build/test/lib/arizona/test" \
    -pa "_build/test/lib/cowboy/ebin" \
    -pa "_build/test/lib/cowlib/ebin" \
    -pa "_build/test/lib/ranch/ebin" \
    -noshell -eval "
{ok, _} = application:ensure_all_started(arizona),
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