#!/bin/bash

# Compile using the test profile
if ! rebar3 as test compile; then
    exit 1
fi

# Use erl directly instead of rebar3 shell because we need the -noshell option
# for automated/CI environments where interactive shell is not available
exec erl \
    -pa "_build/test/lib/arizona/ebin" \
    -pa "_build/test/lib/arizona/test" \
    -pa "_build/test/lib/cowboy/ebin" \
    -pa "_build/test/lib/cowlib/ebin" \
    -pa "_build/test/lib/ranch/ebin" \
    -sname arizona \
    -setcookie framework \
    -noshell -eval "
{ok, _} = application:ensure_all_started(arizona),
Routes = [
    {live, ~\"/realtime\", arizona_realtime_view},
    {live, ~\"/counter\", arizona_counter_view},
    {live, ~\"/todo\", arizona_todo_app_view},
    {live, ~\"/datagrid\", arizona_datagrid_view},
    {live, ~\"/modal\", arizona_modal_view},
    {live_websocket, ~\"/live/websocket\"},
    {static, ~\"/assets\", {priv_dir, arizona, ~\"static/assets\"}}
],
{ok, _ClockPid} = arizona_clock_server:start_link(),
case arizona_server:start(#{port => 8080, routes => Routes}) of
    {ok, _} ->
        io:format(\"Arizona test server started on port 8080~n\"),
        receive stop -> ok end;
    Error ->
        io:format(\"Failed to start server: ~p~n\", [Error]),
        halt(1)
end.
"
