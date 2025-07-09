#!/bin/bash

rebar3 as test shell --eval '
Routes = [
    {live, ~"/test/counter", test_counter_live},
    {live, ~"/test/todo", test_todo_app_live},
    {live_websocket, ~"/live/websocket"},
    {static, ~"/assets", {priv_dir, arizona, ~"static/assets"}}
],
{ok, _} = arizona_server:start(#{port => 8080, routes => Routes}),
io:format("Arizona test server started on port 8080~n").
'
