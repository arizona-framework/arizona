-module(arizona_js_test_reloader).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(_Files, _Options) ->
    try
        Cmd = "npm run build",
        ok = io:format("~n$ ~s~n", [Cmd]),
        CompileResult = os:cmd(Cmd, #{exception_on_failure => true}),
        ok = io:format("~ts", [CompileResult]),
        arizona_pubsub:broadcast(~"arizona:reload", js)
    catch
        error:{command_failed, ResultBeforeFailure, _ExitCode} ->
            io:format("~ts~n", [ResultBeforeFailure])
    end.
