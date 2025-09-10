-module(arizona_erl_test_reloader).
-behaviour(arizona_reloader).
-export([reload/1]).

reload(Files) ->
    try
        Cmd = "rebar3 as test compile",
        ok = io:format("~n$ ~s~n", [Cmd]),
        CompileResult = os:cmd(Cmd, #{exception_on_failure => true}),
        ok = io:format("~ts", [CompileResult]),
        ErlangFiles = [F || F <- Files, filename:extension(F) =:= ".erl"],
        lists:foreach(
            fun(FilePath) ->
                {ok, Cwd} = file:get_cwd(),
                RelativePath =
                    case string:prefix(FilePath, Cwd ++ "/") of
                        nomatch -> FilePath;
                        Suffix -> Suffix
                    end,
                io:format("===> Reloading ~s~n", [RelativePath]),
                BaseName = filename:basename(FilePath, ".erl"),
                try
                    Module = list_to_existing_atom(BaseName),
                    case code:is_loaded(Module) of
                        {file, _} ->
                            logger:info("Reloading module: ~p", [Module]),
                            % code:purge/1 removes old version from memory
                            % Required before loading new version to avoid conflicts
                            code:purge(Module),
                            % code:load_file/1 loads the newly compiled .beam file
                            % This makes the updated code active in the running system
                            code:load_file(Module);
                        false ->
                            ok
                    end
                catch
                    _:_ -> ok
                end
            end,
            ErlangFiles
        )
    catch
        error:{command_failed, ResultBeforeFailure, _ExitCode} ->
            io:format("~ts~n", [ResultBeforeFailure])
    end.
