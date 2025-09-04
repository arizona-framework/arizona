#!/bin/bash

# Build assets files
if ! npm run build; then
    exit 1
fi

# Compile using the test profile
if ! rebar3 as test compile; then
    exit 1
fi

# Use erl directly instead of rebar3 shell because we need the -noshell option
# for automated/CI environments where interactive shell is not available
exec erl \
    -pa "_build/test/lib/arizona/ebin" \
    -pa "_build/test/lib/arizona/test" \
    -pa "_build/test/lib/fs/ebin" \
    -pa "_build/test/lib/cowboy/ebin" \
    -pa "_build/test/lib/cowlib/ebin" \
    -pa "_build/test/lib/ranch/ebin" \
    -sname arizona \
    -setcookie framework \
    ${ERLANG_EXTRA_ARGS} \
    -eval "
{ok, _} = application:ensure_all_started(arizona),
Routes = [
    {view, ~\"/realtime\", arizona_realtime_view, #{}},
    {view, ~\"/counter\", arizona_counter_view, #{}},
    {view, ~\"/todo\", arizona_todo_app_view, #{}},
    {view, ~\"/datagrid\", arizona_datagrid_view, #{}},
    {view, ~\"/modal\", arizona_modal_view, #{}},
    {view, ~\"/\", blog_home_view, #{title => ~\"My Arizona Blog\"}},
    {view, ~\"/about\", blog_about_view, #{title => ~\"About Me\"}},
    % In a real implementation, you might load post data from files or database
    {view, ~\"/post/:post_id\", blog_post_view, #{
        ~\"hello-world\" => #{
            title => ~\"Hello World\",
            content => ~\"Welcome to my first blog post!\"
        },
        ~\"arizona-static\" => #{
            title => ~\"Arizona Static Site Generation\",
            content => ~\"How to build static sites with Arizona framework.\"
        }
    }},
    {view, ~\"/presence\", arizona_presence_view, #{}},
    {controller, ~\"/api/presence/[:action]\", arizona_presence_controller, #{}},
    {websocket, ~\"/live\"},
    {asset, ~\"/assets\", {priv_dir, arizona, ~\"static/assets\"}}
],
{ok, _ClockPid} = arizona_clock_server:start_link(),
{ok, _PresencePid} = arizona_presence_server:start_link(),
case arizona:start(#{
    server => #{
        enabled => true,
        transport_opts => [{port, 8080}],
        routes => Routes
    },
    reloader => #{
        enabled => true,
        rules => [
            #{
                directories => [\"src\", \"test/support/e2e\"],
                patterns => [\".*\\\\.erl$\"],
                callback => fun(Files) ->
                    Cmd = \"rebar3 as test compile\",
                    ok = io:format(\"~n$ ~s~n\", [Cmd]),
                    try
                        CompileResult = os:cmd(Cmd, #{exception_on_failure => true}),
                        ok = io:format(\"~ts\", [CompileResult]),
                        ErlangFiles = [F || F <- Files, filename:extension(F) =:= \".erl\"],
                        lists:foreach(fun(FilePath) ->
                            {ok, Cwd} = file:get_cwd(),
                            RelativePath = case string:prefix(FilePath, Cwd ++ \"/\") of
                                nomatch -> FilePath;
                                Suffix -> Suffix
                            end,
                            io:format(\"===> Reloading ~s~n\", [RelativePath]),
                            BaseName = filename:basename(FilePath, \".erl\"),
                            try
                                Module = list_to_existing_atom(BaseName),
                                case code:is_loaded(Module) of
                                    {file, _} ->
                                        logger:info(\"Reloading module: ~p\", [Module]),
                                        % code:purge/1 removes old version from memory
                                        % Required before loading new version to avoid conflicts
                                        code:purge(Module),
                                        % code:load_file/1 loads the newly compiled .beam file
                                        % This makes the updated code active in the running system
                                        code:load_file(Module);
                                    false -> ok
                                end
                            catch _:_ -> ok
                            end
                        end, ErlangFiles)
                    catch
                        error:{command_failed, ResultBeforeFailure, _ExitCode} ->
                            io:format(\"~ts~n\", [ResultBeforeFailure])
                    end
                end
            },
            #{
                directories => [\"assets/js\"],
                patterns => [\".*\\\\.js$\"],
                callback => fun(_Files) -> os:cmd(\"npm run build\") end
            }
        ]
    }
}) of
    ok ->
        io:format(\"Arizona test server started on port 8080~n\"),
        receive stop -> ok end;
    Error ->
        io:format(\"Failed to start server: ~p~n\", [Error]),
        halt(1)
end.
"
