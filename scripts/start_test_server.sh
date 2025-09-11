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
maybe
    ok = application:set_env([{arizona, [
        {server, #{
            enabled => true,
            transport_opts => [{port, 8080}],
            routes => [
                {view, ~\"/realtime\", arizona_realtime_view, #{}, []},
                {view, ~\"/counter\", arizona_counter_view, #{}, []},
                {view, ~\"/todo\", arizona_todo_app_view, #{}, []},
                {view, ~\"/datagrid\", arizona_datagrid_view, #{}, []},
                {view, ~\"/modal\", arizona_modal_view, #{}, []},
                {view, ~\"/\", arizona_blog_home_view, #{title => ~\"My Arizona Blog\"}, []},
                {view, ~\"/about\", arizona_blog_about_view, #{title => ~\"About Me\"}, []},
                % In a real implementation, you might load post data from files or database
                {view, ~\"/post/:post_id\", arizona_blog_post_view, #{
                    ~\"hello-world\" => #{
                        title => ~\"Hello World\",
                        content => ~\"Welcome to my first blog post!\"
                    },
                    ~\"arizona-static\" => #{
                        title => ~\"Arizona Static Site Generation\",
                        content => ~\"How to build static sites with Arizona framework.\"
                    }
                }, []},
                {view, ~\"/presence\", arizona_presence_view, #{}, []},
                {controller, ~\"/api/presence/[:action]\", arizona_presence_controller, #{}, []},
                {websocket, ~\"/live\", #{}, []},
                {asset, ~\"/assets\", {priv_dir, arizona, ~\"static/assets\"}, []}
            ]
        }},
        {reloader, #{
            enabled => true,
            rules => [
                #{
                    handler => arizona_erl_test_reloader,
                    watcher => #{
                        directories => [\"src\", \"test/support/e2e\"],
                        patterns => [\".*\\\\.erl$\"],
                        debounce_ms => 100
                    }
                },
                #{
                    handler => arizona_js_test_reloader,
                    watcher => #{
                        directories => [\"assets/js\"],
                        patterns => [\".*\\\\.js$\"],
                        debounce_ms => 200
                    }
                }
            ]
        }}
    ]}]),
    {ok, _Started} ?= application:ensure_all_started(arizona),
    {ok, _ClockPid} ?= arizona_clock_server:start_link(),
    {ok, _PresencePid} ?= arizona_presence_server:start_link(),
    io:format(\"Arizona test server started on port 8080~n\"),
    receive stop -> ok end
else
    Error ->
        io:format(\"Failed to start server: ~p~n\", [Error]),
        halt(1)
end.
"
