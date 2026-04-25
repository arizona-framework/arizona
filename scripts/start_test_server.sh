#!/bin/bash

# Build JS assets
if ! npx vite build; then
    exit 1
fi

# Compile (test profile includes test/ modules)
if ! rebar3 as test compile; then
    exit 1
fi

# Start Erlang with the arizona application
exec erl \
    -pa "_build/test/lib/arizona/ebin" \
    -pa "_build/test/lib/arizona/test" \
    -pa "_build/test/lib/cowboy/ebin" \
    -pa "_build/test/lib/cowlib/ebin" \
    -pa "_build/test/lib/ranch/ebin" \
    ${ERLANG_EXTRA_ARGS} \
    -eval "
{ok, _} = application:ensure_all_started(cowboy),
{ok, _} = application:ensure_all_started(arizona),
ok = arizona_test_server:start(),
Port = os:getenv(\"PORT\", \"4040\"),
io:format(\"Arizona test server started on port ~s~n\", [Port]),
receive stop -> ok end.
"
