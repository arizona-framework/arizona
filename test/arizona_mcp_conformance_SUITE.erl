-module(arizona_mcp_conformance_SUITE).
-moduledoc """
Real-client conformance: boots an Arizona MCP server and drives it with the
official `@modelcontextprotocol/sdk` client (via `test/conformance/mcp_client.mjs`),
guarding the interop the Erlang wire tests cannot -- capability negotiation,
session-id threading, and response shapes as the real client expects them.

Skips gracefully when `node` or the SDK are unavailable (e.g. a pure-Erlang
CI lane), so it never produces a false failure.
""".
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([sdk_lifecycle/1]).

-define(LISTENER, arizona_mcp_conformance).

all() ->
    [sdk_lifecycle].

init_per_suite(Config) ->
    case os:find_executable("node") of
        false ->
            {skip, "node executable not found"};
        _ ->
            {ok, _} = application:ensure_all_started(arizona),
            {ok, _} = application:ensure_all_started(roadrunner),
            %% 15050+ is well clear of the WHATWG "bad ports" the SDK refuses.
            Port = 15050 + erlang:unique_integer([positive, monotonic]) rem 1000,
            %% page_size 2 forces multi-page lists, so the SDK driver exercises
            %% the cursor protocol end-to-end.
            Routes = [
                {mcp, ~"/mcp", arizona_mcp_test_server, #{sessions => true, page_size => 2}}
            ],
            {ok, _} = arizona_roadrunner_server:start(?LISTENER, #{
                transport_opts => [{port, Port}],
                routes => Routes
            }),
            [{port, Port}, {script, client_script()} | Config]
    end.

end_per_suite(_Config) ->
    _ = arizona_roadrunner_server:stop(?LISTENER),
    _ = application:stop(arizona),
    ok.

sdk_lifecycle(Config) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(?config(port, Config)) ++ "/mcp",
    Cmd = "node " ++ ?config(script, Config) ++ " " ++ Url ++ " 2>&1",
    Output = os:cmd(Cmd),
    ct:log("MCP SDK conformance:~n~ts", [Output]),
    case {string:find(Output, "RESULT: PASS"), string:find(Output, "RESULT: SKIP")} of
        {nomatch, nomatch} -> ct:fail("conformance failed:~n~ts", [Output]);
        {nomatch, _Skip} -> {skip, "SDK conformance skipped (see log)"};
        {_Pass, _} -> ok
    end.

%% Locate test/conformance/mcp_client.mjs from the CT cwd (the project root) or,
%% as a fallback, relative to the compiled app dir under _build.
client_script() ->
    Rel = filename:join(["test", "conformance", "mcp_client.mjs"]),
    FromLib = filename:join([code:lib_dir(arizona), "..", "..", "..", "..", Rel]),
    Candidates = [Rel, FromLib],
    case [filename:absname(P) || P <- Candidates, filelib:is_regular(P)] of
        [Path | _] -> Path;
        [] -> error({conformance_script_not_found, Candidates})
    end.
