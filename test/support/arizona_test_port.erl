-module(arizona_test_port).
-moduledoc false.

%% A free TCP port for a test listener: bind an ephemeral port (0), read it
%% back, release it. The `Base + unique_integer([positive, monotonic]) rem N`
%% scheme this replaces was deterministic across VM restarts -- the counter
%% restarts at 1 in every fresh VM, so the first listener of every run landed
%% on the same port. That collided with a prior run's listener still in
%% TIME_WAIT, and with any parallel checkout of this repo running its own
%% suites (`eaddrinuse` in init_per_suite, which then cascades into unrelated
%% suites through the half-started application).
%%
%% There is a race in principle -- the port is free when returned, not when the
%% listener binds -- but the OS hands out ephemeral ports round-robin across
%% its range, so a re-collision inside that window is far less likely than the
%% deterministic reuse it replaces.

-export([pick/0]).

-ignore_xref([pick/0]).

%% The highest port in the WHATWG "bad ports" list (fetch spec). A browser --
%% and the MCP SDK client the conformance suite drives -- refuses to connect to
%% any of them, so a suite that speaks to one of those clients cannot use a
%% port at or below this. Linux's default ephemeral range (32768-60999) sits
%% far above it; the guard only matters on a host configured with an unusually
%% low range.
-define(HIGHEST_BAD_PORT, 10080).

-spec pick() -> inet:port_number().
pick() ->
    {ok, Sock} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Sock),
    ok = gen_tcp:close(Sock),
    case Port > ?HIGHEST_BAD_PORT of
        true -> Port;
        false -> pick()
    end.
