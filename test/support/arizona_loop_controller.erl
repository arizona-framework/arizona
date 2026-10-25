-module(arizona_loop_controller).
-moduledoc """
**TEST FIXTURE.** A `{loop, ...}` reply in the Server-Sent Events shape: the
action queues two ticks to its own process (the connection's, where the loop
runs) and each one is pushed as an event; the second ends the response. A
`notify` pid in the route `state` receives every message the loop is handed,
so a disconnect can be observed.
""".

-export([handle/2]).
-export([handle_info/3]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := State}) ->
    self() ! {tick, 1},
    self() ! {tick, 2},
    {{loop, 200, [{~"content-type", ~"text/event-stream"}], State}, Req}.

-spec handle_info(Info, Push, State) -> {ok, State} | {stop, State} when
    Info :: term(),
    Push :: arizona_controller:push_fun(),
    State :: map().
handle_info(Info, Push, State) ->
    ok = notify(State, Info),
    case Info of
        {tick, N} ->
            ok = Push([~"data: ", integer_to_binary(N), ~"\n\n"]),
            case N of
                2 -> {stop, State};
                _ -> {ok, State}
            end;
        _ ->
            {ok, State}
    end.

notify(#{notify := Pid}, Info) ->
    Pid ! {?MODULE, Info},
    ok;
notify(#{}, _Info) ->
    ok.
