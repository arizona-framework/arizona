-module(arizona_loop_controller).
-moduledoc """
**TEST FIXTURE.** A `{loop, ...}` reply in the Server-Sent Events shape: the
action queues two ticks to its own process (the connection's, where the loop
runs) and each one is pushed as an event; the second ends the response. A
`notify` pid in the route `state` receives every message the loop is handed,
so a disconnect can be observed.
""".

-export([handle/2]).
-export([write_then_loop/2]).
-export([hold/2]).
-export([linked/2]).
-export([handle_info/3]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := State}) ->
    self() ! {tick, 1},
    self() ! {tick, 2},
    {{loop, 200, [{~"content-type", ~"text/event-stream"}], State}, Req}.

%% Holds the response open, pushing nothing, until the client goes away.
-spec hold(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
hold(Req, #{state := State}) ->
    {{loop, 200, [{~"content-type", ~"text/event-stream"}], State}, Req}.

%% Holds the response open with two processes linked to the loop, reported to
%% the `notify` pid with the loop's own; each exits with the reason it is sent
%% in `{exit, Reason}`.
-spec linked(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
linked(Req, #{state := State}) ->
    Links = [spawn_link(fun exit_on_request/0) || _ <- [1, 2]],
    ok = notify(State, {linked, self(), Links}),
    {{loop, 200, [{~"content-type", ~"text/event-stream"}], State}, Req}.

exit_on_request() ->
    receive
        {exit, Reason} -> exit(Reason)
    after 10000 -> ok
    end.

%% Writes the session, then asks for a loop: an adapter without loops must
%% refuse it before that write commits.
-spec write_then_loop(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
write_then_loop(Req, _Ctx) ->
    {{loop, 200, [], #{}}, arizona_req:put_session(Req, ~"k", ~"v")}.

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
