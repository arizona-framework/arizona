-module(arizona_clock_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Start the timer immediately
    erlang:send_after(1000, self(), tick),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    % Get current time and broadcast to all subscribers
    NewTime = format_time(erlang:timestamp()),
    arizona_pubsub:broadcast(~"time_update", #{~"time" => NewTime}),

    % Schedule next tick
    erlang:send_after(1000, self(), tick),

    {noreply, State}.

format_time({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(
        {MegaSecs, Secs, MicroSecs}
    ),
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC", [
            Year, Month, Day, Hour, Min, Sec
        ])
    ).
