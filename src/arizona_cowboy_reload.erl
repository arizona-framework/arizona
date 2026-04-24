-module(arizona_cowboy_reload).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-moduledoc """
Cowboy long-poll handler that streams dev-mode reload events as
Server-Sent Events.

The browser connects to this endpoint via `EventSource` and receives
`reload` or `reload_css` events whenever `arizona_reloader` broadcasts
a change. The dev error page injects a script that auto-reloads on
the first event after a successful recompile.

Subscribes to the `arizona_reloader` pubsub topic on `init/2` and
forwards every matching message to the SSE stream from `info/3`.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([info/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([init/2, info/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Cowboy `init/2` callback. Opens a streaming SSE response and joins
the reloader pubsub topic.
""".
-spec init(Req, State) -> {cowboy_loop, Req1, State} when
    Req :: cowboy_req:req(),
    State :: term(),
    Req1 :: cowboy_req:req().
init(Req0, State) ->
    Headers = #{
        ~"content-type" => ~"text/event-stream",
        ~"cache-control" => ~"no-cache"
    },
    Req = cowboy_req:stream_reply(200, Headers, Req0),
    arizona_reloader:join(self()),
    {cowboy_loop, Req, State}.

-doc """
Cowboy loop `info/3` callback. Forwards `arizona_reloader` messages
as SSE events to the connected client.
""".
-spec info(Info, Req, State) -> {ok, Req, State} when
    Info :: term(),
    Req :: cowboy_req:req(),
    State :: term().
info({arizona_reloader, reload}, Req, State) ->
    cowboy_req:stream_body(~"event: reload\ndata: \n\n", nofin, Req),
    {ok, Req, State};
info({arizona_reloader, reload_css}, Req, State) ->
    cowboy_req:stream_body(~"event: reload_css\ndata: \n\n", nofin, Req),
    {ok, Req, State};
info(_Info, Req, State) ->
    {ok, Req, State}.

-ifdef(TEST).

%% The streaming branches are covered end-to-end by
%% `arizona_cowboy_ws_SUITE:reload_endpoint_streams_event`; here we just
%% exercise the unknown-info fallthrough, which otherwise has no caller.
info_ignores_unknown_test() ->
    ?assertEqual({ok, my_req, my_state}, info(some_random_msg, my_req, my_state)).

-endif.
