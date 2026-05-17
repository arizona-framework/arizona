-module(arizona_roadrunner_reload).
-moduledoc """
Roadrunner long-poll handler that streams dev-mode reload events as
Server-Sent Events.

The browser connects to this endpoint via `EventSource` and
receives `reload` or `reload_css` events whenever
`arizona_reloader` broadcasts a change. The dev error page injects
a script that auto-reloads on the first event after a successful
recompile.

Subscribes to the `arizona_reloader` pubsub topic on `handle/1`
and forwards every matching message to the SSE stream from
`handle_info/3`.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).
-export([handle_info/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Opens a streaming SSE response and
joins the reloader pubsub topic.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    %% per-route state is wrapped under `arizona` so roadrunner's
    %% pipeline does not interpret arizona's opaque opts as its own
    %% middlewares.
    _ = roadrunner_req:state(Req),
    arizona_reloader:join(self()),
    Headers = [
        {~"content-type", ~"text/event-stream"},
        {~"cache-control", ~"no-cache"}
    ],
    {{loop, 200, Headers, undefined}, Req}.

-doc """
Roadrunner loop `handle_info/3` callback. Forwards
`arizona_reloader` messages as SSE events to the connected client.
""".
-spec handle_info(Info, Push, State) -> {ok, State} when
    Info :: term(),
    Push :: roadrunner_handler:push_fun(),
    State :: term().
handle_info({arizona_reloader, reload}, Push, State) ->
    _ = Push(~"event: reload\ndata: \n\n"),
    {ok, State};
handle_info({arizona_reloader, reload_css}, Push, State) ->
    _ = Push(~"event: reload_css\ndata: \n\n"),
    {ok, State};
handle_info(_Info, _Push, State) ->
    {ok, State}.
