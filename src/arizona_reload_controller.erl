-module(arizona_reload_controller).
-moduledoc """
The dev hot-reload endpoint (the `{reload, Path, Opts}` route) as a controller,
so every `arizona_server` adapter that can send a `loop` reply serves it.

The browser connects with `EventSource` and gets a `reload` or `reload_css`
Server-Sent Event whenever `arizona_reloader` broadcasts one; the dev error page
injects the script that reloads on the first event after a successful
recompile. The action joins the reloader's pubsub topic and holds the response
open as a loop; `handle_info/3` writes each broadcast as an event.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/2]).
-export([handle_info/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Called by the controller dispatch a `{reload, ...}` route compiles to.
-ignore_xref([handle/2]).
-ignore_xref([handle_info/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Opens the event stream and joins the reloader topic.".
-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    ok = arizona_reloader:join(self()),
    Headers = [{~"content-type", ~"text/event-stream"}, {~"cache-control", ~"no-cache"}],
    {{loop, 200, Headers, undefined}, Req}.

-doc "Writes each reloader broadcast as a Server-Sent Event.".
-spec handle_info(Info, Push, State) -> {ok, State} when
    Info :: term(),
    Push :: arizona_controller:push_fun(),
    State :: undefined.
handle_info({arizona_reloader, reload}, Push, State) ->
    _Sent = Push(~"event: reload\ndata: \n\n"),
    {ok, State};
handle_info({arizona_reloader, reload_css}, Push, State) ->
    _Sent = Push(~"event: reload_css\ndata: \n\n"),
    {ok, State};
handle_info(_Info, _Push, State) ->
    {ok, State}.
