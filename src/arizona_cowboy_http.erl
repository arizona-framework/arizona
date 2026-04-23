-module(arizona_cowboy_http).
-moduledoc """
Cowboy HTTP handler that runs an Arizona handler for the initial
server-side render.

Wired up by `arizona_cowboy_router` for every Arizona route. The
handler delegates the full render pipeline (wrap cowboy req in an
`arizona_req:request()`, run middlewares, call
`arizona_render:render_view_to_iolist/3`, handle crashes + stashed
hot-reload compile errors) to `arizona_http:render/3`, and translates
its result into Cowboy's `{ok, Req, State}` reply shape.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([init/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Cowboy `init/2` callback. Renders the configured Arizona handler and
replies with HTML.
""".
-spec init(Req, State) -> {ok, Req1, State} when
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
init(Req, #{handler := H} = State) ->
    case arizona_http:render(H, Req, State) of
        {halt, RawReq} ->
            {ok, RawReq, State};
        {ok, Status, Body} ->
            reply(Status, Body, Req, State);
        {error, Status, Body} ->
            reply(Status, Body, Req, State)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

reply(Status, Body, Req, State) ->
    Req2 = cowboy_req:reply(
        Status,
        #{~"content-type" => ~"text/html"},
        Body,
        Req
    ),
    {ok, Req2, State}.
