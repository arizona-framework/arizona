-module(arizona_cowboy_req).
-moduledoc """
Cowboy adapter for the `arizona_req` and `arizona_adapter` behaviours.

Implements the `arizona_req` callbacks by delegating each to the
matching `cowboy_req` primitive. `new/1` wraps a raw cowboy request
in an `arizona_req:request()` with `method` and `path` eagerly
populated and everything else lazy-loaded on first access.

Also implements `arizona_adapter:resolve_route/3` so the same module
that knows how to parse a request also knows how to resolve a navigate
target through the cowboy router. `arizona_socket` recovers the adapter
from the request itself via `arizona_req:adapter/1` -- no separate
adapter argument is threaded through.
""".

-behaviour(arizona_req).
-behaviour(arizona_adapter).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).

%% --------------------------------------------------------------------
%% arizona_req callbacks
%% --------------------------------------------------------------------

-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

%% --------------------------------------------------------------------
%% arizona_adapter callbacks
%% --------------------------------------------------------------------

-export([resolve_route/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([resolve_route/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Wraps a cowboy request in an `arizona_req:request()` with `method`
and `path` eagerly populated.
""".
-spec new(CowboyReq) -> arizona_req:request() when
    CowboyReq :: cowboy_req:req().
new(CowboyReq) ->
    arizona_req:new(?MODULE, CowboyReq, #{
        method => cowboy_req:method(CowboyReq),
        path => cowboy_req:path(CowboyReq)
    }).

%% --------------------------------------------------------------------
%% arizona_req behaviour callbacks
%% --------------------------------------------------------------------

-spec parse_bindings(CowboyReq) -> arizona_req:bindings() when
    CowboyReq :: cowboy_req:req().
parse_bindings(CowboyReq) ->
    cowboy_req:bindings(CowboyReq).

-spec parse_params(CowboyReq) -> arizona_req:params() when
    CowboyReq :: cowboy_req:req().
parse_params(CowboyReq) ->
    cowboy_req:parse_qs(CowboyReq).

-spec parse_cookies(CowboyReq) -> arizona_req:cookies() when
    CowboyReq :: cowboy_req:req().
parse_cookies(CowboyReq) ->
    cowboy_req:parse_cookies(CowboyReq).

-spec parse_headers(CowboyReq) -> arizona_req:headers() when
    CowboyReq :: cowboy_req:req().
parse_headers(CowboyReq) ->
    cowboy_req:headers(CowboyReq).

-spec read_body(CowboyReq) -> {arizona_req:body(), CowboyReq} when
    CowboyReq :: cowboy_req:req().
read_body(CowboyReq) ->
    {ok, Body, CowboyReq1} = cowboy_req:read_body(CowboyReq),
    {Body, CowboyReq1}.

%% --------------------------------------------------------------------
%% arizona_adapter callbacks
%% --------------------------------------------------------------------

-doc """
Resolves a path to `{Handler, RouteOpts, Request}` by running the
Cowboy router.

Returns the handler's static route options (including its `bindings`
config) untouched, plus a navigate-scoped `arizona_req:request()`
synthesized from the stored upgrade cowboy req with the new path and
query applied.
""".
-spec resolve_route(Path, Qs, Req) -> {Handler, RouteOpts, ArzReq} when
    Path :: arizona_adapter:path(),
    Qs :: arizona_adapter:qs(),
    Req :: cowboy_req:req(),
    Handler :: module(),
    RouteOpts :: arizona_adapter:route_opts(),
    ArzReq :: arizona_req:request().
resolve_route(Path, Qs, Req) ->
    NavReq = Req#{path => Path, qs => Qs},
    {ok, ResolvedReq, Env} = cowboy_router:execute(
        NavReq,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    Opts = maps:get(handler_opts, Env),
    ArzReq = new(ResolvedReq),
    {maps:get(handler, Opts), maps:without([handler], Opts), ArzReq}.
