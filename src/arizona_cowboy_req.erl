-module(arizona_cowboy_req).
-moduledoc """
Cowboy adapter for the `arizona_req` abstraction.

Implements the `arizona_req` behaviour by delegating each callback to
the matching `cowboy_req` primitive. `new/1` wraps a raw cowboy request
in an `arizona_req:request()` with `method` and `path` eagerly
populated and everything else lazy-loaded on first access.
""".

-behaviour(arizona_req).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).

%% --------------------------------------------------------------------
%% arizona_req adapter callbacks
%% --------------------------------------------------------------------

-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

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
