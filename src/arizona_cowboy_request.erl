-module(arizona_cowboy_request).
-behavior(arizona_request).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).

%% --------------------------------------------------------------------
%% Behaviour (arizona_request) exports
%% --------------------------------------------------------------------

-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(CowboyRequest) -> Request when
    CowboyRequest :: cowboy_req:req(),
    Request :: arizona_request:request().
new(CowboyRequest) ->
    arizona_request:new(?MODULE, CowboyRequest, #{
        method => cowboy_req:method(CowboyRequest),
        path => cowboy_req:path(CowboyRequest),
        % Lazy load
        bindings => undefined,
        % Lazy load
        params => undefined,
        % Lazy load
        cookies => undefined,
        % Lazy load
        headers => undefined,
        % Lazy load
        body => undefined
    }).

%% --------------------------------------------------------------------
%% Behaviour (arizona_request) callbacks
%% --------------------------------------------------------------------

-spec parse_bindings(CowboyRequest) -> Bindings when
    CowboyRequest :: cowboy_req:req(),
    Bindings :: arizona_request:bindings().
parse_bindings(CowboyRequest) ->
    cowboy_req:bindings(CowboyRequest).

-spec parse_params(CowboyRequest) -> Params when
    CowboyRequest :: cowboy_req:req(),
    Params :: arizona_request:params().
parse_params(CowboyRequest) ->
    cowboy_req:parse_qs(CowboyRequest).

-spec parse_cookies(CowboyRequest) -> Cookies when
    CowboyRequest :: cowboy_req:req(),
    Cookies :: arizona_request:cookies().
parse_cookies(CowboyRequest) ->
    cowboy_req:parse_cookies(CowboyRequest).

-spec parse_headers(CowboyRequest) -> Headers when
    CowboyRequest :: cowboy_req:req(),
    Headers :: arizona_request:headers().
parse_headers(CowboyRequest) ->
    cowboy_req:headers(CowboyRequest).

-spec read_body(CowboyRequest) -> {Body, UpdatedCowboyRequest} when
    CowboyRequest :: cowboy_req:req(),
    Body :: arizona_request:body(),
    UpdatedCowboyRequest :: cowboy_req:req().
read_body(CowboyRequest) ->
    {ok, Body, CowboyRequest1} = cowboy_req:read_body(CowboyRequest),
    {Body, CowboyRequest1}.
