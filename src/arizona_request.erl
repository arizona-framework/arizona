-module(arizona_request).

%% API
-export([new/1, from_cowboy/1]).
-export([get_bindings/1, get_params/1, get_cookies/1]).
-export([get_method/1, get_path/1, get_headers/1]).
-export([get_body/1, get_raw_request/1]).

%% Types
-record(request, {
    method :: binary(),
    path :: binary(),
    bindings :: #{atom() => binary()} | undefined,
    params :: [{binary(), binary()}] | undefined,
    cookies :: [{binary(), binary()}] | undefined,
    headers :: #{binary() => binary()} | undefined,
    body :: binary() | undefined,
    % For server-specific data
    raw :: {cowboy_req, cowboy_req:req()} | undefined
}).

-opaque request() :: #request{}.
-export_type([request/0]).

%% API Functions

%% Create a new arizona request
-spec new(map()) -> request().
new(Opts) when is_map(Opts) ->
    #request{
        method = maps:get(method, Opts, ~"GET"),
        path = maps:get(path, Opts, ~"/"),
        bindings = maps:get(bindings, Opts, #{}),
        params = maps:get(params, Opts, []),
        cookies = maps:get(cookies, Opts, []),
        headers = maps:get(headers, Opts, #{}),
        body = maps:get(body, Opts, undefined),
        raw = maps:get(raw, Opts, undefined)
    }.

%% Create arizona request from cowboy request
-spec from_cowboy(cowboy_req:req()) -> request().
from_cowboy(CowboyReq) ->
    #request{
        method = cowboy_req:method(CowboyReq),
        path = cowboy_req:path(CowboyReq),
        % Lazy load
        bindings = undefined,
        % Lazy load
        params = undefined,
        % Lazy load
        cookies = undefined,
        % Lazy load
        headers = undefined,
        % Lazy load
        body = undefined,
        raw = {cowboy_req, CowboyReq}
    }.

%% Get URL path bindings (e.g., #{user_id => <<"123">>})
-spec get_bindings(request()) -> {#{atom() => binary()}, request()}.
get_bindings(#request{bindings = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Bindings = cowboy_req:bindings(CowboyReq),
    Req1 = Req#request{bindings = Bindings},
    {Bindings, Req1};
get_bindings(#request{bindings = Bindings} = Req) ->
    {Bindings, Req}.

%% Get query parameters (e.g., [{<<"tab">>, <<"account">>}])
-spec get_params(request()) -> {[{binary(), binary()}], request()}.
get_params(#request{params = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Params = cowboy_req:parse_qs(CowboyReq),
    Req1 = Req#request{params = Params},
    {Params, Req1};
get_params(#request{params = Params} = Req) ->
    {Params, Req}.

%% Get cookies (e.g., [{<<"session_id">>, <<"abc123">>}])
-spec get_cookies(request()) -> {[{binary(), binary()}], request()}.
get_cookies(#request{cookies = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Cookies = cowboy_req:parse_cookies(CowboyReq),
    Req1 = Req#request{cookies = Cookies},
    {Cookies, Req1};
get_cookies(#request{cookies = Cookies} = Req) ->
    {Cookies, Req}.

%% Get HTTP method
-spec get_method(request()) -> binary().
get_method(#request{method = Method}) ->
    Method.

%% Get request path
-spec get_path(request()) -> binary().
get_path(#request{path = Path}) ->
    Path.

%% Get request headers
-spec get_headers(request()) -> {#{binary() => binary()}, request()}.
get_headers(#request{headers = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Headers = cowboy_req:headers(CowboyReq),
    Req1 = Req#request{headers = Headers},
    {Headers, Req1};
get_headers(#request{headers = Headers} = Req) ->
    {Headers, Req}.

%% Get request body (lazy loaded for cowboy)
-spec get_body(request()) -> {binary(), request()}.
get_body(#request{body = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    {ok, Body, CowboyReq1} = cowboy_req:read_body(CowboyReq),
    Req1 = Req#request{body = Body, raw = {cowboy_req, CowboyReq1}},
    {Body, Req1};
get_body(#request{body = Body} = Req) when Body =/= undefined ->
    {Body, Req};
get_body(#request{} = Req) ->
    {<<>>, Req}.

%% Get the raw server-specific request object
-spec get_raw_request(request()) -> {cowboy_req, cowboy_req:req()} | undefined.
get_raw_request(#request{raw = Raw}) ->
    Raw.
