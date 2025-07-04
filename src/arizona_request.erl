-module(arizona_request).

%% API
-export([new/1, from_cowboy/1]).
-export([get_bindings/1, get_params/1, get_cookies/1]).
-export([get_method/1, get_path/1, get_headers/1]).
-export([get_body/1, get_raw_request/1]).

%% Types
-record(arizona_request, {
    method :: binary(),
    path :: binary(),
    bindings :: #{atom() => binary()},
    params :: [{binary(), binary()}],
    cookies :: [{binary(), binary()}],
    headers :: #{binary() => binary()},
    body :: binary() | undefined,
    % For server-specific data
    raw :: term()
}).

-type request() :: #arizona_request{}.
-export_type([request/0]).

%% API Functions

%% Create a new arizona request
-spec new(map()) -> request().
new(#{} = Opts) ->
    #arizona_request{
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
    #arizona_request{
        method = cowboy_req:method(CowboyReq),
        path = cowboy_req:path(CowboyReq),
        bindings = cowboy_req:bindings(CowboyReq),
        params = cowboy_req:parse_qs(CowboyReq),
        cookies = cowboy_req:parse_cookies(CowboyReq),
        headers = cowboy_req:headers(CowboyReq),
        % Lazy load if needed
        body = undefined,
        raw = CowboyReq
    }.

%% Get URL path bindings (e.g., #{user_id => <<"123">>})
-spec get_bindings(request()) -> #{atom() => binary()}.
get_bindings(#arizona_request{bindings = Bindings}) ->
    Bindings.

%% Get query parameters (e.g., [{<<"tab">>, <<"account">>}])
-spec get_params(request()) -> [{binary(), binary()}].
get_params(#arizona_request{params = Params}) ->
    Params.

%% Get cookies (e.g., [{<<"session_id">>, <<"abc123">>}])
-spec get_cookies(request()) -> [{binary(), binary()}].
get_cookies(#arizona_request{cookies = Cookies}) ->
    Cookies.

%% Get HTTP method
-spec get_method(request()) -> binary().
get_method(#arizona_request{method = Method}) ->
    Method.

%% Get request path
-spec get_path(request()) -> binary().
get_path(#arizona_request{path = Path}) ->
    Path.

%% Get request headers
-spec get_headers(request()) -> #{binary() => binary()}.
get_headers(#arizona_request{headers = Headers}) ->
    Headers.

%% Get request body (lazy loaded for cowboy)
-spec get_body(request()) -> binary().
get_body(#arizona_request{body = Body}) when Body =/= undefined ->
    Body;
get_body(#arizona_request{raw = CowboyReq}) when CowboyReq =/= undefined ->
    {ok, Body, _Req2} = cowboy_req:read_body(CowboyReq),
    Body;
get_body(#arizona_request{}) ->
    <<>>.

%% Get the raw server-specific request object
-spec get_raw_request(request()) -> term().
get_raw_request(#arizona_request{raw = Raw}) ->
    Raw.
