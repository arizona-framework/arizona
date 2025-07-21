-module(arizona_request).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([from_cowboy/1]).
-export([get_bindings/1]).
-export([get_params/1]).
-export([get_cookies/1]).
-export([get_method/1]).
-export([get_path/1]).
-export([get_headers/1]).
-export([get_body/1]).
-export([get_raw_request/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([new/1]).
-ignore_xref([get_bindings/1]).
-ignore_xref([get_params/1]).
-ignore_xref([get_cookies/1]).
-ignore_xref([get_method/1]).
-ignore_xref([get_path/1]).
-ignore_xref([get_headers/1]).
-ignore_xref([get_body/1]).
-ignore_xref([get_raw_request/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([request/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal request record structure
-record(request, {
    method :: method(),
    path :: path(),
    bindings :: bindings() | undefined,
    params :: params() | undefined,
    cookies :: cookies() | undefined,
    headers :: headers() | undefined,
    body :: body() | undefined,
    % For server-specific data
    raw :: raw() | undefined
}).

-opaque request() :: #request{}.
-nominal method() :: binary().
-nominal path() :: binary().
-nominal bindings() :: #{atom() => term()}.
-nominal params() :: [{binary(), binary() | true}].
-nominal cookies() :: [{binary(), binary()}].
-nominal headers() :: #{binary() => iodata()}.
-nominal body() :: binary().
-nominal raw() :: {cowboy_req, cowboy_req:req()}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(Opts) -> Request when
    Opts :: map(),
    Request :: request().
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

-spec from_cowboy(CowboyReq) -> Request when
    CowboyReq :: cowboy_req:req(),
    Request :: request().
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

-spec get_method(Request) -> Method when
    Request :: request(),
    Method :: method().
get_method(#request{method = Method}) ->
    Method.

-spec get_path(Request) -> Path when
    Request :: request(),
    Path :: path().
get_path(#request{path = Path}) ->
    Path.

-spec get_bindings(Request) -> {Bindings, Request1} when
    Request :: request(),
    Bindings :: bindings(),
    Request1 :: request().
get_bindings(#request{bindings = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Bindings = cowboy_req:bindings(CowboyReq),
    Req1 = Req#request{bindings = Bindings},
    {Bindings, Req1};
get_bindings(#request{bindings = Bindings} = Req) when Bindings =/= undefined ->
    {Bindings, Req}.

-spec get_params(Request) -> {Params, Request1} when
    Request :: request(),
    Params :: params(),
    Request1 :: request().
get_params(#request{params = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Params = cowboy_req:parse_qs(CowboyReq),
    Req1 = Req#request{params = Params},
    {Params, Req1};
get_params(#request{params = Params} = Req) when Params =/= undefined ->
    {Params, Req}.

-spec get_cookies(Request) -> {Cookies, Request1} when
    Request :: request(),
    Cookies :: cookies(),
    Request1 :: request().
get_cookies(#request{cookies = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Cookies = cowboy_req:parse_cookies(CowboyReq),
    Req1 = Req#request{cookies = Cookies},
    {Cookies, Req1};
get_cookies(#request{cookies = Cookies} = Req) when Cookies =/= undefined ->
    {Cookies, Req}.

-spec get_headers(Request) -> {Headers, Request1} when
    Request :: request(),
    Headers :: headers(),
    Request1 :: request().
get_headers(#request{headers = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Headers = cowboy_req:headers(CowboyReq),
    Req1 = Req#request{headers = Headers},
    {Headers, Req1};
get_headers(#request{headers = Headers} = Req) when Headers =/= undefined ->
    {Headers, Req}.

-spec get_body(Request) -> {Body, Request1} when
    Request :: request(),
    Body :: body(),
    Request1 :: request().
get_body(#request{body = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    {ok, Body, CowboyReq1} = cowboy_req:read_body(CowboyReq),
    Req1 = Req#request{body = Body, raw = {cowboy_req, CowboyReq1}},
    {Body, Req1};
get_body(#request{body = Body} = Req) when Body =/= undefined ->
    {Body, Req};
get_body(#request{} = Req) ->
    {~"", Req}.

-spec get_raw_request(Request) -> RawRequest when
    Request :: request(),
    RawRequest :: raw() | undefined.
get_raw_request(#request{raw = Raw}) ->
    Raw.
