-module(arizona_middleware).
-moduledoc ~""""
Middleware behavior definition for request processing components.

Defines the behavior for middleware components that can intercept and modify
HTTP requests before they reach the main handler. Middlewares can perform
authentication, authorization, CORS handling, request modification, and
custom response generation across all Arizona route types.

## Behavior Callbacks

- `execute/2` - Process the request with configuration options

## Request Flow Control

Middleware execution results:
- `{continue, CowboyReq1}` - Continue to next middleware or handler
- `{halt, CowboyReq1}` - Stop processing (response already sent)

## Data Passing Between Middlewares

Middlewares can share data using `cowboy_req` metadata:
- `cowboy_req:set_meta(Key, Value, Req)` - Store data
- `cowboy_req:meta(Key, Req, Default)` - Retrieve data

## Example Implementation

```erlang
-module(auth_middleware).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_middleware).
-export([execute/2]).

execute(Req, #{jwt_secret := Secret}) ->
    case cowboy_req:header(~"authorization", Req) of
        <<"Bearer ", Token/binary>> ->
            case validate_jwt(Token, Secret) of
                {ok, Claims} ->
                    Req1 = cowboy_req:set_meta(user_claims, Claims, Req),
                    {continue, Req1};
                {error, _} ->
                    Req1 = cowboy_req:reply(401, #{}, ~"Invalid token", Req),
                    {halt, Req1}
            end;
        undefined ->
            Req1 = cowboy_req:reply(302, #{~"location" => ~"/login"}, ~"", Req),
            {halt, Req1}
    end.

validate_jwt(_Token, _Secret) ->
    {ok, #{user_id => ~"123", role => ~"admin"}}.
```
"""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([process_middlewares/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([result/0]).
-export_type([opts/0]).
-export_type([middleware/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal result() ::
    {continue, CowboyReq :: cowboy_req:req()} | {halt, CowboyReq :: cowboy_req:req()}.
-nominal opts() :: map().
-nominal middleware() :: {Handler :: module(), Opts :: opts()}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback execute(CowboyReq, Opts) -> Result when
    CowboyReq :: cowboy_req:req(),
    Opts :: opts(),
    Result :: result().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Processes middleware chain sequentially.

Executes each middleware in the provided list with its configuration options.
Processing continues until all middlewares complete successfully or one
middleware halts the chain by returning `{halt, CowboyReq1}`.

Returns `{continue, CowboyReq1}` if all middlewares complete successfully,
or `{halt, CowboyReq1}` if any middleware terminates the chain early.
""".
-spec process_middlewares(Middlewares, CowboyReq) -> Result when
    Middlewares :: [middleware()],
    CowboyReq :: cowboy_req:req(),
    Result :: result().
process_middlewares([], CowboyReq) ->
    {continue, CowboyReq};
process_middlewares([{Handler, Opts} | Rest], CowboyReq) when is_atom(Handler), is_map(Opts) ->
    case call_execute_callback(Handler, CowboyReq, Opts) of
        {continue, CowboyReq1} ->
            process_middlewares(Rest, CowboyReq1);
        {halt, CowboyReq1} ->
            {halt, CowboyReq1}
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Executes a middleware's execute callback.
%%
%% Calls the Handler module's `execute/2` function with the cowboy request
%% and configuration options. Used by middleware processor to invoke
%% middleware components consistently.
-spec call_execute_callback(Handler, CowboyReq, Opts) -> Result when
    Handler :: module(),
    CowboyReq :: cowboy_req:req(),
    Opts :: opts(),
    Result :: result().
call_execute_callback(Handler, CowboyReq, Opts) when is_atom(Handler), is_map(Opts) ->
    apply(Handler, execute, [CowboyReq, Opts]).
