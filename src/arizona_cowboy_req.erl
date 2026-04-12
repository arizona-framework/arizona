-module(arizona_cowboy_req).
-moduledoc """
Middleware pipeline for Arizona Cowboy handlers.

Provides a tiny `apply_middlewares/3` runner that threads a Cowboy
`req` and a bindings map through a list of middleware steps. Each
middleware can either let the chain continue (`{cont, Req, Bindings}`)
or halt with a final response (`{halt, Req}`).

Used by `arizona_cowboy_http:init/2` to run any middlewares declared
on a route before the Arizona handler is invoked. Typical use cases:
auth checks, locale negotiation, request logging, redirect-on-condition.

## Middleware shape

```erlang
fun((cowboy_req:req(), Bindings) -> {cont, Req, Bindings} | {halt, Req})
%% or
{Module, Function}     %% same arity, called as Module:Function/2
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([apply_middlewares/3]).

-ifdef(TEST).
-export([test_cont_middleware/2]).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([middleware/0]).
-export_type([middleware_result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal req() :: cowboy_req:req().
-nominal middleware() ::
    fun((req(), arizona_stateful:bindings()) -> middleware_result()) | {module(), atom()}.
-nominal middleware_result() :: {cont, req(), arizona_stateful:bindings()} | {halt, req()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Runs the middleware list left to right, threading `Req` and `Bindings`
through each step. Stops on the first `{halt, Req}` and returns it.
""".
-spec apply_middlewares(Middlewares, Req, Bindings) -> middleware_result() when
    Middlewares :: [middleware()],
    Req :: req(),
    Bindings :: arizona_stateful:bindings().
apply_middlewares([], Req, Bindings) ->
    {cont, Req, Bindings};
apply_middlewares([H | Rest], Req, Bindings) ->
    case call(H, Req, Bindings) of
        {cont, Req1, B1} -> apply_middlewares(Rest, Req1, B1);
        {halt, _Req1} = Halt -> Halt
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

call({Mod, Fun}, Req, Bindings) -> Mod:Fun(Req, Bindings);
call(Fun, Req, Bindings) -> Fun(Req, Bindings).

-ifdef(TEST).

mock_req() ->
    #{
        method => ~"GET",
        version => 'HTTP/1.1',
        scheme => ~"http",
        host => ~"localhost",
        port => 8080,
        path => ~"/",
        qs => ~"",
        headers => #{},
        bindings => #{},
        has_body => false,
        body_length => undefined,
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        ref => test_ref,
        pid => self(),
        streamid => 1
    }.

empty_list_test() ->
    Req = mock_req(),
    ?assertEqual({cont, Req, #{}}, apply_middlewares([], Req, #{})).

fun_cont_test() ->
    Req = mock_req(),
    M = [fun(R, B) -> {cont, R, B#{x => 1}} end],
    ?assertEqual({cont, Req, #{x => 1}}, apply_middlewares(M, Req, #{})).

fun_halt_test() ->
    Req = mock_req(),
    M = [fun(R, _B) -> {halt, R} end],
    ?assertEqual({halt, Req}, apply_middlewares(M, Req, #{})).

mf_cont_test() ->
    Req = mock_req(),
    M = [{?MODULE, test_cont_middleware}],
    ?assertEqual({cont, Req, #{from_mf => true}}, apply_middlewares(M, Req, #{})).

pipeline_halt_stops_early_test() ->
    Req = mock_req(),
    M = [
        fun(R, _B) -> {halt, R} end,
        fun(_, _) -> error(should_not_reach) end
    ],
    ?assertEqual({halt, Req}, apply_middlewares(M, Req, #{})).

pipeline_threads_bindings_test() ->
    Req = mock_req(),
    M = [
        fun(R, B) -> {cont, R, B#{a => 1}} end,
        fun(R, B) -> {cont, R, B#{b => 2}} end
    ],
    ?assertEqual({cont, Req, #{a => 1, b => 2}}, apply_middlewares(M, Req, #{})).

test_cont_middleware(Req, Bindings) ->
    {cont, Req, Bindings#{from_mf => true}}.

-endif.
