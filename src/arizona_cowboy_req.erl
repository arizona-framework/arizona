-module(arizona_cowboy_req).
-export([apply_middlewares/3]).
-ifdef(TEST).
-export([test_cont_middleware/2]).
-endif.

-type req() :: cowboy_req:req().
-type middleware() ::
    fun((req(), arizona_stateful:bindings()) -> middleware_result()) | {module(), atom()}.
-type middleware_result() :: {cont, req(), arizona_stateful:bindings()} | {halt, req()}.
-export_type([middleware/0, middleware_result/0]).

-spec apply_middlewares([middleware()], req(), arizona_stateful:bindings()) -> middleware_result().
apply_middlewares([], Req, Bindings) ->
    {cont, Req, Bindings};
apply_middlewares([H | Rest], Req, Bindings) ->
    case call(H, Req, Bindings) of
        {cont, Req1, B1} -> apply_middlewares(Rest, Req1, B1);
        {halt, _Req1} = Halt -> Halt
    end.

call({Mod, Fun}, Req, Bindings) -> Mod:Fun(Req, Bindings);
call(Fun, Req, Bindings) -> Fun(Req, Bindings).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
