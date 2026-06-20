-module(arizona_session_store_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([ets_put_get_round_trips/1]).
-export([ets_get_missing_is_error/1]).
-export([ets_delete_removes/1]).
-export([ets_get_lazy_expires/1]).
-export([ets_sweep_reaps_expired/1]).
-export([req_put_session_persists_to_store/1]).
-export([req_session_id_available_after_write/1]).
-export([req_read_session_from_store/1]).
-export([req_put_session_merges_onto_stored/1]).
-export([req_clear_session_deletes_from_store/1]).
-export([req_revocation_empties_session/1]).

-define(SECRET, ~"test-secret-key-0123456789abcdef").

%% Sequential: sets the `secret_key`/`session_store` app env and owns the ETS store
%% process for the whole suite.
all() ->
    [
        ets_put_get_round_trips,
        ets_get_missing_is_error,
        ets_delete_removes,
        ets_get_lazy_expires,
        ets_sweep_reaps_expired,
        req_put_session_persists_to_store,
        req_session_id_available_after_write,
        req_read_session_from_store,
        req_put_session_merges_onto_stored,
        req_clear_session_deletes_from_store,
        req_revocation_empties_session
    ].

init_per_suite(Config) ->
    application:set_env(arizona, secret_key, ?SECRET),
    application:set_env(arizona, session_store, arizona_session_store_ets),
    {ok, Pid} = arizona_session_store_ets:start_link(),
    %% Unlink so the store (and its ETS table) outlives the init_per_suite process and
    %% is reachable from the per-testcase processes.
    true = unlink(Pid),
    [{store, Pid} | Config].

end_per_suite(Config) ->
    ok = gen_server:stop(proplists:get_value(store, Config)),
    application:unset_env(arizona, session_store),
    application:unset_env(arizona, secret_key),
    ok.

%% --------------------------------------------------------------------
%% ETS store impl
%% --------------------------------------------------------------------

ets_put_get_round_trips(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"u" => ~"1"}, 3600),
    ?assertEqual({ok, #{~"u" => ~"1"}}, arizona_session_store_ets:get(Id)).

ets_get_missing_is_error(Config) when is_list(Config) ->
    ?assertEqual(error, arizona_session_store_ets:get(~"no-such-id")).

ets_delete_removes(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"u" => ~"1"}, 3600),
    ok = arizona_session_store_ets:delete(Id),
    ?assertEqual(error, arizona_session_store_ets:get(Id)).

ets_get_lazy_expires(Config) when is_list(Config) ->
    %% A row past its deadline reads as error and is dropped on access.
    Past = erlang:system_time(second) - 1,
    true = ets:insert(arizona_session_store_ets, {~"expired", #{~"a" => 1}, Past}),
    ?assertEqual(error, arizona_session_store_ets:get(~"expired")),
    ?assertEqual([], ets:lookup(arizona_session_store_ets, ~"expired")).

ets_sweep_reaps_expired(Config) when is_list(Config) ->
    Past = erlang:system_time(second) - 1,
    true = ets:insert(arizona_session_store_ets, {~"stale", #{}, Past}),
    arizona_session_store_ets ! sweep,
    %% A sync call drains the mailbox past the (FIFO) sweep message.
    ok = gen_server:call(arizona_session_store_ets, sync),
    ?assertEqual([], ets:lookup(arizona_session_store_ets, ~"stale")).

%% --------------------------------------------------------------------
%% arizona_req in store mode
%% --------------------------------------------------------------------

req_put_session_persists_to_store(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{cookies => []}),
    Req1 = arizona_req:put_session(Req0, user_id, ~"42"),
    %% The cookie carries the signed id; the store holds the map (put on response).
    [{~"az_session", Value, _Opts}] = arizona_req:resp_cookies(Req1),
    {ok, Id} = arizona_session:decode_id(Value),
    ?assertEqual({ok, #{~"user_id" => ~"42"}}, arizona_session_store_ets:get(Id)).

req_session_id_available_after_write(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{cookies => []}),
    Req1 = arizona_req:put_session(Req0, user_id, ~"42"),
    %% The id is minted eagerly on the write, so an app can record it for revocation.
    Id = arizona_req:session_id(Req1),
    ?assert(is_binary(Id)),
    [{~"az_session", Value, _}] = arizona_req:resp_cookies(Req1),
    ?assertEqual({ok, Id}, arizona_session:decode_id(Value)).

req_read_session_from_store(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"7"}, 3600),
    Req0 = arizona_req_test_adapter:new(#{
        cookies => [{~"az_session", arizona_session:encode_id(Id)}]
    }),
    {Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{~"user_id" => ~"7"}, Session),
    ?assertEqual(Id, arizona_req:session_id(Req1)).

req_put_session_merges_onto_stored(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"a" => 1}, 3600),
    Req0 = arizona_req_test_adapter:new(#{
        cookies => [{~"az_session", arizona_session:encode_id(Id)}]
    }),
    Req1 = arizona_req:put_session(Req0, b, 2),
    [{~"az_session", Value, _}] = arizona_req:resp_cookies(Req1),
    %% Same id reused; the store holds the merged map.
    ?assertEqual({ok, Id}, arizona_session:decode_id(Value)),
    ?assertEqual({ok, #{~"a" => 1, ~"b" => 2}}, arizona_session_store_ets:get(Id)).

req_clear_session_deletes_from_store(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"7"}, 3600),
    Req0 = arizona_req_test_adapter:new(#{
        cookies => [{~"az_session", arizona_session:encode_id(Id)}]
    }),
    {_, Req1} = arizona_req:read_session(Req0),
    Req2 = arizona_req:clear_session(Req1),
    [{~"az_session", <<>>, Opts}] = arizona_req:resp_cookies(Req2),
    ?assertEqual(0, maps:get(max_age, Opts)),
    ?assertEqual(error, arizona_session_store_ets:get(Id)).

req_revocation_empties_session(Config) when is_list(Config) ->
    %% Revoking the store entry out of band logs the session out on the next read.
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"7"}, 3600),
    Cookie = arizona_session:encode_id(Id),
    ok = arizona_session_store_ets:delete(Id),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {Session, _Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{}, Session).
