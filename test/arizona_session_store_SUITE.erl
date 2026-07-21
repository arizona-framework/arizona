-module(arizona_session_store_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([end_per_testcase/2]).

-export([ets_put_get_round_trips/1]).
-export([ets_get_missing_is_no_session/1]).
-export([ets_delete_removes/1]).
-export([ets_get_lazy_expires/1]).
-export([ets_sweep_reaps_expired/1]).
-export([req_put_session_persists_to_store/1]).
-export([req_session_id_available_after_write/1]).
-export([req_read_session_from_store/1]).
-export([req_put_session_merges_onto_stored/1]).
-export([req_clear_session_deletes_from_store/1]).
-export([req_login_rotation_mints_fresh_id/1]).
-export([req_clear_then_put_without_read_mints_fresh_id/1]).
-export([req_revocation_empties_session/1]).
-export([req_revocation_records_no_error/1]).
-export([req_store_failure_is_observable/1]).
-export([req_tampered_cookie_records_no_error/1]).
-export([req_cookie_mode_records_no_error/1]).
-export([req_store_failure_survives_put_session/1]).
-export([req_store_failure_undefined_reason_is_tagged/1]).

-define(SECRET, ~"test-secret-key-0123456789abcdef").

%% Sequential: sets the `secret_key`/`session_store` app env and owns the ETS store
%% process for the whole suite.
all() ->
    [
        ets_put_get_round_trips,
        ets_get_missing_is_no_session,
        ets_delete_removes,
        ets_get_lazy_expires,
        ets_sweep_reaps_expired,
        req_put_session_persists_to_store,
        req_session_id_available_after_write,
        req_read_session_from_store,
        req_put_session_merges_onto_stored,
        req_clear_session_deletes_from_store,
        req_login_rotation_mints_fresh_id,
        req_clear_then_put_without_read_mints_fresh_id,
        req_revocation_empties_session,
        req_revocation_records_no_error,
        req_store_failure_is_observable,
        req_tampered_cookie_records_no_error,
        req_cookie_mode_records_no_error,
        req_store_failure_survives_put_session,
        req_store_failure_undefined_reason_is_tagged
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

%% Restore the ETS store after each case: a case that swaps in a different backend (e.g.
%% the failing store) or tunes its reason must not leak into the next sequential case,
%% even on failure.
end_per_testcase(_Case, _Config) ->
    application:set_env(arizona, session_store, arizona_session_store_ets),
    application:unset_env(arizona, test_failing_store_reason),
    ok.

%% --------------------------------------------------------------------
%% ETS store impl
%% --------------------------------------------------------------------

ets_put_get_round_trips(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"u" => ~"1"}, 3600),
    ?assertEqual({ok, #{~"u" => ~"1"}}, arizona_session_store_ets:get(Id)).

ets_get_missing_is_no_session(Config) when is_list(Config) ->
    ?assertEqual(no_session, arizona_session_store_ets:get(~"no-such-id")).

ets_delete_removes(Config) when is_list(Config) ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"u" => ~"1"}, 3600),
    ok = arizona_session_store_ets:delete(Id),
    ?assertEqual(no_session, arizona_session_store_ets:get(Id)).

ets_get_lazy_expires(Config) when is_list(Config) ->
    %% A row past its deadline reads as error and is dropped on access.
    Past = erlang:system_time(second) - 1,
    true = ets:insert(arizona_session_store_ets, {~"expired", #{~"a" => 1}, Past}),
    ?assertEqual(no_session, arizona_session_store_ets:get(~"expired")),
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
    ?assertEqual(no_session, arizona_session_store_ets:get(Id)).

%% Session fixation defense: the login rotation pattern (read the incoming id,
%% clear_session, then put_session the new identity) must land the authenticated
%% session under a FRESH id -- never the incoming (possibly attacker-planted) one
%% -- and drop the pre-login entry.
req_login_rotation_mints_fresh_id(Config) when is_list(Config) ->
    IncomingId = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(IncomingId, #{}, 3600),
    Req0 = arizona_req_test_adapter:new(#{
        cookies => [{~"az_session", arizona_session:encode_id(IncomingId)}]
    }),
    {_, Req1} = arizona_req:read_session(Req0),
    Req2 = arizona_req:clear_session(Req1),
    Req3 = arizona_req:put_session(Req2, user_id, ~"42"),
    [{~"az_session", Value, _}] = arizona_req:resp_cookies(Req3),
    {ok, OutId} = arizona_session:decode_id(Value),
    %% Rotated to a fresh id, and the cookie carries it.
    ?assertNotEqual(IncomingId, OutId),
    ?assertEqual(OutId, arizona_req:session_id(Req3)),
    %% The authenticated session lives under the fresh id; the incoming id is gone.
    ?assertEqual({ok, #{~"user_id" => ~"42"}}, arizona_session_store_ets:get(OutId)),
    ?assertEqual(no_session, arizona_session_store_ets:get(IncomingId)).

%% Regression (crash): clear_session then put_session with NO prior read used to
%% crash at response flush ({badkey, session_id}) because no id was minted. It now
%% mints a fresh id and persists the session.
req_clear_then_put_without_read_mints_fresh_id(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{cookies => []}),
    Req1 = arizona_req:clear_session(Req0),
    Req2 = arizona_req:put_session(Req1, user_id, ~"42"),
    [{~"az_session", Value, _}] = arizona_req:resp_cookies(Req2),
    {ok, OutId} = arizona_session:decode_id(Value),
    ?assertEqual(OutId, arizona_req:session_id(Req2)),
    ?assertEqual({ok, #{~"user_id" => ~"42"}}, arizona_session_store_ets:get(OutId)).

req_revocation_empties_session(Config) when is_list(Config) ->
    %% Revoking the store entry out of band logs the session out on the next read.
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"7"}, 3600),
    Cookie = arizona_session:encode_id(Id),
    ok = arizona_session_store_ets:delete(Id),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {Session, _Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{}, Session).

req_revocation_records_no_error(Config) when is_list(Config) ->
    %% Genuine absence (a revoked/expired id) is not a failure: session_error stays undefined,
    %% so an app can tell a signed-out request from a store outage.
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"7"}, 3600),
    Cookie = arizona_session:encode_id(Id),
    ok = arizona_session_store_ets:delete(Id),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{}, Session),
    ?assertEqual(undefined, arizona_req:session_error(Req1)).

req_store_failure_is_observable(Config) when is_list(Config) ->
    %% A store *failure* (backend unreachable) reads as an empty session -- a failed read
    %% must never grant access -- but records the reason so an app can observe the outage,
    %% distinct from a signed-out request. end_per_testcase restores the ETS store.
    application:set_env(arizona, session_store, arizona_failing_session_store),
    Id = arizona_session:new_id(),
    Cookie = arizona_session:encode_id(Id),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{}, Session),
    ?assertEqual(undefined, arizona_req:session_id(Req1)),
    ?assertEqual({error, store_unreachable}, arizona_req:session_error(Req1)),
    %% Public-API proof the failed read grants nothing: get_session misses too.
    ?assertEqual(error, arizona_req:get_session(Req1, user_id)),
    ?assertEqual(default, arizona_req:get_session(Req1, user_id, default)).

req_tampered_cookie_records_no_error(Config) when is_list(Config) ->
    %% A tampered/forged signed id fails decode before the store is ever consulted: it is a
    %% signed-out request (client-supplied bad credential), NOT an outage -- session_error
    %% stays undefined. This is the central invariant: a bad cookie is absence, not failure.
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", ~"not-a-valid-id"}]}),
    {Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{}, Session),
    ?assertEqual(undefined, arizona_req:session_error(Req1)).

req_cookie_mode_records_no_error(Config) when is_list(Config) ->
    %% Cookie mode (no store) never sets session_error: the read path that could fail only
    %% exists in store mode.
    application:unset_env(arizona, session_store),
    Cookie = arizona_session:encode(#{~"user_id" => ~"7"}),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual(#{~"user_id" => ~"7"}, Session),
    ?assertEqual(undefined, arizona_req:session_error(Req1)).

req_store_failure_survives_put_session(Config) when is_list(Config) ->
    %% session_error records a fact about this request's read; a later write does not retract
    %% it (the read genuinely failed), and the write mints a fresh id since the failed read
    %% stashed none.
    application:set_env(arizona, session_store, arizona_failing_session_store),
    Cookie = arizona_session:encode_id(arizona_session:new_id()),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {_Session, Req1} = arizona_req:read_session(Req0),
    Req2 = arizona_req:put_session(Req1, user_id, ~"42"),
    ?assertEqual({error, store_unreachable}, arizona_req:session_error(Req2)),
    ?assert(is_binary(arizona_req:session_id(Req2))).

req_store_failure_undefined_reason_is_tagged(Config) when is_list(Config) ->
    %% The `{error, _}` wrapper keeps a real outage whose reason is the atom `undefined`
    %% distinct from the no-error sentinel -- the reason tagging exists to prevent.
    application:set_env(arizona, session_store, arizona_failing_session_store),
    application:set_env(arizona, test_failing_store_reason, undefined),
    Cookie = arizona_session:encode_id(arizona_session:new_id()),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Cookie}]}),
    {_Session, Req1} = arizona_req:read_session(Req0),
    ?assertEqual({error, undefined}, arizona_req:session_error(Req1)),
    ?assertNotEqual(undefined, arizona_req:session_error(Req1)).
