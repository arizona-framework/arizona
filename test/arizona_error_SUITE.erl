-module(arizona_error_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    did_you_mean_atom_typo_returns_match/1,
    did_you_mean_binary_typo_returns_match/1,
    did_you_mean_no_match_returns_undefined/1,
    did_you_mean_empty_candidates_returns_undefined/1,
    did_you_mean_exact_match_returns_undefined/1,
    did_you_mean_short_target_strict_threshold/1
]).
-export([raise_or_propagate_retags_at_the_callback/1]).
-export([raise_or_propagate_propagates_other_arity_of_same_name/1]).
-export([raise_or_propagate_propagates_inner_failure/1]).

%% Probe callbacks -- the failing "user callbacks" the retag tests crash on.
-export([probe_handle/1]).
-export([probe_inner_undef/1]).

all() ->
    [{group, tests}, {group, raise_or_propagate}].

groups() ->
    [
        {tests, [parallel], [
            did_you_mean_atom_typo_returns_match,
            did_you_mean_binary_typo_returns_match,
            did_you_mean_no_match_returns_undefined,
            did_you_mean_empty_candidates_returns_undefined,
            did_you_mean_exact_match_returns_undefined,
            did_you_mean_short_target_strict_threshold
        ]},
        {raise_or_propagate, [parallel], [
            raise_or_propagate_retags_at_the_callback,
            raise_or_propagate_propagates_other_arity_of_same_name,
            raise_or_propagate_propagates_inner_failure
        ]}
    ].

%% --- did_you_mean/2 ------------------------------------------------------

did_you_mean_atom_typo_returns_match(Config) when is_list(Config) ->
    %% One-character transposition against an atom candidate list.
    ?assertEqual(title, arizona_error:did_you_mean(tilte, [id, title, count])).

did_you_mean_binary_typo_returns_match(Config) when is_list(Config) ->
    %% Same with binary keys.
    ?assertEqual(
        <<"alpha">>,
        arizona_error:did_you_mean(<<"alpah">>, [<<"alpha">>, <<"beta">>])
    ).

did_you_mean_no_match_returns_undefined(Config) when is_list(Config) ->
    %% Nothing close enough -- distance exceeds the threshold.
    ?assertEqual(undefined, arizona_error:did_you_mean(xyzzy, [id, title, count])).

did_you_mean_empty_candidates_returns_undefined(Config) when is_list(Config) ->
    ?assertEqual(undefined, arizona_error:did_you_mean(foo, [])).

did_you_mean_exact_match_returns_undefined(Config) when is_list(Config) ->
    %% An exact match means the target is in the list -- the caller
    %% wouldn't be asking for a suggestion in the first place. Returns
    %% undefined so the caller doesn't suggest "did you mean X" for X.
    ?assertEqual(undefined, arizona_error:did_you_mean(title, [id, title, count])).

did_you_mean_short_target_strict_threshold(Config) when is_list(Config) ->
    %% Threshold scales with target length. A 2-char target should not
    %% match a 5-char candidate even though Levenshtein is small (5)
    %% relative to the 5-char candidate -- it's too large for the 2-char
    %% target.
    ?assertEqual(undefined, arizona_error:did_you_mean(ab, [hello])).

%% --- raise_or_propagate/6 ------------------------------------------------

raise_or_propagate_retags_at_the_callback(Config) when is_list(Config) ->
    %% The callback itself is missing: the top frame names it at the arity the
    %% dispatcher asked for, so the failure is re-tagged into the friendly reason.
    ST = crash_stacktrace(fun() -> (self_mod()):probe_missing(arg) end),
    ?assertError(
        {missing_action, ?MODULE, probe_missing, 1},
        retag(ST, {?MODULE, probe_missing, 1})
    ).

raise_or_propagate_propagates_other_arity_of_same_name(Config) when is_list(Config) ->
    %% `probe_handle/1` exists and ran; it crashed calling an undefined
    %% `probe_handle/2`. Matching on name alone reported the *running* callback as
    %% missing ("does not export probe_handle/1"), pointing at the wrong function
    %% entirely -- the arity is what tells the two apart.
    ST = crash_stacktrace(fun() -> ?MODULE:probe_handle(arg) end),
    ?assertMatch([{?MODULE, probe_handle, [arg, extra], _} | _], ST),
    ?assertError(undef, retag(ST, {?MODULE, probe_handle, 1})).

raise_or_propagate_propagates_inner_failure(Config) when is_list(Config) ->
    %% A failure from a different function inside the callback body propagates
    %% untouched, stacktrace included.
    ST = crash_stacktrace(fun() -> ?MODULE:probe_inner_undef(arg) end),
    ?assertError(undef, retag(ST, {?MODULE, probe_inner_undef, 1})).

%% --- Helpers -------------------------------------------------------------

%% Runs the dispatcher-side retag with a fixed reason/args, so a test only varies
%% the stacktrace and the callback MFA being claimed.
retag(ST, MFA) ->
    {Mod, Fn, Arity} = MFA,
    arizona_error:raise_or_propagate(
        undef, ST, MFA, {missing_action, Mod, Fn, Arity}, [Mod, Fn], ?MODULE
    ).

%% The real stacktrace of a real `undef`, not a synthesized one -- the frame shape
%% (args list vs arity integer) is exactly what the match depends on.
crash_stacktrace(Fun) ->
    try Fun() of
        _ -> ct:fail(expected_undef)
    catch
        error:undef:ST -> ST
    end.

%% Exists, runs, and crashes calling an undefined function of the same name and a
%% different arity -- the shape that used to be misreported.
probe_handle(Arg) ->
    (self_mod()):probe_handle(Arg, extra).

%% Exists, runs, and crashes calling an undefined function of another name.
probe_inner_undef(Arg) ->
    (self_mod()):probe_helper(Arg).

%% This module, behind a call the compiler cannot resolve -- the probes above
%% deliberately call functions that do not exist, which a literal `?MODULE:` call
%% would (correctly) warn about at compile time.
self_mod() ->
    ?MODULE.
