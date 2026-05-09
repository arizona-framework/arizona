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

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            did_you_mean_atom_typo_returns_match,
            did_you_mean_binary_typo_returns_match,
            did_you_mean_no_match_returns_undefined,
            did_you_mean_empty_candidates_returns_undefined,
            did_you_mean_exact_match_returns_undefined,
            did_you_mean_short_target_strict_threshold
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
