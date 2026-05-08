-module(arizona_error_hint_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    closest_atom_typo_returns_match/1,
    closest_binary_typo_returns_match/1,
    closest_no_match_returns_undefined/1,
    closest_empty_candidates_returns_undefined/1,
    closest_exact_match_returns_undefined/1,
    closest_short_target_strict_threshold/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            closest_atom_typo_returns_match,
            closest_binary_typo_returns_match,
            closest_no_match_returns_undefined,
            closest_empty_candidates_returns_undefined,
            closest_exact_match_returns_undefined,
            closest_short_target_strict_threshold
        ]}
    ].

%% --- closest/2 -----------------------------------------------------------

closest_atom_typo_returns_match(Config) when is_list(Config) ->
    %% One-character transposition against an atom candidate list.
    ?assertEqual(title, arizona_error_hint:closest(tilte, [id, title, count])).

closest_binary_typo_returns_match(Config) when is_list(Config) ->
    %% Same with binary keys.
    ?assertEqual(
        <<"alpha">>,
        arizona_error_hint:closest(<<"alpah">>, [<<"alpha">>, <<"beta">>])
    ).

closest_no_match_returns_undefined(Config) when is_list(Config) ->
    %% Nothing close enough -- distance exceeds the threshold.
    ?assertEqual(undefined, arizona_error_hint:closest(xyzzy, [id, title, count])).

closest_empty_candidates_returns_undefined(Config) when is_list(Config) ->
    ?assertEqual(undefined, arizona_error_hint:closest(foo, [])).

closest_exact_match_returns_undefined(Config) when is_list(Config) ->
    %% An exact match means the target is in the list -- the caller
    %% wouldn't be asking for a suggestion in the first place. Returns
    %% undefined so the caller doesn't suggest "did you mean X" for X.
    ?assertEqual(undefined, arizona_error_hint:closest(title, [id, title, count])).

closest_short_target_strict_threshold(Config) when is_list(Config) ->
    %% Threshold scales with target length. A 2-char target should not
    %% match a 5-char candidate even though Levenshtein is small (5)
    %% relative to the 5-char candidate -- it's too large for the 2-char
    %% target.
    ?assertEqual(undefined, arizona_error_hint:closest(ab, [hello])).
