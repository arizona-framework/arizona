-module(arizona_error_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    did_you_mean_atom_typo_returns_match/1,
    did_you_mean_binary_typo_returns_match/1,
    did_you_mean_no_match_returns_undefined/1,
    did_you_mean_empty_candidates_returns_undefined/1,
    did_you_mean_exact_match_returns_undefined/1,
    did_you_mean_short_target_strict_threshold/1,
    did_you_mean_one_char_target_never_matches/1,
    did_you_mean_two_char_target_rejects_total_mismatch/1,
    did_you_mean_prefix_typo_still_matches/1
]).
-export([raise_or_propagate_retags_at_the_callback/1]).
-export([raise_or_propagate_propagates_other_arity_of_same_name/1]).
-export([raise_or_propagate_propagates_inner_failure/1]).
-export([format_title_compile_error/1]).
-export([format_title_arizona_loc/1]).
-export([format_title_prefers_template_module_frame/1]).
-export([format_title_all_framework_stack_uses_loc/1]).
-export([format_title_keeps_genuine_user_frame/1]).
-export([format_reason_compile_error/1]).
-export([format_reason_compile_error_empty/1]).
-export([format_reason_arizona_loc/1]).
-export([format_text_names_reason_and_frames/1]).
-export([format_text_compile_error/1]).

%% Probe callbacks -- the failing "user callbacks" the retag tests crash on.
-export([probe_handle/1]).
-export([probe_inner_undef/1]).

all() ->
    [{group, tests}, {group, raise_or_propagate}, {group, format}].

groups() ->
    [
        {tests, [parallel], [
            did_you_mean_atom_typo_returns_match,
            did_you_mean_binary_typo_returns_match,
            did_you_mean_no_match_returns_undefined,
            did_you_mean_empty_candidates_returns_undefined,
            did_you_mean_exact_match_returns_undefined,
            did_you_mean_short_target_strict_threshold,
            did_you_mean_one_char_target_never_matches,
            did_you_mean_two_char_target_rejects_total_mismatch,
            did_you_mean_prefix_typo_still_matches
        ]},
        {raise_or_propagate, [parallel], [
            raise_or_propagate_retags_at_the_callback,
            raise_or_propagate_propagates_other_arity_of_same_name,
            raise_or_propagate_propagates_inner_failure
        ]},
        {format, [parallel], [
            format_title_compile_error,
            format_title_arizona_loc,
            format_title_prefers_template_module_frame,
            format_title_all_framework_stack_uses_loc,
            format_title_keeps_genuine_user_frame,
            format_reason_compile_error,
            format_reason_compile_error_empty,
            format_reason_arizona_loc,
            format_text_names_reason_and_frames,
            format_text_compile_error
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

did_you_mean_one_char_target_never_matches(Config) when is_list(Config) ->
    %% A one-character key has no near miss: every other one-character key is
    %% 100% different. The old absolute floor of 2 swallowed the whole relative
    %% term for any key up to 11 bytes, so `x` "meant" `y`.
    ?assertEqual(undefined, arizona_error:did_you_mean(x, [y])),
    ?assertEqual(undefined, arizona_error:did_you_mean(x, [y, z, a])).

did_you_mean_two_char_target_rejects_total_mismatch(Config) when is_list(Config) ->
    %% Same length, nothing in common -- distance 2 over a 2-char key. It used
    %% to suggest `xy` for `id`; at most half a short key may differ.
    ?assertEqual(undefined, arizona_error:did_you_mean(id, [xy])),
    %% One of two characters still counts as a near miss.
    ?assertEqual(is, arizona_error:did_you_mean(id, [is])).

did_you_mean_prefix_typo_still_matches(Config) when is_list(Config) ->
    %% The behavior worth keeping: a real key that is a two-character extension
    %% of what was typed is still suggested.
    ?assertEqual(counter, arizona_error:did_you_mean(count, [id, title, counter])).

%% --- raise_or_propagate/6 ------------------------------------------------

raise_or_propagate_retags_at_the_callback(Config) when is_list(Config) ->
    %% The callback itself is missing: the top frame names it at the arity the
    %% dispatcher asked for, so the failure is re-tagged into the friendly reason.
    ST = crash_stacktrace(fun() -> (self_mod()):probe_missing(arg) end),
    ?assertMatch(
        {{missing_action, ?MODULE, probe_missing, 1}, _},
        retag(ST, {?MODULE, probe_missing, 1})
    ).

raise_or_propagate_propagates_other_arity_of_same_name(Config) when is_list(Config) ->
    %% `probe_handle/1` exists and ran; it crashed calling an undefined
    %% `probe_handle/2`. Matching on name alone reported the *running* callback as
    %% missing ("does not export probe_handle/1"), pointing at the wrong function
    %% entirely -- the arity is what tells the two apart.
    ST = crash_stacktrace(fun() -> ?MODULE:probe_handle(arg) end),
    ?assertMatch([{?MODULE, probe_handle, [arg, extra], _} | _], ST),
    ?assertEqual({undef, ST}, retag(ST, {?MODULE, probe_handle, 1})).

raise_or_propagate_propagates_inner_failure(Config) when is_list(Config) ->
    %% A failure from a different function inside the callback body propagates
    %% untouched, original stacktrace included.
    ST = crash_stacktrace(fun() -> ?MODULE:probe_inner_undef(arg) end),
    ?assertEqual({undef, ST}, retag(ST, {?MODULE, probe_inner_undef, 1})).

%% --- Crash formatting ------------------------------------------------------

format_title_compile_error(Config) when is_list(Config) ->
    ?assertEqual(~"Compilation Error", arizona_error:format_title(error, {compile_error, []}, [])).

%% Empty stack -> the title falls back to `arizona_loc`'s captured location.
format_title_arizona_loc(Config) when is_list(Config) ->
    ?assertEqual(
        ~"my_mod:42", arizona_error:format_title(error, {arizona_loc, {my_mod, 42}, badarg}, [])
    ).

%% An app module named `arizona_*` is dropped by the name-prefix framework test,
%% so the generic walk finds no user frame. The loc names the template module, so
%% a frame from THAT module is the right answer.
format_title_prefers_template_module_frame(Config) when is_list(Config) ->
    Reason = {arizona_loc, {arizona_userapp, 7}, badarg},
    Stack = [
        {arizona_template, to_bin, 1, [{line, 400}]},
        {arizona_userapp, '-render/1-fun-0-', 1, [{line, 9}]}
    ],
    ?assertEqual(~"arizona_userapp:9", arizona_error:format_title(error, Reason, Stack)).

%% A value that fails AFTER its closure returned leaves no template frame at all;
%% the lexical loc is the only real answer.
format_title_all_framework_stack_uses_loc(Config) when is_list(Config) ->
    Reason = {arizona_loc, {my_page, 5}, {bad_template_value, #{}}},
    Stack = [
        {arizona_template, to_bin, 1, [{line, 447}]},
        {arizona_render, zip, 3, [{line, 253}]}
    ],
    ?assertEqual(~"my_page:5", arizona_error:format_title(error, Reason, Stack)).

%% A crash inside a helper module still names that module.
format_title_keeps_genuine_user_frame(Config) when is_list(Config) ->
    Reason = {arizona_loc, {my_page, 5}, badarg},
    Stack = [
        {my_helper, fmt, 1, [{line, 42}]},
        {my_page, '-render/1-fun-0-', 1, [{line, 5}]}
    ],
    ?assertEqual(~"my_helper:42", arizona_error:format_title(error, Reason, Stack)).

format_reason_compile_error(Config) when is_list(Config) ->
    Errors = [{"src/foo.erl", [{{10, 1}, erl_lint, {unused_var, 'X'}}]}],
    ?assertEqual(
        ~"src/foo.erl:\n  line 10: variable 'X' is unused",
        arizona_error:format_reason(error, {compile_error, Errors}, [])
    ).

%% No diagnostics recovered -- the body must still say where they are.
format_reason_compile_error_empty(Config) when is_list(Config) ->
    Bin = arizona_error:format_reason(error, {compile_error, []}, []),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"console")).

%% The `arizona_loc` wrapper is stripped and the inner reason rendered by erl_error.
format_reason_arizona_loc(Config) when is_list(Config) ->
    ?assertEqual(
        ~"bad argument", arizona_error:format_reason(error, {arizona_loc, {m, 1}, badarg}, [])
    ).

format_text_names_reason_and_frames(Config) when is_list(Config) ->
    Stack = [
        {my_page, '-render/1-fun-0-', 1, [{file, "src/my_page.erl"}, {line, 9}]},
        {my_helper, fmt, [a], []}
    ],
    ?assertEqual(
        iolist_to_binary([
            ~"my_page:9\n\nbad argument\n\n",
            ~"  my_page:render/1 (anonymous fun) src/my_page.erl:9\n",
            ~"  my_helper:fmt/1\n"
        ]),
        arizona_error:format_text(error, {arizona_loc, {my_page, 9}, badarg}, Stack)
    ).

format_text_compile_error(Config) when is_list(Config) ->
    Errors = [{"src/foo.erl", [{{10, 1}, erl_lint, {unused_var, 'X'}}]}],
    ?assertEqual(
        ~"Compilation Error\n\nsrc/foo.erl:\n  line 10: variable 'X' is unused\n\n",
        arizona_error:format_text(error, {compile_error, Errors}, [])
    ).

%% --- Helpers -------------------------------------------------------------

%% Runs the dispatcher-side retag with a fixed reason/args, so a test only varies
%% the stacktrace and the callback MFA being claimed. The helper never returns
%% normally, so this reports what it raised, stacktrace included.
retag(ST, MFA) ->
    {Mod, Fn, Arity} = MFA,
    try
        arizona_error:raise_or_propagate(
            undef, ST, MFA, {missing_action, Mod, Fn, Arity}, [Mod, Fn], ?MODULE
        )
    catch
        error:Reason:RaisedST -> {Reason, RaisedST}
    end.

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
