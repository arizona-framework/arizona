-module(arizona_error).
-moduledoc """
Helpers for error reporting.

Two utilities consumed across the framework:

- `did_you_mean/2` -- "did you mean?" suggestion for `format_error/2`
  clauses that report a missing key. Used by
  `arizona_template:format_error/2` and `arizona_stream:format_error/2`.
- `raise_or_propagate/7` -- the catch-clause helper that lets a
  dispatcher re-tag a failure originating at the user's exact callback
  while propagating any other failure untouched. Used by every
  `arizona_handler:call_*` wrapper.
""".

-export([did_you_mean/2]).
-export([raise_or_propagate/7]).
-ignore_xref([did_you_mean/2]).
-ignore_xref([raise_or_propagate/7]).

-doc """
Returns the candidate closest to `Target` by Levenshtein edit distance, or
`undefined` when nothing is close enough. The threshold scales with the
target length so very short keys must match almost exactly.
""".
-spec did_you_mean(Target, Candidates) -> Match | undefined when
    Target :: term(),
    Candidates :: [term()],
    Match :: term().
did_you_mean(_Target, []) ->
    undefined;
did_you_mean(Target, Candidates) ->
    TargetBin = to_bin(Target),
    Threshold = max(2, byte_size(TargetBin) div 4),
    Scored = [{distance(TargetBin, to_bin(C)), C} || C <- Candidates],
    case lists:sort(Scored) of
        [{D, Match} | _] when D =< Threshold, D > 0 -> Match;
        _ -> undefined
    end.

-doc """
Catch-clause helper for dispatchers that wrap a user callback.

When the top stack frame is the user's exact callback (`Mod:Fn`), raises
`Reason` with an `error_info` annotation pointing at `FormatErrorMod` --
the module that exports the matching `format_error/2` clause. Otherwise
propagates `error:OrigReason` with the original stacktrace untouched.

The `FormatErrorMod` parameter exists because this helper lives in
`arizona_error` but is called from various dispatcher modules; the
`error_info` annotation must point at whichever module owns the format
clauses for `Reason`.
""".
-spec raise_or_propagate(OrigReason, Stacktrace, Mod, Fn, Reason, ErrorArgs, FormatErrorMod) ->
    no_return()
when
    OrigReason :: term(),
    Stacktrace :: [tuple()],
    Mod :: module(),
    Fn :: atom(),
    Reason :: term(),
    ErrorArgs :: [term()],
    FormatErrorMod :: module().
raise_or_propagate(
    _OrigReason, [{Mod, Fn, _, _} | _], Mod, Fn, Reason, ErrorArgs, FormatErrorMod
) ->
    erlang:error(Reason, ErrorArgs, [{error_info, #{module => FormatErrorMod}}]);
raise_or_propagate(OrigReason, ST, _Mod, _Fn, _Reason, _ErrorArgs, _FormatErrorMod) ->
    erlang:raise(error, OrigReason, ST).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(A) when is_atom(A) -> atom_to_binary(A);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(T) -> unicode:characters_to_binary(io_lib:format("~0tp", [T])).

%% Iterative two-row Levenshtein over byte sequences. O(N*M) time, O(min(N,M))
%% space. Sufficient for short identifier keys, never called on hot paths.
distance(A, B) ->
    distance_rows(binary_to_list(A), binary_to_list(B)).

distance_rows([], B) ->
    length(B);
distance_rows(A, []) ->
    length(A);
distance_rows([A | As], B) ->
    Prev = lists:seq(0, length(B)),
    Final = step_row([A | As], B, Prev),
    lists:last(Final).

step_row([], _B, Prev) ->
    Prev;
step_row([A | As], B, Prev) ->
    [First | _] = Prev,
    Curr = build_row(B, Prev, A, [First + 1]),
    step_row(As, B, lists:reverse(Curr)).

build_row([], _Prev, _A, Acc) ->
    Acc;
build_row([B | Bs], [P0, P1 | Rest], A, [Last | _] = Acc) ->
    Cost =
        case A =:= B of
            true -> 0;
            false -> 1
        end,
    Min = min(min(Last + 1, P1 + 1), P0 + Cost),
    build_row(Bs, [P1 | Rest], A, [Min | Acc]).
