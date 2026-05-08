-module(arizona_error_hint).
-moduledoc """
Helpers for error messages: did-you-mean suggestions.

Used by `arizona_template:format_error/2` and `arizona_stream:format_error/2`
to append `Did you mean 'X'?` when a missing key looks like a typo of an
available key.
""".

-export([closest/2]).
-ignore_xref([closest/2]).

-doc """
Returns the candidate closest to `Target` by Levenshtein edit distance, or
`undefined` when nothing is close enough. The threshold scales with the
target length so very short keys must match almost exactly.
""".
-spec closest(Target, Candidates) -> Match | undefined when
    Target :: term(),
    Candidates :: [term()],
    Match :: term().
closest(_Target, []) ->
    undefined;
closest(Target, Candidates) ->
    TargetBin = to_bin(Target),
    Threshold = max(2, byte_size(TargetBin) div 4),
    Scored = [{distance(TargetBin, to_bin(C)), C} || C <- Candidates],
    case lists:sort(Scored) of
        [{D, Match} | _] when D =< Threshold, D > 0 -> Match;
        _ -> undefined
    end.

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
