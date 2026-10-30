-module(arizona_error).
-moduledoc """
Helpers for error reporting.

Utilities consumed across the framework:

- `did_you_mean/2` -- "did you mean?" suggestion for `format_error/2`
  clauses that report a missing key. Used by
  `arizona_template:format_error/2` and `arizona_stream:format_error/2`.
- `raise_or_propagate/6` -- the catch-clause helper that lets a
  dispatcher re-tag a failure originating at the user's exact callback
  while propagating any other failure untouched. Used by every
  `arizona_stateful:call_*` wrapper.
- `format_title/3`, `format_reason/3`, `format_fun_name/2`, `format_loc/1`
  -- format-neutral crash formatting (location title, reason summary,
  `{compile_error, _}` and `{arizona_loc, ...}` reasons, stack frames), shared
  by the plain-text dev 500 (`format_text/3`) and any rich error page.
""".

-export([did_you_mean/2]).
-export([raise_or_propagate/6]).
-export([format_text/3]).
-export([format_title/3]).
-export([format_reason/3]).
-export([format_fun_name/2]).
-export([format_loc/1]).
-ignore_xref([did_you_mean/2]).
-ignore_xref([raise_or_propagate/6]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Ceiling on the edit distance a suggestion may have, whatever the target's
%% length. A typo is one or two edits; beyond that it is a different word, and
%% letting the budget grow with the key (the old `Len div 4`) suggested keys 25%
%% different for a long one.
-define(MAX_DISTANCE, 2).

-doc """
Returns the candidate closest to `Target` by Levenshtein edit distance, or
`undefined` when nothing is close enough.

The threshold is **at most half the target's length, and never more than
`?MAX_DISTANCE` edits**, so a very short key must match almost exactly: a
one-character key has no near miss at all (every other one is 100% different),
and a two-character key tolerates one edit but not two. A key of four bytes or
more gets the flat typo budget, which still suggests `counter` for `count`.
""".
-spec did_you_mean(Target, Candidates) -> Match | undefined when
    Target :: term(),
    Candidates :: [term()],
    Match :: term().
did_you_mean(_Target, []) ->
    undefined;
did_you_mean(Target, Candidates) ->
    TargetBin = to_bin(Target),
    Threshold = min(byte_size(TargetBin) div 2, ?MAX_DISTANCE),
    Scored = [{distance(TargetBin, to_bin(C)), C} || C <- Candidates],
    case lists:sort(Scored) of
        [{D, Match} | _] when D =< Threshold, D > 0 -> Match;
        _ -> undefined
    end.

-doc """
Catch-clause helper for dispatchers that wrap a user callback.

When the top stack frame is the user's exact callback (`Mod:Fn/Arity`),
raises `Reason` with an `error_info` annotation pointing at
`FormatErrorMod` -- the module that exports the matching `format_error/2`
clause. Otherwise propagates `error:OrigReason` with the original
stacktrace untouched.

The arity is part of the match, not decoration: a callback whose body calls
a same-named function of a different arity (`?MODULE:handle(Req, Opts)`
from inside `handle/1`) leaves a top frame naming that other function, and
re-tagging it would report the running callback as the missing one.

The `FormatErrorMod` parameter exists because this helper lives in
`arizona_error` but is called from various dispatcher modules; the
`error_info` annotation must point at whichever module owns the format
clauses for `Reason`.
""".
-spec raise_or_propagate(OrigReason, Stacktrace, MFA, Reason, ErrorArgs, FormatErrorMod) ->
    no_return()
when
    OrigReason :: term(),
    Stacktrace :: [tuple()],
    MFA :: {module(), atom(), arity()},
    Reason :: term(),
    ErrorArgs :: [term()],
    FormatErrorMod :: module().
%% The top frame of an `undef` or `function_clause` always carries the call's
%% argument list (a deeper frame is the one that degrades to a bare arity), so
%% the arity check is `length(Args)`.
raise_or_propagate(
    _OrigReason, [{Mod, Fn, Args, _} | _], {Mod, Fn, Arity}, Reason, ErrorArgs, FormatErrorMod
) when
    length(Args) =:= Arity
->
    erlang:error(Reason, ErrorArgs, [{error_info, #{module => FormatErrorMod}}]);
raise_or_propagate(OrigReason, ST, _MFA, _Reason, _ErrorArgs, _FormatErrorMod) ->
    erlang:raise(error, OrigReason, ST).

-doc """
A plain-text crash report: the title, the reason and one line per stack frame.
""".
-spec format_text(Class, Reason, Stacktrace) -> binary() when
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace().
format_text(Class, Reason, Stacktrace) ->
    unicode:characters_to_binary([
        format_title(Class, Reason, Stacktrace),
        "\n\n",
        format_reason(Class, Reason, Stacktrace),
        "\n",
        [text_frame(Frame) || Frame <- Stacktrace],
        "\n"
    ]).

%% Title is just a short location -- the body shows the full reason.
%%
%% Strategy:
%%
%% 1. `compile_error` -- always "Compilation Error".
%% 2. Otherwise, walk the stacktrace for the deepest user frame --
%%    skipping `arizona_*` (framework) and any OTP/stdlib module.
%%    That's the user's nearest call site, the line they actually want
%%    to look at.
%% 3. If the whole stack is framework/OTP, fall back to the top frame.
%% 4. If the stack is empty, fall back to `arizona_loc`'s captured
%%    location (when the reason is wrapped) or the bare class.
%%
%% Note: `arizona_loc` captures the *lexical* location of a template
%% dynamic, which for layouts is the outer dynamic's call site, not
%% the inner crash site. The stack tells the truth, so prefer it.
-doc """
A one-line title for a crash: the source location the developer wants to look
at (`Module:Line`), or `Compilation Error` for a hot-reload compile failure.
""".
-spec format_title(Class, Reason, Stacktrace) -> binary() when
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace().
format_title(_Class, {compile_error, _Errors}, _Stacktrace) ->
    ~"Compilation Error";
format_title(Class, {arizona_loc, {LocMod, _}, _} = Reason, Stacktrace) ->
    %% `arizona_loc` names the template module by construction, so prefer a frame
    %% from THAT module over the generic "first non-framework" walk. Two shapes the
    %% generic walk gets wrong: an app module named `arizona_*` (every fixture in
    %% test/support) is dropped as framework and the title falls back to the
    %% wrapper's own -- outer -- line; and a value that fails AFTER its closure
    %% returned (`bad_template_value` from a map in a slot) has no template frame
    %% left on the stack, so the walk keeps going and names the CALLER, which on a
    %% live server is a roadrunner internal.
    case strict_user_frame(Stacktrace) of
        {ok, Mod, Line} ->
            %% A genuine user frame is the most precise answer -- keep it, so a
            %% crash inside a helper module still names that module's line.
            fmt_mod_line(Mod, Line);
        error ->
            case frame_from(LocMod, Stacktrace) of
                {ok, Line} -> fmt_mod_line(LocMod, Line);
                error -> fallback_title(Class, Reason)
            end
    end;
format_title(Class, Reason, Stacktrace) ->
    format_title_generic(Class, Reason, Stacktrace).

%% A compile failed but no diagnostics survived: `arizona_reloader` recovers
%% structured errors by recompiling the files the watcher reported, so a failure
%% anywhere else -- a dependency, or a file outside this wave -- collects nothing.
%% rebar's own return is `{throw, rebar_abort}`, which carries no diagnostics
%% either, and the real output went to the console. Say that, rather than render
%% an empty box that reads as a bug in the error page.
-doc """
The crash's full summary: the `format_error/2` sentence or the shell-style
exception line, a per-file diagnostic list for `{compile_error, Errors}`, with
an `{arizona_loc, Loc, Reason}` wrapper stripped first.
""".
-spec format_reason(Class, Reason, Stacktrace) -> binary() when
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace().
format_reason(_Class, {compile_error, []}, _Stacktrace) ->
    unicode:characters_to_binary([
        "The compile failed, but none of the files that changed reproduce the error,\n",
        "so there is nothing to show here.\n\n",
        "The failure is in a file this reload did not compile -- a dependency, or a\n",
        "file the watcher did not report. Its diagnostics went to the console running\n",
        "the server; read them there."
    ]);
format_reason(_Class, {compile_error, Errors}, _Stacktrace) ->
    unicode:characters_to_binary(
        lists:join("\n\n", [format_file_errors(F, Es) || {F, Es} <:- Errors])
    );
format_reason(Class, {arizona_loc, _Loc, Reason}, Stacktrace) ->
    format_reason(Class, Reason, Stacktrace);
format_reason(Class, Reason, Stacktrace) ->
    %% Body shows the full summary -- title is just the location, so
    %% there's no repetition. Falls back to `~0tp` for plain reasons
    %% without any erl_error-format output.
    case error_summary(Class, Reason, Stacktrace) of
        [] -> unicode:characters_to_binary(io_lib:format("~0tp", [Reason]));
        Summary -> unicode:characters_to_binary(Summary)
    end.

%% Rewrites OTP-mangled fun names for readability.
%%
%%   '-render/1-fun-1-'/N         -> "render/1 (anonymous fun)"
%%   '-render_ssr_val/1-lc$^0/1-0-'/N -> "render_ssr_val/1 (list comprehension)"
%%   '-render/1-mc$^0/1-0-'/N         -> "render/1 (map comprehension)"
%%   <plain Fn>/N                  -> "Fn/N"
-doc """
A stack frame's function name, with compiler-generated names (funs and
comprehensions) rewritten to the function that holds them.
""".
-spec format_fun_name(Fun, Arity) -> binary() when
    Fun :: atom(),
    Arity :: arity() | [term()].
format_fun_name(Fn, A) ->
    Arity = arity(A),
    Name = atom_to_list(Fn),
    case prettify_mangled(Name) of
        {Outer, Suffix} ->
            unicode:characters_to_binary([Outer, " (", Suffix, ")"]);
        plain ->
            unicode:characters_to_binary([Name, "/", integer_to_list(Arity)])
    end.

-doc """
A stack frame's `File:Line`, relative to the working directory, or `<<>>` when
the frame carries no file.
""".
-spec format_loc(Info) -> binary() when
    Info :: [{atom(), term()}].
format_loc(Info) ->
    File = proplists:get_value(file, Info, ""),
    Line = proplists:get_value(line, Info, 0),
    case File of
        "" -> <<>>;
        _ -> unicode:characters_to_binary([relative_path(File), ":", integer_to_list(Line)])
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

text_frame({Mod, Fun, Arity, Info}) ->
    Loc =
        case format_loc(Info) of
            <<>> -> [];
            Path -> [" ", Path]
        end,
    ["\n  ", atom_to_binary(Mod), ":", format_fun_name(Fun, Arity), Loc].

format_title_generic(Class, Reason, Stacktrace) ->
    case first_user_frame(Stacktrace) of
        none ->
            fallback_title(Class, Reason);
        {Mod, _Fn, _A, Info} ->
            case proplists:get_value(line, Info) of
                undefined -> fallback_title(Class, Reason);
                Line -> fmt_mod_line(Mod, Line)
            end
    end.

%% Like `first_user_frame/1` but with no framework fallback: `error` when every
%% frame is framework or OTP. That happens when the value failed AFTER its closure
%% returned (a map reaching `to_bin/1`), leaving no template frame at all -- there
%% the lexical `arizona_loc` is the only real answer, and the generic walk's
%% topmost-frame fallback would name an arizona internal instead.
strict_user_frame([]) ->
    error;
strict_user_frame(Stack) ->
    case lists:dropwhile(fun is_framework_frame/1, Stack) of
        [] ->
            error;
        [{Mod, _Fn, _A, Info} | _] ->
            case proplists:get_value(line, Info) of
                undefined -> error;
                Line -> {ok, Mod, Line}
            end
    end.

%% The topmost line from `Mod`, or `error` when it has no frame carrying one.
frame_from(_Mod, []) ->
    error;
frame_from(Mod, [{Mod, _Fn, _A, Info} | Rest]) ->
    case proplists:get_value(line, Info) of
        undefined -> frame_from(Mod, Rest);
        Line -> {ok, Line}
    end;
frame_from(Mod, [_Frame | Rest]) ->
    frame_from(Mod, Rest).

fallback_title(_Class, {arizona_loc, {Mod, Line}, _}) ->
    fmt_mod_line(Mod, Line);
fallback_title(Class, _Reason) ->
    unicode:characters_to_binary(io_lib:format("~s", [Class])).

fmt_mod_line(Mod, Line) ->
    unicode:characters_to_binary(io_lib:format("~s:~b", [Mod, Line])).

%% Returns the first stack frame whose module is not framework or OTP,
%% the topmost frame when the whole stack is framework/OTP, or `none`
%% when the stack is empty.
first_user_frame([]) ->
    none;
first_user_frame([Top | _] = Stack) ->
    case lists:dropwhile(fun is_framework_frame/1, Stack) of
        [] -> Top;
        [User | _] -> User
    end.

is_framework_frame({Mod, _Fn, _A, _Info}) ->
    is_arizona_module(Mod) orelse is_otp_module(Mod).

is_arizona_module(Mod) ->
    case atom_to_list(Mod) of
        "arizona_" ++ _ -> true;
        _ -> false
    end.

%% True for OTP/stdlib modules. Detected via `code:which/1`:
%% built-ins are `preloaded`, stdlib lives under `code:root_dir()`.
%% User modules and dependencies (e.g. arizona itself when checked out)
%% live elsewhere and return false here.
is_otp_module(Mod) ->
    case code:which(Mod) of
        preloaded ->
            true;
        Path when is_list(Path) ->
            string:prefix(Path, code:root_dir() ++ "/") =/= nomatch;
        _ ->
            false
    end.

prettify_mangled([$- | Rest]) ->
    prettify_mangled_body(Rest, [
        {"-fun-", "anonymous fun"},
        {"-lc$", "list comprehension"},
        {"-mc$", "map comprehension"}
    ]);
prettify_mangled(_) ->
    plain.

prettify_mangled_body(_Rest, []) ->
    plain;
prettify_mangled_body(Rest, [{Sep, Suffix} | More]) ->
    case string:split(Rest, Sep) of
        [Outer, _Tail] when Outer =/= Rest -> {Outer, Suffix};
        _ -> prettify_mangled_body(Rest, More)
    end.

%% Use erl_error:format_exception to get the rich, shell-style summary.
%% Output for an error_info-annotated raise looks like:
%%   "exception error: missing_binding\n
%%      in function  arizona_template:get/2\n
%%         called as arizona_template:get(missing_key, #{a => 1})\n
%%         *** binding missing_key not found. Available bindings: ...\n
%%      in call from ..."
%% The format_error/2 sentence lands on the `*** ` line, nested under
%% the failing function's frame -- prefer it when present. For errors
%% without error_info (plain badmatch, etc.) the top "exception <class>: "
%% line already carries the useful message, so we fall back to that.
error_summary(Class, Reason, Stacktrace) ->
    try
        Flat = lists:flatten(erl_error:format_exception(Class, Reason, Stacktrace)),
        extract_summary(Flat)
    catch
        _:_ ->
            io_lib:format("~w:~0tp", [Class, Reason])
    end.

%% Prefer the format_error/2 annotation line when erl_error attached one.
%% Otherwise take the top "exception <class>: ..." line up to the first
%% stack frame.
extract_summary(Flat) ->
    case extract_format_error_line(Flat) of
        nomatch -> extract_top_line(Flat);
        Line -> Line
    end.

%% erl_error indents the format_error/2 annotation with 5 spaces under
%% "called as ...". Find that marker, skip it, take the rest of the line.
extract_format_error_line(Flat) ->
    Marker = "\n     *** ",
    case string:find(Flat, Marker) of
        nomatch ->
            nomatch;
        Found ->
            AfterMarker = string:slice(Found, length(Marker)),
            take_until_newline(AfterMarker, [])
    end.

take_until_newline([$\n | _], Acc) -> lists:reverse(Acc);
take_until_newline([C | Rest], Acc) -> take_until_newline(Rest, [C | Acc]);
take_until_newline([], Acc) -> lists:reverse(Acc).

extract_top_line("exception " ++ Rest) ->
    skip_class(Rest);
extract_top_line(Other) ->
    Other.

skip_class([$:, $\s | Rest]) ->
    take_summary(Rest, []);
skip_class([_ | Rest]) ->
    skip_class(Rest);
skip_class([]) ->
    [].

%% "\n  in " marks start of stack frames -- stop here.
take_summary([$\n | Rest], Acc) ->
    Trimmed = skip_spaces(Rest),
    case Trimmed of
        [$i, $n, $\s | _] -> lists:reverse(Acc);
        _ -> take_summary(Trimmed, [$\s | Acc])
    end;
take_summary([C | Rest], Acc) ->
    take_summary(Rest, [C | Acc]);
take_summary([], Acc) ->
    lists:reverse(Acc).

skip_spaces([$\s | Rest]) -> skip_spaces(Rest);
skip_spaces(Other) -> Other.

format_file_errors(File, Errors) ->
    [File, ":\n", lists:join("\n", [format_one_error(E) || E <- Errors])].

%% The compiler reports `none` for a diagnostic tied to no line -- an undefined
%% or crashing parse transform -- which `erl_anno:line/1` rejects.
format_one_error({none, Mod, Reason}) ->
    io_lib:format("  ~ts", [Mod:format_error(Reason)]);
format_one_error({Location, Mod, Reason}) ->
    Line = erl_anno:line(Location),
    Msg = Mod:format_error(Reason),
    io_lib:format("  line ~b: ~ts", [Line, Msg]).

%% Trims the cwd prefix from absolute paths so frames render as e.g.
%% `src/arizona_template.erl:153` instead of
%% `/home/.../arizona_template.erl:153`. Falls through unchanged when
%% cwd lookup fails or the path is already relative / outside cwd.
relative_path(File) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            case string:prefix(File, Cwd ++ "/") of
                nomatch -> File;
                Rel -> Rel
            end;
        _ ->
            File
    end.

arity(A) when is_list(A) -> length(A);
arity(N) when is_integer(N) -> N.

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
