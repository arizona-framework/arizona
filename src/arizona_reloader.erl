-module(arizona_reloader).
-export([join/1, broadcast/0]).
-export([reload_erl/1, reload_css/1, compile/1, reload_css/0, get_error/0, clear_error/0]).
-ignore_xref([broadcast/0, reload_erl/1, reload_css/0, reload_css/1, compile/1, clear_error/0]).
-ignore_xref({rebar_agent, do, 1}).

-define(COMPILE_ERROR_KEY, arizona_compile_error).

-spec join(pid()) -> ok.
join(Pid) ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            ok;
        _ ->
            case arizona_pubsub:subscribe(?MODULE, Pid) of
                ok -> ok;
                {error, already_joined} -> ok
            end
    end.

-spec broadcast() -> ok.
broadcast() ->
    arizona_pubsub:broadcast(?MODULE, {?MODULE, reload}).

-spec reload_erl([file:filename()]) -> ok.
reload_erl(Files) ->
    _ = compile(Files),
    arizona_cowboy_server:recompile_routes(),
    broadcast().

-spec reload_css([file:filename()]) -> ok.
reload_css(_Files) ->
    reload_css().

-spec compile([file:filename()]) -> ok | {error, map()}.
compile(Files) ->
    ErlFiles = [F || F <:- Files, filename:extension(F) =:= ".erl"],
    case ErlFiles of
        [] ->
            clear_error(),
            ok;
        _ ->
            case compile_and_load(ErlFiles) of
                ok ->
                    clear_error(),
                    ok;
                {error, ErrorInfo} ->
                    set_error(ErrorInfo),
                    {error, ErrorInfo}
            end
    end.

-spec reload_css() -> ok.
reload_css() ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            ok;
        _ ->
            arizona_pubsub:broadcast(?MODULE, {?MODULE, reload_css}),
            ok
    end.

-spec get_error() -> undefined | map().
get_error() ->
    persistent_term:get(?COMPILE_ERROR_KEY, undefined).

-spec clear_error() -> ok.
clear_error() ->
    persistent_term:put(?COMPILE_ERROR_KEY, undefined),
    ok.

%% --- Internal ----------------------------------------------------------------

set_error(Error) ->
    persistent_term:put(?COMPILE_ERROR_KEY, Error).

%% Use rebar_agent when available (rebar3 shell), fall back to manual compile.
compile_and_load(ErlFiles) ->
    case erlang:whereis(rebar_agent) of
        undefined ->
            manual_compile(ErlFiles);
        _ ->
            rebar3_compile(ErlFiles)
    end.

%% Let rebar3 handle compilation (correct opts, deps, reload).
%% On failure, collect structured errors from the changed files for display.
rebar3_compile(ErlFiles) ->
    case erlang:apply(rebar_agent, do, [compile]) of
        ok ->
            ok;
        {error, _} ->
            collect_errors(ErlFiles)
    end.

%% Direct compile:file fallback when rebar3 is not available.
manual_compile(Files) ->
    manual_compile(Files, []).

manual_compile([], []) ->
    ok;
manual_compile([], Errors) ->
    {error, #{errors => lists:reverse(Errors)}};
manual_compile([File | Rest], Errors) ->
    Mod = list_to_atom(filename:basename(File, ".erl")),
    Opts = get_compile_opts(Mod),
    case compile:file(File, [binary, return_errors | Opts]) of
        {ok, Mod, Binary} ->
            reload_module(Mod, File, Binary),
            manual_compile(Rest, Errors);
        {ok, Mod, Binary, _Warnings} ->
            reload_module(Mod, File, Binary),
            manual_compile(Rest, Errors);
        {error, FileErrors, _Warnings} ->
            manual_compile(Rest, FileErrors ++ Errors)
    end.

%% Re-compile changed files with return_errors to get structured error info.
collect_errors(Files) ->
    Errors = lists:foldl(
        fun(File, Acc) ->
            Mod = list_to_atom(filename:basename(File, ".erl")),
            Opts = get_compile_opts(Mod),
            case compile:file(File, [binary, return_errors | Opts]) of
                {ok, _, _} -> Acc;
                {ok, _, _, _} -> Acc;
                {error, FileErrors, _} -> FileErrors ++ Acc
            end
        end,
        [],
        Files
    ),
    case Errors of
        [] ->
            %% Error was in a dep or file not in the changed list.
            {error, #{errors => []}};
        _ ->
            {error, #{errors => lists:reverse(Errors)}}
    end.

reload_module(Mod, File, Binary) ->
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, File, Binary),
    ok.

get_compile_opts(Mod) ->
    try
        Info = Mod:module_info(compile),
        RawOpts = proplists:get_value(options, Info, []),
        filter_opts(RawOpts)
    catch
        _:_ -> [debug_info]
    end.

filter_opts(Opts) ->
    [O || O <:- Opts, keep_opt(O)].

keep_opt({outdir, _}) -> false;
keep_opt(binary) -> false;
keep_opt(return_errors) -> false;
keep_opt(report_errors) -> false;
keep_opt(report_warnings) -> false;
keep_opt(_) -> true.
