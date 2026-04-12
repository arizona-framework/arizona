-module(arizona_reloader).
-moduledoc """
Dev-mode hot reloader for `.erl` and CSS files.

Subscribers (typically connected WebSocket clients) `join/1` the
reloader's pubsub topic and receive `{arizona_reloader, reload}` (or
`reload_css`) messages whenever files change. The watcher process
(`arizona_watcher`) calls `reload_erl/1` or `reload_css/1` after a
debounced burst of file events.

## Compilation flow

`reload_erl/1` recompiles changed `.erl` files, then triggers a route
recompile and broadcasts a reload message:

1. If `rebar_agent` is registered (running under `rebar3 shell`), uses
   `rebar_agent:do(compile)` so deps and compile options stay aligned
   with the project.
2. Otherwise falls back to `compile:file/2` with options recovered from
   the existing module's `module_info(compile)`.
3. On compile error, structured errors from the changed files are
   stashed in a persistent term so the dev error page can render them
   on the next request. Subsequent successful compiles clear the error.

## Public reload API

- `join/1` -- subscribe a pid to the reloader topic (idempotent)
- `broadcast/0` -- emit a reload message manually
- `reload_erl/1` -- compile + reload + broadcast (called by the watcher)
- `reload_css/0,1` -- broadcast a CSS reload (no compile needed)
- `get_error/0` / `clear_error/0` -- inspect/reset the last compile error
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([join/1]).
-export([broadcast/0]).
-export([reload_erl/1]).
-export([reload_css/1]).
-export([reload_css/0]).
-export([compile/1]).
-export([get_error/0]).
-export([clear_error/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([broadcast/0, reload_erl/1, reload_css/0, reload_css/1, compile/1, clear_error/0]).
-ignore_xref({rebar_agent, do, 1}).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% Module names from filenames are bounded by the project's source files;
%% list_to_atom is the standard way to derive them in dev hot-reload code.
-elvis([{elvis_style, no_common_caveats_call, disable}]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(COMPILE_ERROR_KEY, arizona_compile_error).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Subscribes `Pid` to the reloader pubsub topic. Idempotent: re-joining
returns `ok`.

Silently no-ops if `arizona_pubsub` is not running (production mode).
""".
-spec join(Pid) -> ok when
    Pid :: pid().
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

-doc """
Broadcasts an `{arizona_reloader, reload}` message on the reloader
topic. Subscribed live processes typically reply by triggering a
client reload.
""".
-spec broadcast() -> ok.
broadcast() ->
    arizona_pubsub:broadcast(?MODULE, {?MODULE, reload}).

-doc """
Recompiles the changed `.erl` files, refreshes Cowboy routes, and
broadcasts a reload message. Called by the file watcher.
""".
-spec reload_erl(Files) -> ok when
    Files :: [file:filename()].
reload_erl(Files) ->
    _ = compile(Files),
    arizona_cowboy_server:recompile_routes(),
    broadcast().

-doc """
Triggers a CSS reload broadcast. Ignores the file list -- the client
re-fetches its stylesheet on receipt.
""".
-spec reload_css(Files) -> ok when
    Files :: [file:filename()].
reload_css(_Files) ->
    reload_css().

-doc """
Compiles a list of source files. Filters to `.erl` files, then either
delegates to `rebar_agent` (when running under `rebar3 shell`) or
falls back to a direct `compile:file/2` per file.

Returns `ok` on success or `{error, #{errors := [...]}}` on failure.
The error is also stashed in a persistent term so the dev error page
can read it via `get_error/0`.
""".
-spec compile(Files) -> ok | {error, map()} when
    Files :: [file:filename()].
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

-doc """
Broadcasts a CSS reload message. No-op when `arizona_pubsub` is not
running (production).
""".
-spec reload_css() -> ok.
reload_css() ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            ok;
        _ ->
            arizona_pubsub:broadcast(?MODULE, {?MODULE, reload_css}),
            ok
    end.

-doc """
Returns the last stashed compile error, or `undefined` if the most
recent compile succeeded.
""".
-spec get_error() -> undefined | map().
get_error() ->
    persistent_term:get(?COMPILE_ERROR_KEY, undefined).

-doc """
Clears the stashed compile error. Called after a successful compile.
""".
-spec clear_error() -> ok.
clear_error() ->
    persistent_term:put(?COMPILE_ERROR_KEY, undefined),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

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
        Info = apply(Mod, module_info, [compile]),
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
