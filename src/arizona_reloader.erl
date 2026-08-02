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
- `sync/0` -- force a compile+reload sync now (called by the dev MCP `reload` tool)
- `reload_css/0,1` -- broadcast a CSS reload (no compile needed)
- `get_error/0` / `clear_error/0` -- inspect/reset the last compile error
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([join/1]).
-export([broadcast/0]).
-export([reload_erl/1]).
-export([sync/0]).
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
Recompiles the changed `.erl` files, refreshes routes, and
broadcasts a reload message. Called by the file watcher.

After the reload is broadcast, runs a best-effort call-consistency check
(`arizona_reloader_consistency:check/1`) that warns about a loaded module left
calling a function the just-reloaded module no longer exports (the classic
stale-beam-after-mtime-skip mismatch that otherwise stays silent until a runtime
`undef`). It is advisory, so it runs *after* the broadcast -- never in the
reload's critical path, and its beam sweep can never delay the client seeing the
change. It never crashes or interferes with the reload.
""".
-spec reload_erl(Files) -> ok when
    Files :: [file:filename()].
reload_erl(Files) ->
    CompileResult = compile(Files),
    arizona_roadrunner_server:recompile_routes(),
    broadcast(),
    case CompileResult of
        ok -> arizona_reloader_consistency:check(Files);
        {error, _} -> ok
    end.

-doc """
Forces a compile+reload sync now, without waiting for a watcher event: under
`rebar3 shell` the project is recompiled through `rebar_agent` (which also
hot-reloads the modules it rebuilds); then any module whose loaded code still
differs from its beam on disk (a rebuild outside the watcher's eye) is
reloaded from that beam. Refreshes routes and broadcasts a reload on success,
mirroring `reload_erl/1`. Called by the dev MCP's `reload` tool.

Returns `{ok, #{agent := AgentUsed, reloaded := Mods}}` on success, or
`{error, Reason}` when the `rebar_agent` compile fails. The failure is
reported to the caller only -- the watcher wave owns the dev error page's
stash, and a forced sync has no changed-file list to collect structured
errors from.
""".
-spec sync() -> {ok, Info} | {error, term()} when
    Info :: #{agent := boolean(), reloaded := [module()]}.
sync() ->
    maybe
        {ok, AgentUsed} ?= agent_compile(),
        Reloaded = reload_stale_modules(),
        ok = arizona_roadrunner_server:recompile_routes(),
        ok = broadcast(),
        {ok, #{agent => AgentUsed, reloaded => Reloaded}}
    end.

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
    ErlFiles = [F || F <- Files, filename:extension(F) =:= ".erl"],
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

%% Recompile the project through rebar_agent when running under `rebar3 shell`
%% (it hot-reloads what it rebuilds). Absent the agent there is nothing to
%% compile with -- the sync still reconciles loaded code against the beams on
%% disk. A successful compile clears the stashed error, like compile/1 does.
agent_compile() ->
    case erlang:whereis(rebar_agent) of
        undefined ->
            {ok, false};
        _Pid ->
            case erlang:apply(rebar_agent, do, [compile]) of
                ok ->
                    clear_error(),
                    {ok, true};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Reload every module whose loaded code differs from its beam on disk, from
%% that beam (the path it was loaded from). A beam that fails to load
%% (mid-write) is skipped, as the consistency pass skips unreadable beams.
reload_stale_modules() ->
    Candidates = arizona_reloader_consistency:candidate_modules(),
    Stale = arizona_reloader_consistency:stale_modules(Candidates),
    [Mod || {Mod, _LoadedVsn, _DiskVsn} <:- Stale, reload_from_disk(Mod)].

reload_from_disk(Mod) ->
    case code:which(Mod) of
        Path when is_list(Path) ->
            code:purge(Mod),
            case code:load_abs(filename:rootname(Path)) of
                {module, Mod} -> true;
                {error, _Reason} -> false
            end;
        _NonPath ->
            false
    end.

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

%% Direct compile:file fallback when rebar3 is not available. The watcher treats
%% a delete as a relevant event, so a wave can carry paths that no longer exist;
%% compiling one would stash a spurious {epp, enoent} error on the dev error page
%% until an unrelated successful wave clears it. A deletion is not a compile
%% error, so vanished paths are skipped.
manual_compile(Files) ->
    manual_compile([F || F <- Files, filelib:is_regular(F)], []).

manual_compile([], []) ->
    ok;
manual_compile([], Errors) ->
    {error, #{errors => lists:reverse(Errors)}};
manual_compile([File | Rest], Errors) ->
    %% Only used to look up prior compile opts before the module name is known;
    %% the reloaded module is whatever the compiler reports, not the basename (a
    %% file whose `-module` differs from its filename otherwise `case_clause`es).
    Mod = list_to_atom(filename:basename(File, ".erl")),
    Opts = get_compile_opts(Mod),
    case compile:file(File, [binary, return_errors | Opts]) of
        {ok, ActualMod, Binary} ->
            reload_module(ActualMod, File, Binary),
            manual_compile(Rest, Errors);
        {ok, ActualMod, Binary, _Warnings} ->
            reload_module(ActualMod, File, Binary),
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
    [O || O <- Opts, keep_opt(O)].

keep_opt({outdir, _}) -> false;
keep_opt(binary) -> false;
keep_opt(return_errors) -> false;
keep_opt(report_errors) -> false;
keep_opt(report_warnings) -> false;
keep_opt(_) -> true.
