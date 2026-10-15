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
   stashed in a persistent term, per file, so the dev error page can
   render them on the next request. A file's errors stay until a wave
   that carries it compiles it (or drops it as deleted); a clean
   `rebar_agent` compile builds the whole project, so it clears them all.

After the compile, a sweep reloads any module whose loaded code no longer
matches its beam on disk. Apps listed in rebar3's `app_reload_blacklist` shell
option (or the `rebar` `refresh_paths_blacklist` env that overrides it) are
skipped: reloading a module purges the old code, which kills any process still
lingering in it -- a listener blocked in its own loop, typically -- so every
recompile would otherwise log a supervisor report.

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
-export([format_error/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    broadcast/0, reload_erl/1, reload_css/0, reload_css/1, compile/1, clear_error/0, format_error/1
]).
-ignore_xref({rebar_agent, do, 1}).
-ignore_xref({rebar_state, get, 3}).

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

Returns `ok` when this wave compiled, or `{error, #{errors := [...]}}` with
this wave's errors. The stash `get_error/0` reads is kept per file, so it can
still hold another file's errors after an `ok`: a broken file stays reported
until a wave compiles it, not until any unrelated file compiles.
""".
-spec compile(Files) -> ok | {error, map()} when
    Files :: [file:filename()].
compile(Files) ->
    case [F || F <- Files, filename:extension(F) =:= ".erl"] of
        [] -> ok;
        ErlFiles -> compile_and_load(ErlFiles)
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
Returns the stashed compile errors of every file still failing, or
`undefined` when none is.
""".
-spec get_error() -> undefined | map().
get_error() ->
    case persistent_term:get(?COMPILE_ERROR_KEY, undefined) of
        undefined -> undefined;
        ByFile -> #{errors => flatten_errors(ByFile)}
    end.

-doc """
Clears every stashed compile error. Called after a clean whole-project compile.
""".
-spec clear_error() -> ok.
clear_error() ->
    persistent_term:put(?COMPILE_ERROR_KEY, undefined),
    ok.

-doc """
Formats a diagnostic this module adds to the stashed compile errors, the way
`erl_lint:format_error/1` formats its own: the dev error page calls it through
the `{Location, Module, Reason}` triple.
""".
-spec format_error(Reason) -> string() when
    Reason :: {load_failed, module(), term()}.
format_error({load_failed, Mod, on_load_failure}) ->
    io_lib:format("module ~ts compiled but did not load: its -on_load function failed", [Mod]);
format_error({load_failed, Mod, Reason}) ->
    io_lib:format("module ~ts compiled but did not load: ~tp", [Mod, Reason]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Replaces the stashed errors of the wave's files with what this wave found,
%% keeping every other file's. `ProjectFailed` is a `rebar_agent` failure: the
%% project is broken even when no file reproduces it, and the error page must say
%% so (an empty error list, which it explains) rather than show no error at all.
stash_wave(Wave, WaveErrors, ProjectFailed) ->
    Kept = maps:without([unattributed | Wave], stashed_errors()),
    Stash0 = maps:merge(Kept, WaveErrors),
    Stash =
        case ProjectFailed andalso map_size(Stash0) =:= 0 of
            true -> #{unattributed => []};
            false -> Stash0
        end,
    ok =
        case map_size(Stash) of
            0 -> clear_error();
            _ -> persistent_term:put(?COMPILE_ERROR_KEY, Stash)
        end,
    case ProjectFailed orelse map_size(WaveErrors) > 0 of
        true -> {error, #{errors => flatten_errors(WaveErrors)}};
        false -> ok
    end.

stashed_errors() ->
    case persistent_term:get(?COMPILE_ERROR_KEY, undefined) of
        undefined -> #{};
        ByFile -> ByFile
    end.

flatten_errors(ByFile) ->
    [E || _ := Es <- maps:iterator(ByFile, ordered), E <- Es].

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
    Blacklist = reload_blacklist(),
    [
        Mod
     || {Mod, _LoadedVsn, _DiskVsn} <:- Stale,
        not blacklisted(Mod, Blacklist),
        reload_from_disk(Mod)
    ].

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

%% Apps whose modules this sweep leaves alone, read from the same
%% `app_reload_blacklist` rebar3's own shell reloader honors. A listener process
%% sits blocked in its own loop, so reloading its module leaves it lingering in
%% old code and the next `code:purge/1` kills it -- turning every recompile into
%% a supervisor report. rebar3 applies the option only to the reload it performs
%% itself, so this sweep has to read it too or it purges the very module rebar3
%% was told to skip.
reload_blacklist() ->
    case application:get_env(rebar, refresh_paths_blacklist) of
        {ok, Blacklist} ->
            Blacklist;
        undefined ->
            shell_blacklist()
    end.

%% The option lives in the running `rebar3 shell` state, which exposes no
%% accessor for it -- read it the way the agent stores it, and treat any other
%% shape as no blacklist so a rebar3 that reorganizes its state degrades to
%% today's behaviour instead of crashing the reload.
shell_blacklist() ->
    case erlang:whereis(rebar_agent) of
        undefined ->
            [];
        _Pid ->
            try
                {state, RState, _Cwd, _ShowWarning} = sys:get_state(rebar_agent),
                ShellOpts = erlang:apply(rebar_state, get, [RState, shell, []]),
                proplists:get_value(app_reload_blacklist, ShellOpts, [])
            catch
                _Class:_Reason ->
                    []
            end
    end.

blacklisted(_Mod, []) ->
    false;
blacklisted(Mod, Blacklist) ->
    case application:get_application(Mod) of
        {ok, App} ->
            lists:member(App, Blacklist);
        undefined ->
            false
    end.

%% Use rebar_agent when available (rebar3 shell), fall back to manual compile.
compile_and_load(ErlFiles) ->
    case erlang:whereis(rebar_agent) of
        undefined ->
            stash_wave(ErlFiles, manual_compile(ErlFiles), false);
        _ ->
            rebar3_compile(ErlFiles)
    end.

%% Let rebar3 handle compilation (correct opts, deps, reload).
%% On failure, collect structured errors from the changed files for display.
rebar3_compile(ErlFiles) ->
    case erlang:apply(rebar_agent, do, [compile]) of
        ok ->
            clear_error();
        {error, _} ->
            stash_wave(ErlFiles, collect_errors(ErlFiles), true)
    end.

%% Direct compile:file fallback when rebar3 is not available, returning the
%% errors of each file that failed.
manual_compile(Files) ->
    wave_errors(Files, fun compile_one/1).

compile_one(File) ->
    case compile_file(File) of
        {ok, Mod, Binary} -> load_errors(File, reload_module(Mod, File, Binary));
        {error, FileErrors} -> FileErrors
    end.

%% Re-compile changed files with return_errors to get structured error info.
collect_errors(Files) ->
    wave_errors(Files, fun file_errors/1).

file_errors(File) ->
    case compile_file(File) of
        {ok, _Mod, _Binary} -> [];
        {error, FileErrors} -> FileErrors
    end.

%% The errors of each wave file that has any, in either mode. The watcher treats
%% a delete as a relevant event, so a wave can carry paths that no longer exist;
%% compiling one would stash a spurious {epp, enoent} error on the dev error page.
%% A deletion is not a compile error, so vanished paths are skipped (and, being in
%% the wave, have their stashed errors dropped).
wave_errors(Files, FileErrors) ->
    #{F => Es || F <- Files, filelib:is_regular(F), Es <- [FileErrors(F)], Es =/= []}.

%% `return_warnings` pins the success shape to the 4-tuple whatever the
%% recovered options hold; nothing here reports warnings either way. The module
%% is whatever the compiler reports, not the basename (a file whose `-module`
%% differs from its filename otherwise `case_clause`es).
compile_file(File) ->
    case compile:file(File, [binary, return_errors, return_warnings | compile_opts(File)]) of
        {ok, Mod, Binary, _Warnings} -> {ok, Mod, Binary};
        {error, FileErrors, _Warnings} -> {error, FileErrors}
    end.

%% A module that compiles can still fail to load (`-on_load` not returning
%% `ok`). That is reported like a compile error rather than raised: the watcher
%% runs this in its own callback, so a raise would crash it and skip the rest of
%% the wave along with the route recompile.
reload_module(Mod, File, Binary) ->
    code:purge(Mod),
    case code:load_binary(Mod, File, Binary) of
        {module, Mod} -> ok;
        {error, Reason} -> {error, {load_failed, Mod, Reason}}
    end.

load_errors(_File, ok) ->
    [];
load_errors(File, {error, Reason}) ->
    [{File, [{none, ?MODULE, Reason}]}].

%% The basename is only a guess at the module, used to look up the options it
%% was last compiled with before the compiler reports the real name.
compile_opts(File) ->
    get_compile_opts(list_to_atom(filename:basename(File, ".erl"))).

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
