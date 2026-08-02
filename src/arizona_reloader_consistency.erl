-module(arizona_reloader_consistency).
-moduledoc """
Post-reload call-consistency check for the dev hot reloader.

`arizona_reloader` hot-loads recompiled modules one at a time. Nothing
otherwise verifies that the **set** of loaded modules stays call-consistent
after a wave, so two common dev situations leave the node running a mix of
module versions that each compile fine but cannot call each other:

- an exported function changes arity (or is renamed) in module `A` while a
  caller `B` is not recompiled/reloaded in the same wave -- because the build
  tool skipped it on stale `_build` beam mtimes (a branch switch, or another
  checkout stamping beams newer than the sources), or because the reload batch
  raced;
- a module is running from a beam that is older than its source on disk.

The mismatch stays **silent** until the first runtime call, which raises
`undef`. When that call sits on a hot path (a per-tick `gen_server`), the
process crash-loops, exhausts its supervisor's restart intensity within
seconds, and takes the whole tree down -- while route serving keeps working, so
the app looks up but is dead behind it. The mismatch is machine-detectable the
moment it exists.

## What this does

After each reload wave the reloader calls `check/1`, which:

1. Collects the just-reloaded module set from the changed `.erl` files
   (`reloaded_modules/1`), keeping only modules that are currently loaded.
2. For every loaded application module (`candidate_modules/0` -- the loaded set
   minus OTP, filtered to modules with a readable beam file), reads its static
   external-call table and reports every edge into a reloaded module whose
   target function the reloaded module no longer exports (`broken_edges/2`).
3. Optionally flags any loaded module whose in-memory version differs from its
   beam on disk (`stale_modules/1`), which catches the mtime-skip case even
   before a call breaks.
4. Logs one loud `warning` per finding, naming the caller and the missing MFA.

The two edge signals are complementary. A caller's call table is read from its
beam on disk, so `broken_edges/2` catches the classic stale-caller-on-disk case
(the caller was never recompiled). `stale_modules/1` catches the opposite case
-- a caller whose disk beam is fresh but whose in-memory code is stale -- via
the version mismatch.

## Detection mechanism

The static call table comes from the beam `imports` chunk via
`beam_lib:chunks/2` (stdlib), not `xref` -- so there is no runtime dependency on
the `tools` application and no xref server to start. Dynamic calls (`apply/3`,
behaviour callbacks resolved at runtime) are invisible to the import table; that
is fine, the check is best-effort and the static case is the one that bites.

The pass runs synchronously in the watcher on every save, over every loaded
non-OTP module, so its disk work is kept minimal: each candidate's `imports`
and `attributes` (vsn) chunks are fetched in **one** `beam_lib` pass, and the
result is cached across waves keyed by the beam file's path + mtime -- an
unchanged module costs one `stat` per wave, not repeated full reads. The wave's
just-reloaded modules are evicted up front (their beams were rewritten, and an
mtime has whole-second resolution), so their fresh facts are always re-read.

The whole pass is **best-effort**: it runs only on a successful reload (dev
mode), finds nothing and logs nothing on a clean wave, and never crashes or
interferes with the reload -- `check/1` wraps everything in a catch-all.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([check/1]).
-export([reloaded_modules/1]).
-export([candidate_modules/0]).
-export([broken_edges/2]).
-export([stale_modules/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% These are called locally by check/1 and directly by the test suite; xref sees
%% no external caller, so the exports read as unused. (candidate_modules/0 and
%% stale_modules/1 need no entry -- arizona_dev_mcp's reloader_status calls them.)
-ignore_xref([reloaded_modules/1]).
-ignore_xref([broken_edges/2]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% Module names are derived from source filenames via list_to_atom, the same
%% way the reloader itself does; the set is bounded by the project's sources.
-elvis([{elvis_style, no_common_caveats_call, disable}]).

-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% The cross-wave beam-facts cache: one row per module, keyed by the beam
%% file's path + mtime. Created lazily by whichever process runs the first
%% check (the watcher in dev mode) and owned by it -- losing the owner just
%% drops a cache, rebuilt on the next wave.
-define(CACHE, arizona_reloader_beam_cache).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type mfargs() :: {module(), atom(), arity()}.
-type broken_edge() :: {Caller :: module(), Missing :: mfargs()}.
-type stale() :: {module(), LoadedVsn :: term(), DiskVsn :: term()}.

-export_type([broken_edge/0]).
-export_type([stale/0]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Runs the consistency pass over the wave described by `Files` (the changed
source files) and logs one `warning` per finding. Always returns `ok`.

Best-effort by design: a post-reload probe must never crash or interfere with
the reload it follows, so the whole pass is wrapped in a catch-all. On a clean
reload it finds nothing and logs nothing.
""".
-spec check(Files) -> ok when
    Files :: [file:filename()].
check(Files) ->
    %% Best-effort: this catch-all is the one sanctioned defensive guard --
    %% everything the probe inspects (beams on disk, the loaded code table) can
    %% shift under it between reads, and none of it must ever break the reload.
    try
        do_check(Files)
    catch
        Class:Reason:Stacktrace ->
            logger:debug(
                "arizona_reloader consistency check aborted: ~p:~p~n~p",
                [Class, Reason, Stacktrace]
            )
    end.

-doc """
The modules just reloaded in this wave: the module name of each changed `.erl`
file that is currently loaded. A not-yet-loaded module is dropped -- lazy
loading is normal, not a mismatch.
""".
-spec reloaded_modules(Files) -> [module()] when
    Files :: [file:filename()].
reloaded_modules(Files) ->
    [
        Mod
     || File <- Files,
        filename:extension(File) =:= ".erl",
        Mod <- [list_to_atom(filename:basename(File, ".erl"))],
        is_loaded(Mod)
    ].

-doc """
The application modules to scan as candidate callers: every loaded module with
a readable beam file that is not part of OTP. Excluding OTP keeps the sweep to
project and dependency code (an OTP module never calls a just-changed app
module) and cheap.
""".
-spec candidate_modules() -> [module()].
candidate_modules() ->
    [Mod || {Mod, _Loaded} <:- code:all_loaded(), beam_path(Mod) =/= error].

-doc """
Every broken call edge from a candidate caller into a reloaded module: a
`{Caller, {Mod, Fun, Arity}}` for each static call in `Caller`'s beam whose
target `Mod` is in `Reloaded` and whose `Mod:Fun/Arity` the currently-loaded
`Mod` no longer exports.

Scoped to `Reloaded` targets so only edges this wave could have broken are
reported, not any pre-existing unrelated `undef`.
""".
-spec broken_edges(Reloaded, Candidates) -> [broken_edge()] when
    Reloaded :: [module()],
    Candidates :: [module()].
broken_edges(Reloaded, Candidates) ->
    [
        {Caller, {Mod, Fun, Arity}}
     || Caller <- Candidates,
        {Mod, Fun, Arity} <:- maps:get(imports, beam_facts(Caller)),
        lists:member(Mod, Reloaded),
        not erlang:function_exported(Mod, Fun, Arity)
    ].

-doc """
Every candidate module whose in-memory version differs from its beam on disk --
a `{Mod, LoadedVsn, DiskVsn}` per stale module. A module whose loaded or disk
version cannot be read is skipped.
""".
-spec stale_modules(Candidates) -> [stale()] when
    Candidates :: [module()].
stale_modules(Candidates) ->
    [
        {Mod, LoadedVsn, DiskVsn}
     || Mod <- Candidates,
        {LoadedVsn, DiskVsn} <:- [module_vsns(Mod)],
        LoadedVsn =/= unknown,
        DiskVsn =/= unknown,
        LoadedVsn =/= DiskVsn
    ].

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

do_check(Files) ->
    case reloaded_modules(Files) of
        [] ->
            ok;
        Reloaded ->
            %% The wave just rewrote these beams; drop their cache rows so this
            %% pass re-reads them even when the rewrite landed within the same
            %% mtime second as the cached read.
            ok = evict(Reloaded),
            Candidates = candidate_modules(),
            log_broken_edges(broken_edges(Reloaded, Candidates)),
            log_stale_modules(stale_modules(Candidates))
    end.

evict(Mods) ->
    ok = ensure_cache(),
    lists:foreach(fun(Mod) -> true = ets:delete(?CACHE, Mod) end, Mods).

log_broken_edges(Edges) ->
    lists:foreach(fun log_broken_edge/1, Edges).

log_broken_edge({Caller, {Mod, Fun, Arity}}) ->
    logger:warning(
        "[arizona_reloader] inconsistent load: ~p calls ~p:~p/~p, which the "
        "loaded ~p no longer exports. The caller was not reloaded in this "
        "wave -- stale beam? Recompile the app and reload.",
        [Caller, Mod, Fun, Arity, Mod]
    ).

log_stale_modules(Stale) ->
    lists:foreach(fun log_stale_module/1, Stale).

log_stale_module({Mod, LoadedVsn, DiskVsn}) ->
    logger:warning(
        "[arizona_reloader] loaded module ~p is stale relative to its beam on "
        "disk (loaded vsn ~p, disk vsn ~p). Reload it so the running code "
        "matches the source.",
        [Mod, LoadedVsn, DiskVsn]
    ).

%% The disk-derived facts the checks need -- the static external-call table
%% (the `imports` chunk) and the beam's vsn (from the `attributes` chunk, the
%% same source `beam_lib:version/1` reads) -- fetched in ONE beam_lib pass and
%% cached across waves keyed by the beam file's path + mtime, so a wave
%% re-reads only modules whose beam actually changed (one stat per candidate
%% instead of two full reads). A beam can be unreadable (cover-compiled,
%% mid-write, loaded from memory, or a source-path filename after a manual
%% reload) -- that is genuinely variable input, so the module yields the same
%% skip values as an absent beam rather than aborting the whole sweep.
%% Limitation: a rewrite landing within the same mtime second as the cached
%% read of a module OUTSIDE the reloaded set (which do_check evicts) can serve
%% stale facts until that beam's next rewrite -- acceptable for a best-effort
%% advisory pass.
beam_facts(Mod) ->
    case beam_path(Mod) of
        {ok, Path} ->
            case file:read_file_info(Path, [{time, posix}]) of
                {ok, #file_info{mtime = Mtime}} ->
                    cached_beam_facts(Mod, Path, Mtime);
                {error, _Reason} ->
                    unreadable_facts()
            end;
        error ->
            unreadable_facts()
    end.

cached_beam_facts(Mod, Path, Mtime) ->
    ok = ensure_cache(),
    case ets:lookup(?CACHE, Mod) of
        [{Mod, Path, Mtime, Facts}] ->
            Facts;
        _StaleOrMissing ->
            Facts = read_beam_facts(Mod, Path),
            true = ets:insert(?CACHE, {Mod, Path, Mtime, Facts}),
            Facts
    end.

read_beam_facts(Mod, Path) ->
    case beam_lib:chunks(Path, [imports, attributes]) of
        {ok, {Mod, [{imports, Imports}, {attributes, Attrs}]}} ->
            #{imports => Imports, disk_vsn => attrs_vsn(Attrs)};
        _Other ->
            unreadable_facts()
    end.

unreadable_facts() ->
    #{imports => [], disk_vsn => unknown}.

ensure_cache() ->
    case ets:whereis(?CACHE) of
        undefined ->
            try
                _ = ets:new(?CACHE, [named_table, public, {read_concurrency, true}]),
                ok
            catch
                %% Lost the creation race to a concurrent wave; the winner's
                %% table serves this one.
                error:badarg -> ok
            end;
        _Tid ->
            ok
    end.

module_vsns(Mod) ->
    {loaded_vsn(Mod), maps:get(disk_vsn, beam_facts(Mod))}.

loaded_vsn(Mod) ->
    attrs_vsn(Mod:module_info(attributes)).

%% The vsn attribute -- present even without an explicit `-vsn`, defaulting to
%% the module checksum -- read the same way for the loaded and the disk side so
%% the staleness compare is apples-to-apples.
attrs_vsn(Attrs) ->
    case lists:keyfind(vsn, 1, Attrs) of
        {vsn, Vsn} -> Vsn;
        false -> unknown
    end.

%% A readable, non-OTP beam file for Mod, or error when there is none to read.
beam_path(Mod) ->
    case code:which(Mod) of
        Path when is_list(Path) ->
            case filename:extension(Path) of
                ".beam" ->
                    case is_otp_path(Path) of
                        true -> error;
                        false -> {ok, Path}
                    end;
                _ ->
                    error
            end;
        _NonPath ->
            %% non_existing | preloaded | cover_compiled -- no beam file to read.
            error
    end.

is_loaded(Mod) ->
    case code:is_loaded(Mod) of
        {file, _Loaded} -> true;
        false -> false
    end.

is_otp_path(Path) ->
    lists:prefix(code:root_dir() ++ "/", Path).
