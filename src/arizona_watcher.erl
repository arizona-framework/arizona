-module(arizona_watcher).
-moduledoc """
File system watcher with debouncing and pattern matching.

Wraps the `fs` library in a `gen_server` that watches a directory for
file events, debounces bursts of changes, filters by regex patterns,
and broadcasts the resulting file list via `arizona_pubsub` (and
optionally invokes a user callback).

## Usage

```erlang
1> arizona_watcher:watch("/home/me/myapp/src", #{
       patterns => [".*\\.erl$"],
       debounce => 200,
       callback => fun(Files) -> io:format("changed: ~p~n", [Files]) end
   }).
{ok, <0.123.0>}
```

Subscribers receive `{arizona_watcher, Files}` messages via the pubsub
topic named `arizona_watcher`.

## Options

- `patterns` -- list of regex strings to filter file paths (default `[".*"]`)
- `callback` -- optional `fun([string()]) -> term()` invoked synchronously
  with the changed file list before the broadcast
- `debounce` -- milliseconds to wait after the last event before flushing
  (default `100`); coalesces editor-save bursts into a single event

## Lifecycle and shutdown

The `fs` library spawns an OS process (`sh` + `inotifywait`) with the
watched directory as cwd. `terminate/2` shuts that down explicitly and
sleeps briefly so callers that delete the watched directory don't race
with the OS process's `getcwd`.
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([watch/2]).
-export([broadcast/1]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([watch/2, broadcast/1]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% Watcher names are derived from a hash of {self, dir} and registered as
%% atoms because the underlying `fs` library requires named processes.
-elvis([{elvis_style, no_common_caveats_call, disable}]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(state, {
    abs_dir :: string(),
    fs_sup :: atom(),
    compiled :: [re:mp()],
    callback :: fun(([string()]) -> term()) | undefined,
    debounce_ms :: pos_integer(),
    debounce_timer :: reference() | undefined,
    pending_files :: sets:set(string())
}).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type state() :: #state{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts a watcher process for `Dir` with the given options.

Errors with `{not_a_directory, Dir}` if the path is not an existing
directory.
""".
-spec watch(Dir, Opts) -> {ok, pid()} | {error, term()} when
    Dir :: string(),
    Opts :: map().
watch(Dir, Opts) ->
    gen_server:start(?MODULE, {Dir, Opts}, []).

-doc """
Broadcasts a `{arizona_watcher, Files}` message on the
`arizona_watcher` pubsub topic.

Called internally after debounce expires; exposed so callers can
manually trigger a broadcast (useful in tests).
""".
-spec broadcast(Files) -> ok when
    Files :: [string()].
broadcast(Files) ->
    arizona_pubsub:broadcast(?MODULE, {?MODULE, Files}).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({Dir, Opts}) -> {ok, state()} | {stop, term()} when
    Dir :: string(),
    Opts :: map().
init({Dir, Opts}) ->
    case filelib:is_dir(Dir) of
        true ->
            AbsDir = filename:absname(Dir),
            Patterns = maps:get(patterns, Opts, [".*"]),
            Compiled = [compile_pattern(P) || P <:- Patterns],
            Callback = maps:get(callback, Opts, undefined),
            DebounceMs = maps:get(debounce, Opts, 100),
            Name = watcher_name(AbsDir),
            SupName = list_to_atom(atom_to_list(Name) ++ "sup"),
            {ok, _} = fs:start_link(Name, AbsDir),
            fs:subscribe(Name),
            {ok, #state{
                abs_dir = AbsDir,
                fs_sup = SupName,
                compiled = Compiled,
                callback = Callback,
                debounce_ms = DebounceMs,
                debounce_timer = undefined,
                pending_files = sets:new([{version, 2}])
            }};
        false ->
            {stop, {not_a_directory, Dir}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Pid, {fs, file_event}, {FilePath, Events}}, State) ->
    case is_relevant_event(Events) andalso file_matches(FilePath, State) of
        true ->
            Pending = sets:add_element(FilePath, State#state.pending_files),
            {noreply, restart_debounce(State#state{pending_files = Pending})};
        false ->
            {noreply, State}
    end;
handle_info(debounce_fire, #state{pending_files = Pending} = State) ->
    case sets:size(Pending) of
        0 ->
            {noreply, State#state{debounce_timer = undefined}};
        _ ->
            Files = sets:to_list(Pending),
            call_callback(State#state.callback, Files),
            broadcast(Files),
            {noreply, State#state{
                debounce_timer = undefined,
                pending_files = sets:new([{version, 2}])
            }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{fs_sup = SupName}) ->
    case erlang:whereis(SupName) of
        undefined ->
            ok;
        Pid ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 ->
                exit(Pid, kill),
                receive
                    {'DOWN', Ref, process, Pid, _} -> ok
                after 1000 -> ok
                end
            end
    end,
    %% The fs port spawns an OS process (sh + inotifywait) with the
    %% watched directory as cwd. port_close sends SIGHUP but the OS
    %% process may still be exiting. Give it a moment so that callers
    %% who delete the watched directory don't race with its getcwd.
    timer:sleep(10).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

compile_pattern(Pattern) ->
    {ok, MP} = re:compile(Pattern),
    MP.

watcher_name(AbsDir) ->
    Hash = erlang:phash2({self(), AbsDir}),
    list_to_atom("arizona_watcher_fs_" ++ integer_to_list(Hash)).

is_relevant_event(Events) ->
    lists:member(created, Events) orelse
        lists:member(modified, Events) orelse
        lists:member(deleted, Events).

file_matches(FilePath, #state{abs_dir = AbsDir, compiled = Compiled}) ->
    in_directory(FilePath, AbsDir) andalso matches_pattern(FilePath, Compiled).

in_directory(FilePath, AbsDir) ->
    lists:prefix(AbsDir ++ "/", FilePath).

matches_pattern(FilePath, Compiled) ->
    lists:any(
        fun(MP) ->
            re:run(FilePath, MP) =/= nomatch
        end,
        Compiled
    ).

restart_debounce(#state{debounce_timer = OldTimer, debounce_ms = Ms} = State) ->
    cancel_timer(OldTimer),
    NewTimer = erlang:send_after(Ms, self(), debounce_fire),
    State#state{debounce_timer = NewTimer}.

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

call_callback(undefined, _Files) -> ok;
call_callback(Fun, Files) -> Fun(Files).
