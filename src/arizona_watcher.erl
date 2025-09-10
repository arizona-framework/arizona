-module(arizona_watcher).
-moduledoc ~"""
Generic file system watcher for custom automation.

Provides a standalone GenServer-based file system watcher that monitors
specified directories for file changes and triggers custom callback functions.
Designed as a building block for developers to create their own automation
tools and workflows.

## Features

- **File System Watching**: Monitors directories and subdirectories
- **Pattern Matching**: Uses regex patterns to filter relevant files
- **Debounced Execution**: Groups rapid file changes to prevent spam
- **Custom Callbacks**: Execute any custom function on file changes
- **Standalone Operation**: No framework dependencies
- **Manual Control**: Start/stop instances as needed

## Configuration

```erlang
Config = #{
    directories => ["src", "templates"],
    patterns => [".*\\.erl$", ".*\\.html$"],  % Optional: defaults to [".*"] (all files)
    callback => fun(Files) ->
        io:format("Files changed: ~p~n", [Files]),
        my_custom_result  % Can return anything
    end,
    debounce_ms => 100  % Optional: defaults to 100ms
}.
```

## Callback Function

Callback functions receive a list of changed files and can return any value.
The watcher continues running regardless of the callback's return value.

## Event Flow

1. File system event occurs (create/modify/delete)
2. Check if file matches directory and pattern criteria
3. Accumulate matching files during debounce period
4. Execute custom callback with list of changed files
5. Continue watching for more changes

## Usage Examples

```erlang
% Start a watcher for Erlang files
{ok, Pid} = arizona_watcher:start_link(#{
    directories => ["src"],
    patterns => [".*\\.erl$"],
    callback => fun(Files) ->
        % Your custom logic here
        io:format("Compiling ~p~n", [Files]),
        ok
    end
}).

% Start multiple independent watchers
{ok, ErlangWatcher} = arizona_watcher:start_link(ErlangConfig),
{ok, AssetWatcher} = arizona_watcher:start_link(AssetConfig).
```
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

% list_to_atom/1 is acceptable here: atoms created from process/path hashes
-elvis([{elvis_style, no_common_caveats_call, #{caveat_functions => []}}]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/1]).
-export([get_state/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start_link/1]).
-ignore_xref([get_state/1]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).
-export_type([state_map/0]).
-export_type([config/0]).
-export_type([directory/0]).
-export_type([watcher_pattern/0]).
-export_type([watcher_callback/0]).
-export_type([watcher_callback_result/0]).
-export_type([filename/0]).
-export_type([debounce_ms/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    directories :: [directory()],
    % Precomputed absolute directories
    abs_directories :: [directory()],
    patterns :: [watcher_pattern()],
    % Precompiled regex patterns
    compiled_patterns :: [re:mp()],
    callback :: watcher_callback(),
    debounce_ms :: debounce_ms(),
    debounce_timer :: reference() | undefined,
    % Use set to avoid duplicates
    pending_files :: sets:set(filename())
}).

-opaque state() :: #state{}.
-nominal state_map() :: #{
    directories := [directory()],
    abs_directories := [directory()],
    patterns := [watcher_pattern()],
    compiled_patterns := [re:mp()],
    callback := watcher_callback(),
    debounce_ms := debounce_ms(),
    debounce_timer := reference() | undefined,
    pending_files := [filename()]
}.
-nominal config() :: #{
    directories := [directory()],
    patterns => [watcher_pattern()],
    callback := watcher_callback(),
    debounce_ms => debounce_ms()
}.
-nominal directory() :: string().
-nominal watcher_pattern() :: string().
-nominal watcher_callback() :: fun(([filename()]) -> watcher_callback_result()).
-nominal watcher_callback_result() :: dynamic().
-nominal filename() :: string().
-nominal debounce_ms() :: non_neg_integer().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts a file watcher with the given configuration.

Initializes file system watchers for all directories specified.
Validates directories and patterns before starting watchers.
Returns {ok, Pid} for the new watcher instance.
""".
-spec start_link(Config) -> gen_server:start_ret() when
    Config :: config().
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-spec get_state(Pid) -> StateMap when
    Pid :: pid(),
    StateMap :: state_map().
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% --------------------------------------------------------------------
%% gen_server callback definitions
%% --------------------------------------------------------------------

-spec init(Config) -> {ok, State} | {stop, Reason} when
    Config :: config(),
    State :: state(),
    Reason :: term().
init(Config) ->
    MergedConfig = merge_config_with_defaults(Config),
    case validate_and_start_watchers(MergedConfig) of
        {ok, ConfigData} ->
            State = create_initial_state(ConfigData),
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(Request, From, State) -> {reply, Reply, State} when
    Request :: term(),
    From :: {pid(), term()},
    State :: state(),
    Reply :: state_map().
handle_call(get_state, _From, State) ->
    % Return state as map for testing
    StateMap = #{
        directories => State#state.directories,
        abs_directories => State#state.abs_directories,
        patterns => State#state.patterns,
        compiled_patterns => State#state.compiled_patterns,
        callback => State#state.callback,
        debounce_ms => State#state.debounce_ms,
        debounce_timer => State#state.debounce_timer,
        pending_files => sets:to_list(State#state.pending_files)
    },
    {reply, StateMap, State}.

-spec handle_cast(Request, State) -> {noreply, State} when
    Request :: term(),
    State :: state().
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, State} when
    Info :: term(),
    State :: state().
handle_info({_Pid, {fs, file_event}, {FilePath, Events}}, State) ->
    case should_trigger_callback(FilePath, Events, State) of
        true ->
            accumulate_pending_files(FilePath, State);
        false ->
            {noreply, State}
    end;
% Handle debounced callback execution
handle_info({execute_callback, PendingFiles}, State) ->
    execute_and_clear_pending(PendingFiles, State#state{debounce_timer = undefined});
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% Merge user configuration with defaults
merge_config_with_defaults(Config) ->
    DefaultOpts = #{
        % Match all files by default
        patterns => [".*"],
        debounce_ms => 100
    },
    maps:merge(DefaultOpts, Config).

% Validate configuration and start watchers
validate_and_start_watchers(MergedConfig) ->
    #{
        directories := Directories,
        patterns := Patterns,
        callback := Callback,
        debounce_ms := DebounceMs
    } = MergedConfig,

    % Filter to existing directories and warn about missing ones
    {ExistingDirs, MissingDirs} = lists:partition(fun filelib:is_dir/1, Directories),

    % Warn about missing directories
    lists:foreach(
        fun(Dir) -> logger:warning("Directory does not exist: ~s", [Dir]) end,
        MissingDirs
    ),

    case ExistingDirs of
        [] ->
            {error, no_valid_directories};
        ValidDirs ->
            case start_fs_watchers(ValidDirs) of
                ok ->
                    ok = wait_for_fs_ready(),
                    ConfigData = #{
                        directories => ValidDirs,
                        patterns => Patterns,
                        callback => Callback,
                        debounce_ms => DebounceMs
                    },
                    {ok, ConfigData};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Create initial state record
create_initial_state(ConfigData) ->
    #{
        directories := Directories,
        patterns := Patterns,
        callback := Callback,
        debounce_ms := DebounceMs
    } = ConfigData,

    % Precompute absolute directories for fast matching
    AbsDirectories = [filename:absname(Dir) || Dir <- Directories],

    % Precompile regex patterns for fast matching
    CompiledPatterns = [compile_pattern(Pattern) || Pattern <- Patterns],

    #state{
        directories = Directories,
        abs_directories = AbsDirectories,
        patterns = Patterns,
        compiled_patterns = CompiledPatterns,
        callback = Callback,
        debounce_ms = DebounceMs,
        debounce_timer = undefined,
        pending_files = sets:new([{version, 2}])
    }.

% Check if file should trigger callback
should_trigger_callback(FilePath, Events, State) ->
    case is_relevant_event(Events) of
        true ->
            file_matches_watcher(FilePath, State);
        false ->
            false
    end.

% Check if file matches this watcher's criteria
file_matches_watcher(FilePath, #state{
    abs_directories = AbsDirectories, compiled_patterns = CompiledPatterns
}) ->
    % Check if file is in one of the directories (including subdirectories)
    InDirectory = lists:any(
        fun(AbsDir) ->
            lists:prefix(AbsDir ++ "/", FilePath) orelse
                filename:dirname(FilePath) =:= AbsDir
        end,
        AbsDirectories
    ),

    % Check if file matches one of the precompiled patterns
    MatchesPattern = lists:any(
        fun(CompiledPattern) ->
            case re:run(FilePath, CompiledPattern) of
                {match, _} -> true;
                nomatch -> false
            end
        end,
        CompiledPatterns
    ),

    InDirectory andalso MatchesPattern.

% Accumulate files during debounce period
accumulate_pending_files(FilePath, #state{pending_files = Pending} = State) ->
    UpdatedPending = sets:add_element(FilePath, Pending),
    start_debounce_timer(UpdatedPending, State).

% Start or restart the debounce timer
start_debounce_timer(
    UpdatedPending, #state{debounce_ms = 0} = State
) ->
    execute_and_clear_pending(UpdatedPending, State);
start_debounce_timer(
    UpdatedPending, #state{debounce_ms = DebounceMs, debounce_timer = Timer} = State
) ->
    ok = cancel_timer(Timer),
    NewTimer = erlang:send_after(DebounceMs, self(), {execute_callback, UpdatedPending}),
    {noreply, State#state{debounce_timer = NewTimer, pending_files = UpdatedPending}}.

% Execute callback and clear pending files
execute_and_clear_pending(PendingFiles, State) ->
    FilesList = sets:to_list(PendingFiles),
    _Result = execute_callback(State#state.callback, FilesList),
    {noreply, State#state{pending_files = sets:new([{version, 2}])}}.

% Execute callback with pending files
execute_callback(Callback, Files) ->
    apply(Callback, [Files]).

% Check if this is a relevant file system event
is_relevant_event(Events) ->
    lists:member(created, Events) orelse
        lists:member(modified, Events) orelse
        lists:member(deleted, Events).

% Cancel timer safely
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef),
    ok.

% Start fs watchers for all watch paths
start_fs_watchers(WatchPaths) ->
    try
        lists:foreach(
            fun(Path) ->
                % Create unique atom name using process and path hash
                ProcessHash = erlang:phash2(self()),
                PathHash = erlang:phash2(Path),
                Name = list_to_atom(
                    "arizona_watcher_" ++ integer_to_list(ProcessHash) ++ "_" ++
                        integer_to_list(PathHash)
                ),
                case fs:start_link(Name, Path) of
                    {ok, _Pid} ->
                        ok = fs:subscribe(Name);
                    {error, {already_started, _Pid}} ->
                        ok;
                    Error ->
                        throw({fs_start_failed, Path, Error})
                end
            end,
            WatchPaths
        )
    catch
        throw:Reason -> {error, Reason}
    end.

% Give fs watcher time to initialize
wait_for_fs_ready() ->
    timer:sleep(100).

% Compile regex pattern for fast matching
compile_pattern(Pattern) ->
    {ok, MP} = re:compile(Pattern),
    MP.
