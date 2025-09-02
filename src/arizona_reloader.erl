-module(arizona_reloader).
-moduledoc ~"""
Development-time file watcher for automatic code reloading and live browser refresh.

Provides a GenServer-based file system watcher that monitors specified
directories for file changes and triggers configurable reload actions.
Designed for development environments to enable fast feedback loops
with automatic browser refresh and custom reload callbacks.

## Features

- **File System Watching**: Monitors directories and subdirectories
- **Pattern Matching**: Uses regex patterns to filter relevant files
- **Debounced Execution**: Groups rapid file changes to prevent spam
- **Custom Callbacks**: Execute custom functions on file changes
- **Browser Refresh**: Automatic live reload for connected browsers
- **Rule-Based**: Multiple watch rules with different behaviors

## Configuration

```erlang
Config = #{
    enabled => true,
    debounce_ms => 100,  % Wait 100ms before executing
    rules => [
        #{
            directories => ["src", "templates"],
            patterns => [".*\\.erl$", ".*\\.html$"],
            callback => fun(Files) -> recompile_modules(Files) end
        },
        #{
            directories => ["assets"],
            patterns => [".*\\.(css|js)$"],
            % No callback = browser refresh only
        }
    ]
}.
```

## Event Flow

1. File system event occurs (create/modify/delete)
2. Check if file matches any rule (directory + pattern)
3. Accumulate matching rules during debounce period
4. Execute custom callbacks for each rule
5. Broadcast live reload message to browsers

## Development Integration

Typically started by development server and integrated with
`arizona_websocket` for browser live reload functionality.
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

% list_to_atom/1 is acceptable here: this is development-only code with limited watch paths
% Module names derive from .erl filenames and we only reload already-loaded modules
-elvis([{elvis_style, no_common_caveats_call, #{caveat_functions => []}}]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/1]).

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
-export_type([config/0]).
-export_type([reload_rule/0]).
-export_type([rule_directory/0]).
-export_type([rule_pattern/0]).
-export_type([rule_callback/0]).
-export_type([filename/0]).
-export_type([debounce_ms/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    rules :: [reload_rule()],
    debounce_ms :: debounce_ms(),
    debounce_timer :: reference() | undefined,
    pending_rules :: #{}
}).

-opaque state() :: #state{}.
-nominal config() :: #{
    enabled := boolean(),
    rules := [reload_rule()],
    debounce_ms => debounce_ms()
}.
-nominal reload_rule() :: #{
    directories := [rule_directory()],
    patterns := [rule_pattern()],
    callback => rule_callback() | undefined
}.
-nominal rule_directory() :: string().
-nominal rule_pattern() :: string().
-nominal rule_callback() :: fun(([filename()]) -> any()).
-nominal filename() :: string().
-nominal debounce_ms() :: pos_integer().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts the file watcher with the given configuration.

Initializes file system watchers for all directories specified in
the rules configuration. Validates directories and patterns before
starting watchers.
""".
-spec start_link(Config) -> gen_server:start_ret() when
    Config :: config().
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

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
    Reply :: map().
handle_call(get_state, _From, State) ->
    % Return state as map for testing
    StateMap = #{
        rules => State#state.rules,
        debounce_ms => State#state.debounce_ms,
        debounce_timer => State#state.debounce_timer,
        pending_rules => State#state.pending_rules
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
    case should_trigger_reload(FilePath, Events, State) of
        {true, MatchingRules} ->
            accumulate_pending_rules(FilePath, MatchingRules, State);
        false ->
            {noreply, State}
    end;
% Handle debounced rule execution
handle_info({execute_rules, PendingRules}, State) ->
    case execute_pending_rules(PendingRules) of
        ok ->
            % Send simple reload message to browsers
            ok = arizona_pubsub:broadcast(~"live_reload", {file_changed, reload}),
            {noreply, State#state{debounce_timer = undefined, pending_rules = #{}}};
        error ->
            {noreply, State#state{debounce_timer = undefined, pending_rules = #{}}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% Merge user configuration with defaults
merge_config_with_defaults(Config) ->
    DefaultOpts = #{
        rules => [],
        debounce_ms => 100
    },
    maps:merge(DefaultOpts, Config).

% Validate paths and start watchers
validate_and_start_watchers(MergedConfig) ->
    #{
        rules := Rules,
        debounce_ms := DebounceMs
    } = MergedConfig,

    % Collect all directories from all rules
    AllDirectories = lists:flatmap(fun(#{directories := Dirs}) -> Dirs end, Rules),

    % Remove duplicates and filter to existing directories only
    UniqueDirs = lists:usort(AllDirectories),
    ExistingDirs = lists:filter(fun filelib:is_dir/1, UniqueDirs),

    case ExistingDirs of
        [] ->
            {error, no_valid_directories};
        ValidDirs ->
            case start_fs_watchers(ValidDirs) of
                ok ->
                    ok = wait_for_fs_ready(),
                    ConfigData = #{
                        rules => Rules,
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
        rules := Rules,
        debounce_ms := DebounceMs
    } = ConfigData,
    #state{
        rules = Rules,
        debounce_ms = DebounceMs,
        debounce_timer = undefined,
        pending_rules = #{}
    }.

% Find matching rules for a file
should_trigger_reload(FilePath, Events, State) ->
    case is_relevant_event(Events) of
        true ->
            MatchingRules = find_matching_rules(FilePath, State#state.rules),
            case MatchingRules of
                [] -> false;
                _ -> {true, MatchingRules}
            end;
        false ->
            false
    end.

% Find all rules that match the given file path
find_matching_rules(FilePath, Rules) ->
    lists:filter(
        fun(Rule) ->
            #{directories := Directories, patterns := Patterns} = Rule,

            % Check if file is in one of the rule's directories (including subdirectories)
            InDirectory = lists:any(
                fun(Dir) ->
                    % Convert relative dir to absolute for comparison
                    AbsDir = filename:absname(Dir),
                    lists:prefix(AbsDir ++ "/", FilePath) orelse
                        filename:dirname(FilePath) =:= AbsDir
                end,
                Directories
            ),

            % Check if file matches one of the patterns
            MatchesPattern = lists:any(
                fun(Pattern) ->
                    case re:run(FilePath, Pattern) of
                        {match, _} -> true;
                        nomatch -> false
                    end
                end,
                Patterns
            ),

            InDirectory andalso MatchesPattern
        end,
        Rules
    ).

% Accumulate files for matching rules during debounce period
accumulate_pending_rules(FilePath, MatchingRules, #state{pending_rules = Pending} = State) ->
    UpdatedPending = lists:foldl(
        fun(Rule, Acc) ->
            Files =
                case Acc of
                    #{Rule := ExistingFiles} -> [FilePath | ExistingFiles];
                    #{} -> [FilePath]
                end,
            Acc#{Rule => lists:usort(Files)}
        end,
        Pending,
        MatchingRules
    ),

    start_debounce_timer(UpdatedPending, State).

% Start or restart the debounce timer
start_debounce_timer(
    UpdatedPending, #state{debounce_ms = DebounceMs, debounce_timer = Timer} = State
) ->
    ok = cancel_timer(Timer),
    NewTimer = erlang:send_after(DebounceMs, self(), {execute_rules, UpdatedPending}),
    {noreply, State#state{debounce_timer = NewTimer, pending_rules = UpdatedPending}}.

% Execute all pending rules sequentially
execute_pending_rules(PendingRules) ->
    Results = [
        execute_rule_callback(Callback, Files)
     || #{callback := Callback} := Files <- PendingRules, Callback =/= undefined
    ],
    case [ok || ok <- Results] of
        [] ->
            % If no callbacks to execute, still consider it successful
            % (e.g., when all callbacks are undefined - watch-only mode)
            case maps:size(PendingRules) > 0 of
                % Rules exist but no callbacks, still reload browsers
                true -> ok;
                % No rules at all
                false -> error
            end;
        _ ->
            ok
    end.

% Execute a single rule with its files
execute_rule_callback(Callback, Files) ->
    try
        _ = Callback(Files),
        ok
    catch
        _:Reason ->
            logger:warning("Rule callback failed: ~p", [Reason]),
            error
    end.

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
                % Create short, safe atom name using hash of path
                Hash = erlang:phash2(Path),
                Name = list_to_atom("fs_watcher_" ++ integer_to_list(Hash)),
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
