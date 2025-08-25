-module(arizona_reloader).
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
-export_type([watch_paths/0]).
-export_type([file_patterns/0]).
-export_type([debounce_ms/0]).
-export_type([reload_command/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    watch_paths :: watch_paths(),
    file_patterns :: file_patterns(),
    debounce_ms :: debounce_ms(),
    debounce_timer :: reference() | undefined,
    reload_command :: reload_command() | undefined
}).

-opaque state() :: #state{}.
-nominal config() :: #{
    enabled := boolean(),
    watch_paths => watch_paths(),
    file_patterns => file_patterns(),
    debounce_ms => debounce_ms(),
    reload_command => reload_command()
}.
-nominal watch_paths() :: [string()].
-nominal file_patterns() :: [binary()].
-nominal debounce_ms() :: pos_integer().
-nominal reload_command() :: string().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

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
        {ok, ValidPaths, ConfigData} ->
            State = create_initial_state(ValidPaths, ConfigData),
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
        watch_paths => State#state.watch_paths,
        file_patterns => State#state.file_patterns,
        debounce_ms => State#state.debounce_ms,
        reload_command => State#state.reload_command,
        debounce_timer => State#state.debounce_timer
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
        true ->
            debounced_broadcast(FilePath, State);
        false ->
            {noreply, State}
    end;
% Handle debounced reload message
handle_info({send_reload, FilePath}, #state{reload_command = Command} = State) ->
    % Run reload command if configured
    case run_reload_command(Command) of
        ok ->
            % Reload the changed module
            reload_changed_module(FilePath),

            % Send reload message to browsers
            ok = arizona_pubsub:broadcast(~"live_reload", {file_changed, FilePath}),

            % Clear the timer from state
            {noreply, State#state{debounce_timer = undefined}};
        {error, Reason} ->
            % Log error but continue with reload
            logger:warning("Reload command failed: ~s~nReason: ~s", [Command, Reason]),

            % Clear the timer from state
            {noreply, State#state{debounce_timer = undefined}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% Merge user configuration with defaults
merge_config_with_defaults(Config) ->
    DefaultOpts = #{
        watch_paths => ["include", "src", "c_src"],
        file_patterns => [".*\\.erl$", ".*\\.hrl$", ".*\\.src$", ".*\\.config$", ".*\\.lock$"],
        debounce_ms => 100,
        reload_command => "rebar3 compile"
    },
    maps:merge(DefaultOpts, Config).

% Validate paths and start watchers
validate_and_start_watchers(MergedConfig) ->
    #{
        watch_paths := WatchPaths,
        file_patterns := FilePatterns,
        debounce_ms := DebounceMs,
        reload_command := ReloadCommand
    } = MergedConfig,

    % Filter to existing directories only
    ExistingPaths = lists:filter(fun filelib:is_dir/1, WatchPaths),

    case ExistingPaths of
        [] ->
            {error, no_valid_watch_paths};
        ValidPaths ->
            case start_fs_watchers(ValidPaths) of
                ok ->
                    ok = wait_for_fs_ready(),
                    ConfigData = #{
                        file_patterns => FilePatterns,
                        debounce_ms => DebounceMs,
                        reload_command => ReloadCommand
                    },
                    {ok, ValidPaths, ConfigData};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Create initial state record
create_initial_state(ValidPaths, ConfigData) ->
    #{
        file_patterns := FilePatterns,
        debounce_ms := DebounceMs,
        reload_command := ReloadCommand
    } = ConfigData,
    #state{
        watch_paths = ValidPaths,
        file_patterns = FilePatterns,
        debounce_ms = DebounceMs,
        debounce_timer = undefined,
        reload_command = ReloadCommand
    }.

% Implement file pattern matching
should_trigger_reload(FilePath, Events, State) ->
    is_relevant_event(Events) andalso matches_patterns(FilePath, State).

% Check if this is a relevant file system event
is_relevant_event(Events) ->
    lists:member(created, Events) orelse
        lists:member(modified, Events) orelse
        lists:member(deleted, Events).

% Check if file matches any of the configured patterns
matches_patterns(FilePath, #state{file_patterns = Patterns}) ->
    lists:any(
        fun(Pattern) ->
            case re:run(FilePath, Pattern) of
                {match, _} -> true;
                nomatch -> false
            end
        end,
        Patterns
    ).

% Handle debouncing of file change events
debounced_broadcast(FilePath, #state{debounce_ms = DebounceMs, debounce_timer = Timer} = State) ->
    % Cancel any existing timer
    ok = cancel_timer(Timer),

    % Start new debounce timer
    NewTimer = erlang:send_after(DebounceMs, self(), {send_reload, FilePath}),

    % Update state with new timer
    {noreply, State#state{debounce_timer = NewTimer}}.

% Cancel timer safely
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef),
    ok.

% Reload a specific module based on file path
reload_changed_module(FilePath) ->
    case filename:extension(FilePath) of
        ".erl" ->
            % Extract module name from file path
            BaseName = filename:basename(FilePath, ".erl"),
            Module = list_to_existing_atom(BaseName),

            % Only reload if module is currently loaded
            case code:is_loaded(Module) of
                {file, _} ->
                    logger:info("Reloading module: ~p", [Module]),
                    code:purge(Module),
                    case code:load_file(Module) of
                        {module, Module} ->
                            ok;
                        {error, Reason} ->
                            logger:warning("Failed to reload module ~p: ~p", [Module, Reason])
                    end;
                false ->
                    % Module not loaded, nothing to do
                    ok
            end;
        _ ->
            % Not an Erlang file, nothing to reload
            ok
    end.

% Run the configured reload command
run_reload_command(Command) ->
    case Command of
        undefined ->
            ok;
        Command ->
            try
                case os:cmd(Command) of
                    [] ->
                        ok;
                    Output ->
                        logger:info("Reload command output: ~s", [Output]),
                        ok
                end
            catch
                _:Reason ->
                    {error, Reason}
            end
    end.

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
