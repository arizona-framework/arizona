-module(arizona_watcher).
-behaviour(gen_server).
-export([watch/2, broadcast/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-ignore_xref([watch/2, broadcast/1]).

-record(state, {
    abs_dir :: string(),
    fs_sup :: atom(),
    compiled :: [re:mp()],
    callback :: fun(([string()]) -> term()) | undefined,
    debounce_ms :: pos_integer(),
    debounce_timer :: reference() | undefined,
    pending_files :: sets:set(string())
}).

-spec watch(string(), map()) -> {ok, pid()} | {error, term()}.
watch(Dir, Opts) ->
    gen_server:start(?MODULE, {Dir, Opts}, []).

-spec broadcast([string()]) -> ok.
broadcast(Files) ->
    arizona_pubsub:broadcast(?MODULE, {?MODULE, Files}).

init({Dir, Opts}) ->
    case filelib:is_dir(Dir) of
        true ->
            AbsDir = filename:absname(Dir),
            Patterns = maps:get(patterns, Opts, [".*"]),
            Compiled = [
                begin
                    {ok, MP} = re:compile(P),
                    MP
                end
             || P <:- Patterns
            ],
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

%% Internal -------------------------------------------------------------------

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
