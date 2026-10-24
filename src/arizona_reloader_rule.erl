-module(arizona_reloader_rule).
-moduledoc """
Runs one dev-reloader rule: watches a directory through the configured
`arizona_watcher` implementation, filters the changed paths by the rule's
patterns, debounces bursts, and hands the resulting file list to the rule's
callback and the `arizona_watcher` pubsub topic.

`arizona_reloader_sup` runs one per rule of the `reloader` application env
(`child_specs/1`); see `arizona_watcher` for the config and the watcher
contract.

## Usage

```erlang
1> arizona_reloader_rule:start_link("/home/me/myapp/src", #{
       watcher => arizona_watcher_fs,
       patterns => [".*\\.erl$"],
       debounce => 200,
       callback => fun arizona_reloader:reload_erl/1
   }).
{ok, <0.123.0>}
```

Subscribers of the `arizona_watcher` pubsub topic receive
`{arizona_watcher, Dir, Files}` after each debounced burst, `Dir` being the
rule's absolute directory, so one subscriber can tell the rules apart.

## Options

- `watcher` -- the `arizona_watcher` implementation: a module, or
  `{Module, Opts}` to pass it options (required)
- `patterns` -- list of regex strings to filter file paths (default `[".*"]`)
- `callback` -- optional `fun([string()]) -> term()` invoked synchronously
  with the changed file list before the broadcast
- `debounce` -- milliseconds to wait after the last change before flushing
  (default `100`); coalesces editor-save bursts into a single event
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/2]).
-export([child_specs/1]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% --------------------------------------------------------------------
%% Error formatting exports
%% --------------------------------------------------------------------

-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start_link/2, format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([watcher/0]).
-export_type([callback/0]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(state, {
    abs_dir :: string(),
    watcher :: module(),
    watcher_state :: term(),
    compiled :: [re:mp()],
    callback :: callback() | undefined,
    debounce_ms :: pos_integer(),
    debounce_timer :: reference() | undefined,
    pending_files :: sets:set(string())
}).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type state() :: #state{}.

-nominal watcher() :: module() | {module(), map()}.
-nominal callback() :: fun(([string()]) -> term()).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts a rule process for `Dir` with the given options.

Errors with `{not_a_directory, Dir}` if the path is not an existing
directory.
""".
-spec start_link(Dir, Opts) -> gen_server:start_ret() when
    Dir :: string(),
    Opts :: #{watcher := watcher(), _ => term()}.
start_link(Dir, Opts) ->
    case filelib:is_dir(Dir) of
        true -> gen_server:start_link(?MODULE, {Dir, Opts}, []);
        false -> {error, {not_a_directory, Dir}}
    end.

-doc """
The child specs for the `reloader` application env: one rule process per rule
when it holds `enabled => true` and a non-empty `rules` list, none otherwise.

Raises a formatted error when there are rules to run but the `watcher` key is
missing, names a module that is not on the code path (the package providing it
is not a dependency of this build), or names one that does not implement
`arizona_watcher`, and when a rule has no `directory`.
""".
-spec child_specs(Reloader) -> [supervisor:child_spec()] when
    Reloader :: map().
child_specs(#{enabled := true, rules := [_ | _] = Rules} = Reloader) ->
    Watcher = watcher(Reloader),
    [child_spec(I, Rule, Watcher) || {I, Rule} <- lists:enumerate(Rules)];
child_specs(#{}) ->
    [].

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({Dir, Opts}) -> {ok, state()} | {stop, term()} when
    Dir :: string(),
    Opts :: map().
init({Dir, Opts}) ->
    AbsDir = filename:absname(Dir),
    proc_lib:set_label({?MODULE, AbsDir}),
    %% Trapped so the watcher's terminate/1 runs on a supervisor shutdown too,
    %% which would otherwise kill this process without calling terminate/2.
    process_flag(trap_exit, true),
    {Watcher, WatcherOpts} = normalize_watcher(maps:get(watcher, Opts)),
    case Watcher:init(AbsDir, WatcherOpts) of
        {ok, WatcherState} ->
            {ok, #state{
                abs_dir = AbsDir,
                watcher = Watcher,
                watcher_state = WatcherState,
                compiled = [compile_pattern(P) || P <- maps:get(patterns, Opts, [".*"])],
                callback = maps:get(callback, Opts, undefined),
                debounce_ms = maps:get(debounce, Opts, 100),
                debounce_timer = undefined,
                pending_files = sets:new([{version, 2}])
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(debounce_fire, #state{abs_dir = AbsDir, pending_files = Pending} = State) ->
    case sets:size(Pending) of
        0 ->
            {noreply, State#state{debounce_timer = undefined}};
        _ ->
            Files = sets:to_list(Pending),
            call_callback(State#state.callback, Files),
            broadcast(AbsDir, Files),
            {noreply, State#state{
                debounce_timer = undefined,
                pending_files = sets:new([{version, 2}])
            }}
    end;
%% Trapping exits turns a crash of whatever the watcher linked to this process
%% into a message; stop with its reason, as the link alone would have, so a dead
%% watch restarts instead of silently reporting nothing. The supervisor's own
%% exit never gets here (gen_server handles its parent's).
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, #state{watcher = Watcher, watcher_state = WatcherState0} = State0) ->
    case Watcher:handle_info(Info, WatcherState0) of
        {ok, Paths, WatcherState} ->
            changed(Paths, State0#state{watcher_state = WatcherState});
        {stop, Reason, WatcherState} ->
            {stop, Reason, State0#state{watcher_state = WatcherState}}
    end.

terminate(_Reason, #state{watcher = Watcher, watcher_state = WatcherState}) ->
    case erlang:function_exported(Watcher, terminate, 1) of
        true -> Watcher:terminate(WatcherState);
        false -> ok
    end.

%% --------------------------------------------------------------------
%% Error formatting
%% --------------------------------------------------------------------

-doc """
Formats the `reloader` config errors `child_specs/1` raises into an actionable
boot message. Picked up by `erl_error:format_exception/3` via the `error_info`
annotation at the raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(no_watcher, _ST) ->
    #{
        general =>
            "the arizona `reloader` env has rules but no `watcher` to run them; add an "
            "`arizona_watcher` implementation to a dev-only rebar3 profile (e.g. "
            "`{deps, [arizona_watcher_fs]}`) and set `watcher => arizona_watcher_fs`"
    };
format_error({watcher_not_found, Mod}, _ST) ->
    #{
        general =>
            io_lib:format(
                "the arizona `reloader` watcher ~ts is not on the code path; add the "
                "package that provides it to the deps of the rebar3 profile this node "
                "runs (a dev-only profile keeps it out of production)",
                [Mod]
            )
    };
format_error({watcher_invalid, Watcher}, _ST) ->
    #{
        general =>
            io_lib:format(
                "the arizona `reloader` watcher ~tp is not an `arizona_watcher` "
                "implementation; give a module exporting init/2 and handle_info/2, or "
                "`{Module, Opts}` with an Opts map",
                [Watcher]
            )
    };
format_error({rule_without_directory, Rule}, _ST) ->
    #{
        general =>
            io_lib:format(
                "the arizona `reloader` rule ~tp has no `directory`; every rule names "
                "the directory it watches (`#{directory => \"src\", ...}`)",
                [Rule]
            )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

watcher(#{watcher := Watcher} = Reloader) ->
    {Mod, _Opts} = normalize_watcher(Watcher),
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            case implements_watcher(Mod) of
                true -> Watcher;
                false -> config_error({watcher_invalid, Watcher}, Reloader)
            end;
        {error, _} ->
            config_error({watcher_not_found, Mod}, Reloader)
    end;
watcher(Reloader) ->
    config_error(no_watcher, Reloader).

implements_watcher(Mod) ->
    erlang:function_exported(Mod, init, 2) andalso
        erlang:function_exported(Mod, handle_info, 2).

config_error(Reason, Arg) ->
    erlang:error(Reason, [Arg], [{error_info, #{module => ?MODULE}}]).

normalize_watcher(Mod) when is_atom(Mod) ->
    {Mod, #{}};
normalize_watcher({Mod, Opts} = Watcher) when is_atom(Mod), is_map(Opts) ->
    Watcher;
normalize_watcher(Watcher) ->
    config_error({watcher_invalid, Watcher}, Watcher).

child_spec(I, #{directory := Dir} = Rule, Watcher) ->
    Opts = maps:without([directory], Rule),
    #{
        id => {?MODULE, I},
        start => {?MODULE, start_link, [Dir, Opts#{watcher => Watcher}]},
        type => worker
    };
child_spec(_I, Rule, _Watcher) ->
    config_error({rule_without_directory, Rule}, Rule).

changed(Paths, #state{compiled = Compiled} = State) ->
    case [Path || Path <- Paths, matches_pattern(Path, Compiled)] of
        [] ->
            {noreply, State};
        Matched ->
            New = sets:from_list(Matched, [{version, 2}]),
            Pending = sets:union(State#state.pending_files, New),
            {noreply, restart_debounce(State#state{pending_files = Pending})}
    end.

broadcast(AbsDir, Files) ->
    arizona_pubsub:broadcast(arizona_watcher, {arizona_watcher, AbsDir, Files}).

compile_pattern(Pattern) ->
    {ok, MP} = re:compile(Pattern),
    MP.

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
    case erlang:cancel_timer(Ref) of
        false ->
            %% The timer already fired: its `debounce_fire` is sitting in the
            %% mailbox. Flush it so a stale fire can't trigger the debounce early
            %% right after we arm a fresh timer, which would defeat coalescing
            %% (and possibly recompile a half-written file).
            receive
                debounce_fire -> ok
            after 0 -> ok
            end;
        _RemainingMs ->
            ok
    end.

call_callback(undefined, _Files) -> ok;
call_callback(Fun, Files) -> Fun(Files).
