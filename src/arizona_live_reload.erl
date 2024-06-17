-module(arizona_live_reload).
-behaviour(gen_server).

-export([start_link/1, recompile/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {timer, files = #{}, clients = #{}}).

-define(SERVER, ?MODULE).

start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?SERVER, Opts, []).

recompile() ->
    r3:do(compile).

init(_Opts) ->
    register_events(),
    setup_watcher(),
    {ok, #state{}}.

register_events() ->
    arizona_websocket:subscribe(init),
    arizona_websocket:subscribe(terminate).

setup_watcher() ->
    fs:start_link(?MODULE, filename:absname("")),
    fs:subscribe(?MODULE).

handle_call(Request, From, State) ->
    io:format("[LiveReload] call: ~p from ~p~n", [Request, From]),
    {reply, State, State}.

handle_cast(Request, State) ->
    io:format("[LiveReload] cast: ~p~n", [Request]),
    {noreply, State}.

handle_info({init, Client}, #state{clients = Clients} = State) ->
    io:format("[LiveReload] init: ~p~n", [Client]),
    {noreply, State#state{clients = Clients#{Client => true}}};
handle_info({terminate, Client}, #state{clients = Clients} = State) ->
    io:format("[LiveReload] terminate: ~p~n", [Client]),
    {noreply, State#state{clients = maps:without([Client], Clients)}};
handle_info({_Pid, {fs, file_event}, {File, Events}}, #state{timer = Timer, files = Files} = State0) ->
    % Try recompile only when the last modified file was received.
    case Timer =:= undefined of
        true ->
            ok;
        false ->
            erlang:cancel_timer(Timer)
    end,
    State = State0#state{timer = erlang:send_after(25, self(), recompile)},
    case filename:extension(File) of
        ".erl" ->
            case Events of
                [modified, closed] ->
                    {noreply, State#state{files = Files#{File => {erl, modified}}}};
                _ ->
                    % TODO: Act on created, renamed, deleted.
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;
handle_info(recompile, #state{files = Files} = State) when map_size(Files) > 0 ->
    io:format("[LiveReload] recompile: ~p~n", [State]),
    maps:foreach(fun
        (File, {erl, modified}) ->
            Mod = list_to_existing_atom(filename:basename(File, ".erl")),
            c:c(Mod)
    end, Files),
    maps:foreach(fun(Client, _) -> Client ! reload end, State#state.clients),
    {noreply, State#state{timer = undefined, files = #{}}};
handle_info(recompile, State) ->
    {noreply, State#state{timer = undefined}};
handle_info(Request, State) ->
    io:format("[LiveReload] info: ~p~n", [Request]),
    {noreply, State}.

