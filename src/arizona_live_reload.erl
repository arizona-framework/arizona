-module(arizona_live_reload).
-moduledoc """
Live-reload functionality for use during development.
""".
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([reload/0]).

%

-ignore_xref([start_link/0]).
-ignore_xref([reload/0]).

%% --------------------------------------------------------------------
%% Behaviour (gen_server) callbacks
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(state, {
    timer :: undefined | reference(),
    files :: #{string() := {erl, modified}},
    clients :: #{pid() := boolean()}
}).
-opaque state() :: #state{}.
-export_type([state/0]).
-elvis([{elvis_style, state_record_and_type, disable}]). % opaque not identified as "type"

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

-spec reload() -> ok.
reload() ->
    gen_server:cast(?SERVER, reload).

%% --------------------------------------------------------------------
%% Behaviour (gen_server) callbacks
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, State}
    when Args :: term(),
         State :: state().
init(_Args) ->
    register_events(),
    setup_watcher(),
    {ok, #state{
        files = #{},
        clients = #{}
    }}.

-spec handle_call(Request, From, State) -> no_return()
    when Request :: term(),
         From :: gen_server:from(),
         State :: state().
handle_call(_Request, _From, _State) ->
    exit(not_implemented).

-spec handle_cast(Request, State1) -> {noreply, State2}
    when Request :: term(),
         State1 :: state(),
         State2 :: state().
handle_cast(reload, State) ->
    reload(State#state.clients),
    {noreply, State}.

-spec handle_info(Info, State1) -> {noreply, State2}
    when Info :: term(),
         State1 :: state(),
         State2 :: state().
handle_info({init, Client}, #state{clients = Clients} = State) ->
    {noreply, State#state{clients = Clients#{Client => true}}};
handle_info({terminate, Client}, #state{clients = Clients} = State) ->
    {noreply, State#state{clients = maps:without([Client], Clients)}};
handle_info({_Pid, {fs, file_event}, {File, Events}},
            #state{timer = Timer, files = Files} = State0) ->
    % Try recompile only when the last modified file was received.
    Timer =/= undefined andalso erlang:cancel_timer(Timer),
    State = State0#state{timer = erlang:send_after(25, self(), recompile)},
    case filename:extension(File) of
        ".erl" ->
            case Events of
                [modified, closed] ->
                    {noreply, State#state{files = Files#{File => {erl, modified}}}};
                _ ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;
handle_info(recompile, #state{files = Files} = State) when map_size(Files) > 0 ->
    maps:foreach(fun
        (File, {erl, modified}) ->
            Mod = list_to_existing_atom(filename:basename(File, ".erl")),
            c:c(Mod)
    end, Files),
    reload(State#state.clients),
    {noreply, State#state{timer = undefined, files = #{}}};
handle_info(recompile, State) ->
    {noreply, State#state{timer = undefined}};
handle_info(_Request, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

register_events() ->
    arizona_websocket:subscribe(init),
    arizona_websocket:subscribe(terminate).

setup_watcher() ->
    {ok, _} = fs:start_link(arizona_live_reload_fs, filename:absname("")),
    fs:subscribe(arizona_live_reload_fs).

reload(Clients) ->
    maps:foreach(fun(Client, _) -> Client ! reload end, Clients).
