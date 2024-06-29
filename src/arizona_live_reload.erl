%%
%% %CopyrightBegin%
%%
%% Copyright 2023-2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(arizona_live_reload).
-moduledoc """
Live-reload functionality for use during development.
""".

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API functions
-export([start_link/0]).
-ignore_xref([start_link/0]).
-export([reload/0]).
-ignore_xref([reload/0]).

%% gen_server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% State
-record(state, {timer, files = #{}, clients = #{}}).

%% Macros
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

reload() ->
    gen_server:cast(?SERVER, reload).

%% --------------------------------------------------------------------
%% gen_server callbacks.
%% --------------------------------------------------------------------

init([]) ->
    register_events(),
    setup_watcher(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?LOG_INFO("[LiveReload] call: ~p from ~p~n", [Request, From]),
    {reply, State, State}.

handle_cast(reload, State) ->
    ?LOG_INFO("[LiveReload] reload: ~p~n", [State]),
    reload(State#state.clients),
    {noreply, State};
handle_cast(Request, State) ->
    ?LOG_INFO("[LiveReload] cast: ~p~n", [Request]),
    {noreply, State}.

handle_info({init, Client}, #state{clients = Clients} = State) ->
    ?LOG_INFO("[LiveReload] init: ~p~n", [Client]),
    {noreply, State#state{clients = Clients#{Client => true}}};
handle_info({terminate, Client}, #state{clients = Clients} = State) ->
    ?LOG_INFO("[LiveReload] terminate: ~p~n", [Client]),
    {noreply, State#state{clients = maps:without([Client], Clients)}};
handle_info({_Pid, {fs, file_event}, {File, Events}},
            #state{timer = Timer, files = Files} = State0) ->
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
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;
handle_info(recompile, #state{files = Files} = State) when map_size(Files) > 0 ->
    ?LOG_INFO("[LiveReload] recompile: ~p~n", [State]),
    maps:foreach(fun
        (File, {erl, modified}) ->
            Mod = list_to_existing_atom(filename:basename(File, ".erl")),
            c:c(Mod)
    end, Files),
    reload(State#state.clients),
    {noreply, State#state{timer = undefined, files = #{}}};
handle_info(recompile, State) ->
    {noreply, State#state{timer = undefined}};
handle_info(Request, State) ->
    ?LOG_INFO("[LiveReload] info: ~p~n", [Request]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

register_events() ->
    arizona_websocket:subscribe(init),
    arizona_websocket:subscribe(terminate).

setup_watcher() ->
    fs:start_link(arizona_live_reload_fs, filename:absname("")),
    fs:subscribe(arizona_live_reload_fs).

reload(Clients) ->
    maps:foreach(fun(Client, _) -> Client ! reload end, Clients).

