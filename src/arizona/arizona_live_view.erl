%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc LiveView.

%% Copyright 2023 William Fank Thomé
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
-module(arizona_live_view).

%% API
-export([ mount/3, render_state/2, handle_event/4 ]).

%%%=====================================================================
%%% API
%%%=====================================================================

mount(View, Params, Socket) ->
    io:format("[LiveView] ~w: ~p~n", [View, [mount, Params]]),
    case erlang:function_exported(View, mount, 2) of
        true ->
            View:mount(Params, Socket);
        false ->
            {ok, Socket}
    end.

render_state(View, Bindings) ->
    io:format("[LiveView] ~w: ~p~n", [View, [render, Bindings]]),
    View:render(Bindings).

handle_event(View, Event, Payload, Socket) ->
    io:format("[LiveView] ~w: ~p~n", [View, [event, Event, Payload]]),
    View:handle_event(Event, Payload, Socket).
