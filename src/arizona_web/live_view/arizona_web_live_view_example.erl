%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc LiveView example.

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
-module(arizona_web_live_view_example).

-behaviour(arizona_live_view).

%% arizona_web_live_view callbacks
-export([ mount/2, render/1, handle_event/3 ]).

%% Libs
-include("arizona_live_view.hrl").

%%%=====================================================================
%%% arizona_web_live_view callbacks
%%%=====================================================================

mount(_Params, Socket0) ->
    Socket = arizona_socket:bind(count, 0, Socket0),
    {ok, Socket}.

%%----------------------------------------------------------------------
%% @doc
%% In OTP-27, we can take advantage of using the triple-quoted string
%% ([https://www.erlang.org/eeps/eep-0064]) to write cleaner templates
%% and not worry about quotes escaping.
%% @end
%%----------------------------------------------------------------------
render(Bindings) ->
    ?LV(<<"
    <div>Count: <span id=\"counter\"><%= @count .%></span></div>
    <button type=\"button\" arz-click=\"+1\">+1</button>
    ">>).

handle_event(<<"+1">>, _Payload, Socket0) ->
    #{count := Count} = arizona_socket:get_bindings(Socket0),
    Socket = arizona_socket:bind(count, Count+1, Socket0),
    {ok, Socket}.
