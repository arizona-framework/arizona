%% @author {{author_name}} <{{author_email}}>
%% @copyright {{copyright_year}} {{author_name}}
%% @doc Error controller.

%% Copyright {{copyright_year}} {{author_name}}
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
-module({{name}}_web_live_home).

-behaviour(arizona_live_view).

%% arizona_web_live_view callbacks
-export([ mount/2, render/1, handle_event/3 ]).

%% Libs
-include_lib("arizona/include/arizona_live_view.hrl").

%%%=====================================================================
%%% arizona_web_live_view callbacks
%%%=====================================================================

mount(_Params, Socket0) ->
    Socket = arizona_socket:bind(count, 0, Socket0),
    {ok, Socket}.

render(Bindings) ->
    ?LV(<<"
    <div>Count: <span id=\"counter\"><%= @count .%></span></div>
    <button type=\"button\" arz-click=\"+1\">+1</button>
    ">>).

handle_event(<<"+1">>, _Payload, Socket0) ->
    #{count := Count} = arizona_socket:get_bindings(Socket0),
    Socket = arizona_socket:bind(count, Count+1, Socket0),
    {ok, Socket}.
