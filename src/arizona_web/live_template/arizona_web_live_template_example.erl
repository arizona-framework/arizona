%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Template example.

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
-module(arizona_web_live_template_example).

-behaviour(arizona_live_view).

%% API
-export([ mount/2, render/1 ]).

%% Libs
-include("arizona_live_view.hrl").

%%%=====================================================================
%%% API
%%%=====================================================================

mount(_Params, Socket0) ->
    Socket = arizona_socket:bind(#{
        title => <<"Arizona Example">>
    }, Socket0),
    {ok, Socket}.

render(Bindings) ->
    ?LV(<<"
    <!DOCTYPE html>
    <html lang=\"en\">
    <head>
        <meta charset=\"UTF-8\">
        <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
        <title><%= @title .%></title>
        <script src=\"assets/arizona.js\"></script>
        <script src=\"assets/morphdom.min.js\"></script>
        <script src=\"assets/main.js\"></script>
    </head>
    <body>
        <%= @inner_content .%>
    </body>
    </html>
    ">>).
