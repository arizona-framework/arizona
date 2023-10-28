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

-behaviour(arizona_web_live_view).

%% arizona_web_live_view callbacks
-export([ render/1 ]).

%% Libs
-include("arizona_live_view.hrl").

%%%=====================================================================
%%% arizona_web_live_view callbacks
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc
%% In OTP-27, we can take advantage of using the triple-quoted string
%% ([https://www.erlang.org/eeps/eep-0064]) to write cleaner templates
%% and not worry about quotes escaping.
%% @end
%%----------------------------------------------------------------------
render(Bindings0) ->
    Bindings = Bindings0#{title => <<"Arizona Example">>},
    ?LV(<<"
    <!DOCTYPE html>
    <html lang=\"en\">
    <head>
        <meta charset=\"UTF-8\">
        <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
        <title><%= @title .%></title>
    </head>
    <body>
        Hello, <%= @name .%>!
    </body>
    </html>
    ">>).
