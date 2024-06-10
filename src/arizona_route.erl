%%
%% %CopyrightBegin%
%%
%% Copyright 2024 William Fank Thomé
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
-module(arizona_route).
-moduledoc """
Router.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([live_view/1]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

live_view(Mod) ->
    {ok, _Tpl} = arizona_live_view:compile(Mod),
    error(not_implemented_yet).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

% nothing here yet!

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

live_view_test() ->
    ?assertError(not_implemented_yet, live_view(arizona_live_view)).

-endif.

