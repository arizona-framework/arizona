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
-module(arizona_sup).
-behaviour(supervisor).
-moduledoc false.

%% API functions.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% Supervisor callbacks.
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}}
    when Args :: term(),
         SupFlags :: supervisor:sup_flags(),
         ChildSpec :: supervisor:child_spec().
init(_Args) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = case arizona_cfg:endpoint() of
        #{live_reload := true} ->
            [#{id => arizona_live_reload, start =>
              {arizona_live_reload, start_link, []}}];
        #{} ->
            []
    end,
    {ok, {SupFlags, ChildSpecs}}.
