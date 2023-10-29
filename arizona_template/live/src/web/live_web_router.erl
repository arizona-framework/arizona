%% @author {{author_name}} <{{author_email}}>
%% @copyright {{copyright_year}} {{author_name}}
%% @doc Router.

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
-module({{name}}_web_router).

-behaviour(arizona_router_adapter).

%% arizona_router_adapter callbacks
-export([ match/2 ]).

%%%=====================================================================
%%% arizona_router_adapter callbacks
%%%=====================================================================

match(get, [ ]) ->
    LiveOpts = #{template => {{name}}_web_template_root},
    { { live, {{name}}_web_live_home, LiveOpts }, #{} };
match(Method, Path) ->
    { { {{name}}_web_controller_error, invalid_route, [Method, Path] }, #{} }.
