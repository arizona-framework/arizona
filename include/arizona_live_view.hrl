%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Live view helpers.

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

%% Macros
%% @todo: eval arizona_template:compile/1 via parse_transform.
-define(LV(Bin), arizona_template:render(
    arizona_template:bind(Bindings, arizona_template:compile(Bin))
)).

-define(LV(Template, Bin), Template:render(?MODULE, Bindings, ?LV(Bin))).
