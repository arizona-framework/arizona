%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc EEl template adapter.

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
-module(arizona_template_adapter_eel).

-behaviour(arizona_template_adapter).

%% arizona_template_adapter callbacks
-export([ compile/1, bind/2, vars/1, tree/1, diff/1 ]).

%%%=====================================================================
%%% arizona_template_adapter callbacks
%%%=====================================================================

compile(Input) ->
    Tokens = eel_tokenizer:tokenize(Input),
    Tree = eel_structurer:tree(Tokens),
    eel_compiler:compile(Tree).

bind(Bindings, State) ->
    RenderFun = case is_map_key(changes, State) of
        true -> fun eel_renderer:render_changes/2;
        false -> fun eel_renderer:render/2
    end,
    Changes = RenderFun(Bindings, State),
    State#{changes => Changes}.

vars(#{vars := Vars}) ->
    Vars.

tree(#{parts := Parts, changes := Changes}) ->
    maps:merge(Parts, Changes).

diff(State) ->
    maps:get(changes, State, #{}).
