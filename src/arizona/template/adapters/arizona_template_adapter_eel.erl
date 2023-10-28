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
-export([ compile/1, bind/2, render/1 ]).

%%======================================================================
%% arizona_template_adapter callbacks
%%======================================================================

compile(Input) ->
    Tokens = eel_tokenizer:tokenize(Input),
    Tree = eel_structurer:tree(Tokens),
    eel_compiler:compile(Tree).

bind(Bindings, State) ->
    Parts = maps:get(parts, State),
    ChangedParts = eel_renderer:render(Bindings, State),
    State#{parts => maps:merge(Parts, ChangedParts)}.

render(State) ->
    maps:values(maps:get(parts, State)).
