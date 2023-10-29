%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Template adapter.

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
-module(arizona_template_adapter).

%% Types
-export_type([ bindings/0, state/0, diff/0 ]).

-type bindings() :: map().
-type state() :: term().
-type tree() :: #{term() => binary()}.
-type diff() :: #{binary() => iodata()}.

%% Callbacks
-optional_callbacks([]).

% @todo review all the results/returns.

-callback compile(Input) -> {ok, State} | {error, term()}
    when Input :: binary()
       , State :: state()
       .

-callback bind(Bindings, State) -> {ok, State} | {error, term()}
    when Bindings :: bindings()
       , State :: state()
       .

-callback tree(State) -> Tree
    when State :: state()
       , Tree :: tree()
       .

-callback diff(State) -> {ok, Diff} | none
    when State :: state()
       , Diff :: diff()
       .
