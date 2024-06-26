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
-module(arizona_html).
-moduledoc """
HTML support.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([safe/1]).
-export([safe_types/0]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

% TODO: Do this really safe for HTML.
safe(V) when is_binary(V) ->
    V;
safe(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
safe(V) when is_integer(V) ->
    integer_to_binary(V, 10);
safe(V) when is_float(V) ->
    io_lib:format("~p", [V]).

safe_types() ->
    [binary, atom, integer, float].

