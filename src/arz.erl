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
-module(arz).
-moduledoc """
Alias for more convenient calls from a shell.
""".

%% API functions.
-export([r/0]).
-ignore_xref([r/0]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

-doc """
Recompile.
""".
r() ->
    apply(r3, do, [compile]),
    arizona_live_reload:reload().

