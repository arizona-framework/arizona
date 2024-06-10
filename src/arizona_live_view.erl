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
-module(arizona_live_view).
-moduledoc """
Live view.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([parse_str/2, compile/2]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

parse_str(Str, Macros) ->
    case arizona_tpl_scan:string(Str) of
        {ok, Tokens, _EndLocation} ->
            arizona_tpl_parse:parse_exprs(Tokens, Macros);
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

compile(Mod, Macros) ->
    arizona_tpl_compile:compile({Mod, render, Macros}).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

% nothing here yet!

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include("live_view.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_str_test() ->
    ?assertMatch([
        {tag,
         #{name := <<"main">>,
           directives := #{statefull := true}}
    }], render(#{})).

% Start parse_str support.

render(Macros) ->
    ?LV("""
    <main :statefull>
        <h1>{_@title}</h1>
        <.arizona_live_view:counter/>
    </main>
    """).

counter(Macros) ->
    ?LV("""
    <div :statefull>
        <div>{_@count}</div>
        <button type="button">Increment</button>
    </div>
    """).

% End parse_str support.

compile_test() ->
    ?assertMatch({ok, #{block := _}}, compile(?MODULE, #{})).

-endif.

