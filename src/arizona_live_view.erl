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
-export([parse_str/1, compile/1]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

parse_str(Str) ->
    case arizona_tpl_scan:string(Str) of
        {ok, Tokens, _EndLocation} ->
            arizona_tpl_parse:parse_exprs(Tokens);
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

compile(Mod) ->
    arizona_tpl_compile:compile(Mod:render()).

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
    ?assertEqual([
        {tag,
        #{name => <<"main">>,void => false,
          tokens =>
           [{tag,
             #{name => <<"h1">>,void => false,
               tokens =>
                [{expr,{<<"fun(Assigns) -> _@title end">>,[]}}],
               directives => #{},attrs => []}},
            {block,
             #{function => counter,module => arizona_live_view,
               tokens => [],directives => #{},attrs => []}}],
          directives => #{statefull => true},
          attrs => []}}
    ], render()).

% Start parse_str support.

render() ->
    ?LV("""
    <main :statefull>
        <h1>{_@title}</h1>
        <.arizona_live_view:counter/>
    </main>
    """).

counter() ->
    ?LV("""
    <div :statefull>

    </div>
    """).

% End parse_str support.

compile_test() ->
    ?assertEqual({ok, [
        {0,#{id => [0],text => <<"<main>">>}},
        {1,#{id => [1],text => <<"<h1>">>}},
        {2,#{id => [2],expr => <<"fun(Assigns) -> _@title end">>}},
        {3,#{id => [3],text => <<"</h1>">>}},
        {4,
         #{block =>
               #{0 => #{id => [4,0],text => <<"<div>">>},
                 1 => #{id => [4,1],text => <<"</div>">>}},
           id => [4]}},
        {5,#{id => [5],text => <<"</main>">>}}
    ]}, compile(?MODULE)).

-endif.

