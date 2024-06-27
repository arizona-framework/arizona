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
-export([parse_str/2]).
-ignore_xref([parse_str/2]).
-export([compile/3]).
-ignore_xref([compile/3]).
-export([persist_get/3]).
-export([mount/2]).
-export([handle_event/4]).

-export([put_macro/3]).
-ignore_xref([put_macro/3]).
-export([get_macro/3]).
-ignore_xref([get_macro/3]).

-opaque macros() :: map().
-export_type([macros/0]).

-opaque tree() :: list().
-export_type([tree/0]).

%% Macros
-define(PERSIST_KEY, ?MODULE).

%% --------------------------------------------------------------------
%% Callbacks.
%% --------------------------------------------------------------------

-callback mount(arizona_socket:t()) ->
    {ok, arizona_socket:t()}.

-callback render(macros()) ->
    tree().

-callback handle_event(EventName :: binary(), arizona:payload(), arizona_socket:t()) ->
    {noreply, arizona_socket:t()}.

-optional_callbacks([handle_event/3]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec put_macro(Key, Value, Macros) -> Macros
  when Key :: atom(),
       Value :: term(),
       Macros :: macros().
put_macro(Key, Value, Macros) ->
  Macros#{
    Key => maps:get(Key, Macros, Value)
  }.

-spec get_macro(Key, Macros, Default) -> Got
  when Key :: atom(),
       Macros :: macros(),
       Default :: term(),
       Got :: term().
get_macro(Key, Macros, Default) ->
  maps:get(Key, Macros, Default).

-spec parse_str(Str, Macros) -> Parsed
    when Str :: string() | binary(),
         Macros :: macros(),
         Parsed :: tree().
parse_str(Str, Macros) ->
    {ok, Tokens, _EndLocation} = arizona_tpl_scan:string(Str),
    {ok, Parsed} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Parsed.

compile(Mod, Fun, Macros) ->
    arizona_tpl_compile:compile({Mod, Fun, Macros}).

persist_get(Mod, Fun, Macros) ->
    persistent_term:get({?PERSIST_KEY, {Mod, Fun}}, persist(Mod, Fun, Macros)).

%% --------------------------------------------------------------------
%% Callback support functions.
%% --------------------------------------------------------------------

mount(Mod, Socket) ->
    Mod:mount(Socket).

handle_event(Mod, Event, Payload, Socket) ->
    Mod:handle_event(Event, Payload, Socket).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

persist(Mod, Fun, Macros) ->
    {ok, Compiled} = compile(Mod, Fun, Macros),
    persistent_term:put({?PERSIST_KEY, {Mod, Fun}}, Compiled),
    Compiled.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include("arizona.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_str_test() ->
    ?assertMatch([
        {tag,
         #{name := <<"main">>,
           directives := #{stateful := true}}
    }], render(#{})).

% Start parse_str support.

render(Macros) ->
    ?ARIZONA_LIVEVIEW(Macros, """
    <main :stateful>
        <h1>{_@title}</h1>
        <.arizona_live_view:counter/>
    </main>
    """).

counter(Macros) ->
    ?ARIZONA_LIVEVIEW(Macros, """
    <div :stateful>
        <div>{_@count}</div>
        <button type="button">Increment</button>
    </div>
    """).

% End parse_str support.

compile_test() ->
    ?assertMatch({ok, #{block := _}}, compile(?MODULE, render, #{})).

-endif.

