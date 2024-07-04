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
-export([mount/2]).
-export([handle_event/4]).
-export([put_macro/3]).
-ignore_xref([put_macro/3]).
-export([get_macro/3]).
-ignore_xref([get_macro/3]).

-type macros() :: map().
-export_type([macros/0]).

%% --------------------------------------------------------------------
%% Callbacks.
%% --------------------------------------------------------------------

-callback mount(Socket) -> Socket
    when Socket :: arizona_socket:t().

-callback render(Macros) -> Tree
    when Macros :: macros(),
         Tree :: arizona_tpl_parse:tree().

-callback handle_event(EventName, Payload, Socket) -> Socket
    when EventName :: binary(),
         Payload :: arizona:payload(),
         Socket :: arizona_socket:t().

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

-spec parse_str(Str, Macros) -> arizona_tpl_parse:tree()
    when Str :: string() | binary(),
         Macros :: macros().
parse_str(Str, Macros) ->
    Tokens = arizona_tpl_scan:string(Str),
    arizona_tpl_parse:parse_exprs(Tokens, Macros).

%% --------------------------------------------------------------------
%% Callback support functions.
%% --------------------------------------------------------------------

-spec mount(Mod, Socket) -> Socket
    when Mod :: module(),
         Socket :: arizona_socket:t().
mount(Mod, Socket) ->
    Mod:mount(Socket).

-spec handle_event(Mod, EventName, Payload, Socket) -> Socket
    when Mod :: module(),
         EventName :: binary(),
         Payload :: arizona:payload(),
         Socket :: arizona_socket:t().
handle_event(Mod, Event, Payload, Socket) ->
    Mod:handle_event(Event, Payload, Socket).

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include("arizona.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    ?assert(is_block(arizona_tpl_compile:compile(?MODULE, render, #{}))).

is_block(#{block := _}) ->
    true;
is_block(_NonBlock) ->
    false.

-endif.
