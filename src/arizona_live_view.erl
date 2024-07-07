-module(arizona_live_view).
-moduledoc """
Live view.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([put_macro/3]).
-export([get_macro/3]).
-export([parse_str/2]).

%

-ignore_xref([parse_str/2]).
-ignore_xref([put_macro/3]).
-ignore_xref([get_macro/3]).

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/2]).
-export([render/2]).
-export([handle_event/4]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type macros() :: map().
-export_type([macros/0]).

%% --------------------------------------------------------------------
%% Callback definitions
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
%% API function definitions
%% --------------------------------------------------------------------

-spec put_macro(Key, Value, Macros1) -> Macros2
  when Key :: atom(),
       Value :: term(),
       Macros1 :: macros(),
       Macros2 :: macros().
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
%% Callback support function definitions
%% --------------------------------------------------------------------

-spec mount(Mod, Socket1) -> Socket2
    when Mod :: module(),
         Socket1 :: arizona_socket:t(),
         Socket2 :: arizona_socket:t().
mount(Mod, Socket) ->
    Mod:mount(Socket).

-spec render(Mod, Macros) -> Tree
    when Mod :: module(),
         Macros :: macros(),
         Tree :: arizona_tpl_parse:tree().
render(Mod, Macros) ->
    Mod:render(Mod, Macros).

-spec handle_event(Mod, EventName, Payload, Socket1) -> Socket2
    when Mod :: module(),
         EventName :: binary(),
         Payload :: arizona:payload(),
         Socket1 :: arizona_socket:t(),
         Socket2 :: arizona_socket:t().
handle_event(Mod, Event, Payload, Socket) ->
    Mod:handle_event(Event, Payload, Socket).

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    ?assert(is_block(arizona_tpl_compile:compile(?MODULE, render, #{}))).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

render(Macros) ->
    arizona_live_view:parse_str("""
    <main :stateful>
        <h1>{_@title}</h1>
        <.arizona_live_view:counter/>
    </main>
    """, Macros).

counter(Macros) ->
    arizona_live_view:parse_str("""
    <div :stateful>
        <div>{_@count}</div>
        <button type="button">Increment</button>
    </div>
    """, Macros).

is_block(#{block := _}) ->
    true;
is_block(_NonBlock) ->
    false.

-endif.
