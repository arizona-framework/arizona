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

%

-ignore_xref([render/2]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-callback mount(Socket) -> Socket
    when Socket :: arizona_socket:t().

-callback render(Macros1) -> Result
    when Macros1 :: arizona_template_compiler:macros(),
         Result :: {ok, {Parsed, Macros2}}
                 | {error, ErrReason},
         Parsed :: [arizona_template_parser:element()],
         Macros2 :: arizona_template_compiler:macros(),
         ErrReason :: arizona_template_scanner:error_reason()
                    | arizona_template_parser:error_reason().

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
       Macros1 :: arizona_template_compiler:macros(),
       Macros2 :: arizona_template_compiler:macros().
put_macro(Key, Value, Macros) ->
    Macros#{
        Key => maps:get(Key, Macros, Value)
    }.

-spec get_macro(Key, Macros, Default) -> Got
  when Key :: atom(),
       Macros :: arizona_template_compiler:macros(),
       Default :: term(),
       Got :: term().
get_macro(Key, Macros, Default) ->
  maps:get(Key, Macros, Default).

-spec parse_str(Str, Macros1) -> Result
    when Str :: string() | binary(),
         Macros1 :: arizona_template_compiler:macros(),
         Result :: {ok, {Parsed, Macros2}}
                 | {error, ErrReason},
         Parsed :: [arizona_template_parser:element()],
         Macros2 :: arizona_template_compiler:macros(),
         ErrReason :: arizona_template_scanner:error_reason()
                    | arizona_template_parser:error_reason().
parse_str(Str, Macros) ->
    maybe
        {ok, Tokens} ?= arizona_template_scanner:scan(Str),
        {ok, Elems} ?= arizona_template_parser:parse(Tokens),
        {ok, {Elems, Macros}}
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% Callback support function definitions
%% --------------------------------------------------------------------

-spec mount(Mod, Socket1) -> Socket2
    when Mod :: module(),
         Socket1 :: arizona_socket:t(),
         Socket2 :: arizona_socket:t().
mount(Mod, Socket) ->
    Mod:mount(Socket).

-spec render(Mod, Macros1) -> Result
    when Mod :: module(),
         Macros1 :: arizona_template_compiler:macros(),
         Result :: {ok, {Parsed, Macros2}}
                 | {error, ErrReason},
         Parsed :: [arizona_template_parser:element()],
         Macros2 :: arizona_template_compiler:macros(),
         ErrReason :: arizona_template_scanner:error_reason()
                    | arizona_template_parser:error_reason().
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
