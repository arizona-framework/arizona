-module(arizona_socket).
-moduledoc """
Components state.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/2]).
-export([put_assigns/2]).
-export([put_assign/3]).
-export([get_assigns/1]).
-export([get_changes/1]).
-export([get_view/1]).
-export([get_events/1]).
-export([get_assign/2]).
-export([get_assign/3]).
-export([push_event/3]).
-export([prune/1]).

%

-ignore_xref([put_assigns/2]).
-ignore_xref([put_assign/3]).
-ignore_xref([get_assigns/1]).
-ignore_xref([get_assign/2]).
-ignore_xref([get_assign/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-opaque t() :: map().
-export_type([t/0]).

-type view() :: module().
-export_type([view/0]).

-type events() :: list().
-export_type([events/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(View, Assigns) -> t()
    when View :: view(),
         Assigns :: arizona_template_renderer:assigns().
new(View, Assigns) ->
    #{
        view => View,
        assigns => Assigns,
        events => [],
        changes => ordsets:from_list(maps:keys(Assigns))
    }.

-spec put_assigns(Assigns, Socket1) -> Socket2
    when Assigns :: arizona_template_renderer:assigns(),
         Socket1 :: t(),
         Socket2 :: t().
put_assigns(Assigns, Socket) ->
    maps:fold(fun put_assign/3, Socket, Assigns).

-spec put_assign(Key, Value, Socket1) -> Socket2
    when Key :: atom(),
         Value :: term(),
         Socket1 :: t(),
         Socket2 :: t().
put_assign(Key, Value, Socket) ->
    #{assigns := Assigns, changes := Changes} = Socket,
    case Assigns of
        #{Key := Value} ->
            Socket;
        #{} ->
            Socket#{
                assigns => Assigns#{Key => Value},
                changes => ordsets:add_element(Key, Changes)
            }
    end.

-spec get_assigns(t()) -> arizona_template_renderer:assigns().
get_assigns(#{assigns := Assigns}) ->
    Assigns.

-spec get_changes(t()) -> ordsets:ordset(atom()).
get_changes(#{changes := Changes}) ->
    Changes.

-spec get_view(t()) -> view().
get_view(#{view := View}) ->
    View.

-spec get_events(t()) -> events().
get_events(#{events := Events}) ->
    Events.

-spec get_assign(Key, t()) -> Got
    when Key :: atom(),
         Got :: term().
get_assign(Key, Socket) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns).

-spec get_assign(Key, t(), Default) -> Got
    when Key :: atom(),
         Default :: term(),
         Got :: term().
get_assign(Key, Socket, Default) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns, Default).

-spec push_event(EventName, Payload, Socket1) -> Socket2
    when EventName :: binary(),
         Payload :: arizona:payload(),
         Socket1 :: t(),
         Socket2 :: t().
push_event(Name, Payload, #{events := Events} = Socket) ->
    Socket#{events => [[Name, Payload] | Events]}.

-spec prune(Socket1) -> Socket2
    when Socket1 :: t(),
         Socket2 :: t().
prune(Socket) ->
    Socket#{
        events => [],
        changes => ordsets:new()
    }.
