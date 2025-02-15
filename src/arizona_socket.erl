-module(arizona_socket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([new/1]).
-export([put_view/2]).
-export([put_view/3]).

%

-ignore_xref([new/0]).
-ignore_xref([new/1]).
-ignore_xref([put_view/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(socket, {
    views :: views()
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

-type views() :: #{arizona_view:id() => arizona_view:view()}.
-export_type([views/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new() -> Socket when
    Socket :: socket().
new() ->
    new(#{}).

-spec new(Views) -> Socket when
    Views :: views(),
    Socket :: socket().
new(Views) ->
    #socket{
        views = Views
    }.

-spec put_view(View, Socket0) -> Socket1 when
    View :: arizona_view:view(),
    Socket0 :: socket(),
    Socket1 :: socket().
put_view(View, Socket) ->
    ViewId = arizona_view:get_assign(id, View),
    put_view(ViewId, View, Socket).

-spec put_view(ViewId, View, Socket0) -> Socket1 when
    ViewId :: arizona_view:id(),
    View :: arizona_view:view(),
    Socket0 :: socket(),
    Socket1 :: socket().
put_view(ViewId, View, Socket) ->
    Socket#socket{views = maps:put(ViewId, View, Socket#socket.views)}.
