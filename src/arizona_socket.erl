-module(arizona_socket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([new/2]).
-export([render_context/1]).
-export([put_view/2]).
-export([put_view/3]).

%

-ignore_xref([new/1]).
-ignore_xref([new/2]).
-ignore_xref([render_context/1]).
-ignore_xref([put_view/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(socket, {
    render_context :: render_context(),
    views :: views()
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

-type render_context() :: render | diff.
-export_type([render_context/0]).

-type views() :: #{arizona_view:id() => arizona_view:view()}.
-export_type([views/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(RenderContext) -> Socket when
    RenderContext :: render_context(),
    Socket :: socket().
new(RenderContext) ->
    new(RenderContext, #{}).

-spec new(RenderContext, Views) -> Socket when
    RenderContext :: render_context(),
    Views :: views(),
    Socket :: socket().
new(RenderContext, Views) ->
    #socket{
        render_context = RenderContext,
        views = Views
    }.

-spec render_context(Socket) -> RenderContext when
    Socket :: socket(),
    RenderContext :: render_context().
render_context(#socket{} = Socket) ->
    Socket#socket.render_context.

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
