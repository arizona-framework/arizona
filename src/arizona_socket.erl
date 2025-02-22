-module(arizona_socket).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([new/2]).
-export([render_context/1]).
-export([set_render_context/2]).
-export([put_view/2]).
-export([put_view/3]).
-export([get_view/2]).
-export([remove_view/2]).

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

-spec set_render_context(RenderContext, Socket0) -> Socket1 when
    RenderContext :: render_context(),
    Socket0 :: socket(),
    Socket1 :: socket().
set_render_context(render, #socket{} = Socket) ->
    Socket#socket{render_context = render};
set_render_context(diff, #socket{} = Socket) ->
    Socket#socket{render_context = diff}.

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
put_view(ViewId, View0, #socket{views = Views} = Socket) when is_binary(ViewId) ->
    case Socket#socket.render_context of
        render ->
            View = arizona_view:set_tmp_rendered([], View0),
            Socket#socket{views = Views#{ViewId => View}};
        diff ->
            View = arizona_view:set_diff([], View0),
            Socket#socket{views = Views#{ViewId => View}}
    end.

-spec get_view(ViewId, Socket) -> {ok, View} | error when
    ViewId :: arizona_view:id(),
    Socket :: socket(),
    View :: arizona_view:view().
get_view(ViewId, #socket{} = Socket) when is_binary(ViewId) ->
    case Socket#socket.views of
        #{ViewId := View} ->
            {ok, View};
        #{} ->
            error
    end.

-spec remove_view(ViewId, Socket0) -> Socket1 when
    ViewId :: arizona_view:id(),
    Socket0 :: socket(),
    Socket1 :: socket().
remove_view(ViewId, #socket{views = Views} = Socket) when is_map_key(ViewId, Views) ->
    Socket#socket{views = maps:remove(ViewId, Views)}.
