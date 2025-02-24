-module(arizona_socket).

%% --------------------------------------------------------------------
%% Support function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([new/4]).
-export([render_context/1]).
-export([set_render_context/2]).
-export([views/1]).
-export([put_view/2]).
-export([get_view/2]).
-export([remove_view/2]).
-export([path_params/1]).
-export([query_string/1]).
-export([query_params/1]).
-export([set_query_params/2]).

%

-ignore_xref([new/2]).
-ignore_xref([render_context/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(socket, {
    render_context :: render_context(),
    views :: views(),
    path_params :: arizona:path_params(),
    query_string :: arizona:query_string(),
    query_params :: arizona:query_params() | undefined
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

-type render_context() :: render | diff.
-export_type([render_context/0]).

-type views() :: #{arizona_view:id() => arizona_view:view()}.
-export_type([views/0]).

%% --------------------------------------------------------------------
%% Support function definitions
%% --------------------------------------------------------------------

-spec new(RenderContext) -> Socket when
    RenderContext :: render_context(),
    Socket :: socket().
new(RenderContext) ->
    new(RenderContext, #{}, #{}, ~"").

-spec new(RenderContext, Views, PathParams, QueryString) -> Socket when
    RenderContext :: render_context(),
    Views :: views(),
    PathParams :: arizona:path_params(),
    QueryString :: arizona:query_string(),
    Socket :: socket().
new(RenderContext, Views, PathParams, QueryString) ->
    #socket{
        render_context = RenderContext,
        views = Views,
        path_params = PathParams,
        query_string = QueryString,
        query_params = undefined
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

-spec views(Socket) -> Views when
    Socket :: socket(),
    Views :: views().
views(#socket{} = Socket) ->
    Socket#socket.views.

-spec put_view(View, Socket0) -> Socket1 when
    View :: arizona_view:view(),
    Socket0 :: socket(),
    Socket1 :: socket().
put_view(View, Socket) ->
    ViewId = arizona_view:get_binding(id, View),
    put_view(ViewId, View, Socket).

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

-spec path_params(Socket) -> PathParams when
    Socket :: socket(),
    PathParams :: arizona:path_params().
path_params(#socket{} = Socket) ->
    Socket#socket.path_params.

-spec query_string(Socket) -> QueryString when
    Socket :: socket(),
    QueryString :: arizona:query_string().
query_string(#socket{} = Socket) ->
    Socket#socket.query_string.

-spec query_params(Socket) -> QueryParams when
    Socket :: socket(),
    QueryParams :: arizona:query_params() | undefined.
query_params(#socket{} = Socket) ->
    Socket#socket.query_params.

-spec set_query_params(QueryParams, Socket0) -> Socket1 when
    QueryParams :: arizona:query_params(),
    Socket0 :: socket(),
    Socket1 :: socket().
set_query_params(QueryParams, #socket{} = Socket) when is_list(QueryParams) ->
    Socket#socket{query_params = QueryParams}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

put_view(ViewId, View0, #socket{views = Views} = Socket) when is_binary(ViewId) ->
    case Socket#socket.render_context of
        render ->
            View = arizona_view:set_tmp_rendered([], View0),
            Socket#socket{views = Views#{ViewId => View}};
        diff ->
            View = arizona_view:set_diff([], View0),
            Socket#socket{views = Views#{ViewId => View}}
    end.
