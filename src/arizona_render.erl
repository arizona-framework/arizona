-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/3]).
-export([view_template/3]).
-export([component_template/3]).
-export([nested_template/1]).
-export([view/2]).
-export([component/3]).

%

-ignore_xref([render/3]).
-ignore_xref([view_template/3]).
-ignore_xref([component_template/3]).
-ignore_xref([nested_template/1]).
-ignore_xref([view/2]).
-ignore_xref([component/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type token() ::
    {view_template, View :: arizona_view:view(), Socket :: arizona_socket:socket(),
        Static :: [binary()], Dynamic :: [binary()]}
    | {component_template, View :: arizona_view:view(), Socket :: arizona_socket:socket(),
        Static :: [binary()], Dynamic :: [binary()]}
    | {nested_template, ParentView :: arizona_view:view(), Socket :: arizona_socket:socket(),
        Static :: [binary()], Dynamic :: [binary()]}
    | {view, ParentView :: arizona_view:view(), Socket :: arizona_socket:socket(), Mod :: module(),
        Assigns :: arizona_view:assigns()}
    | {component, ParentView :: arizona_view:view(), Socket :: arizona_socket:socket(),
        Mod :: module(), Fun :: atom(), Assigns :: arizona_view:assigns()}
    | {callback, callback(Token :: token())}.
-export_type([token/0]).

-type callback(Token) :: fun(
    (View :: arizona_view:view(), Socket :: arizona_socket:socket()) -> Token
).
-export_type([callback/1]).

-type rendered() ::
    [rendered_value()]
    | [template | Static :: [binary()] | Dynamic :: [binary()]].
-export_type([rendered/0]).

-type rendered_value() ::
    binary()
    | rendered()
    % I could not figure out a correct type without the `dynamic/0`.
    | dynamic().
-export_type([rendered_value/0]).

%% --------------------------------------------------------------------
%% Doctests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Payload, ParentView, ParentSocket) -> {View, Socket} when
    Payload :: Token | Rendered,
    Token :: token(),
    Rendered :: rendered(),
    ParentView :: arizona_view:view(),
    ParentSocket :: arizona_socket:socket(),
    View :: ParentView | arizona_view:view(),
    Socket :: ParentSocket | arizona_socket:socket().
render({view_template, View, Socket, Static, Dynamic}, _ParentView, _ParentSocket) ->
    render_view_template(View, Socket, Static, Dynamic);
render({component_template, View, Socket, Static, Dynamic}, _ParentView, _ParentSocket) ->
    render_component_template(View, Socket, Static, Dynamic);
render({nested_template, ParentView, Socket, Static, Dynamic}, _ParentView, _ParentSocket) ->
    render_nested_template(ParentView, Socket, Static, Dynamic);
render({view, ParentView, Socket, Mod, Assigns}, _ParentView, _ParentSocket) ->
    render_view(ParentView, Socket, Mod, Assigns);
render({component, ParentView, Socket, Mod, Fun, Assigns}, _ParentView, _ParentSocket) ->
    render_component(ParentView, Socket, Mod, Fun, Assigns);
render({callback, Callback}, ParentView, ParentSocket) ->
    Token = erlang:apply(Callback, [ParentView, ParentSocket]),
    render(Token, ParentView, ParentSocket);
render(Rendered, View0, Socket) ->
    View = arizona_view:put_rendered(Rendered, View0),
    {View, Socket}.

-spec view_template(View, Socket, Template) -> Token when
    View :: arizona_view:view(),
    Socket :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Token :: {view_template, View, Socket, Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()].
view_template(View, Socket, {Static, Dynamic}) ->
    {view_template, View, Socket, Static, Dynamic};
view_template(View, Socket, Template) when is_binary(Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    view_template(View, Socket, {Static, Dynamic}).

-spec component_template(View, Socket, Template) -> Token when
    View :: arizona_view:view(),
    Socket :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Token :: {component_template, View, Socket, Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()].
component_template(View, Socket, {Static, Dynamic}) ->
    {component_template, View, Socket, Static, Dynamic};
component_template(View, Socket, Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    component_template(View, Socket, {Static, Dynamic}).

-spec nested_template(Template) -> Callback when
    Template :: binary() | {Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()],
    Callback :: {callback, callback(Token)},
    Token :: {nested_template, ParentView, Socket, Static, Dynamic},
    ParentView :: arizona_view:view(),
    Socket :: arizona_socket:socket().
nested_template({Static, Dynamic}) ->
    {callback, fun(ParentView, Socket) ->
        {nested_template, ParentView, Socket, Static, Dynamic}
    end};
nested_template(Template) ->
    {callback, fun(ParentView, Socket) ->
        Bindings = #{'View' => ParentView, 'Socket' => Socket},
        {Static, Dynamic} = parse_template(Bindings, Template),
        {nested_template, ParentView, Socket, Static, Dynamic}
    end}.

-spec view(Mod, Assigns) -> Callback when
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Callback :: {callback, callback(Token)},
    Token :: {view, ParentView, Socket, Mod, Assigns},
    ParentView :: arizona_view:view(),
    Socket :: arizona_socket:socket().
view(Mod, Assigns) when is_atom(Mod), is_map(Assigns), is_map_key(id, Assigns) ->
    {callback, fun(ParentView, Socket) ->
        {view, ParentView, Socket, Mod, Assigns}
    end}.

-spec component(Mod, Fun, Assigns) -> Callback when
    Mod :: module(),
    Fun :: atom(),
    Assigns :: arizona_view:assigns(),
    Callback :: {callback, callback(Token)},
    Token :: {component, ParentView, Socket, Mod, Fun, Assigns},
    ParentView :: arizona_view:view(),
    Socket :: arizona_socket:socket().
component(Mod, Fun, Assigns) when is_atom(Mod), is_atom(Fun), is_map(Assigns) ->
    {callback, fun(ParentView, Socket) ->
        {component, ParentView, Socket, Mod, Fun, Assigns}
    end}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

render_view_template(View0, Socket0, Static, Dynamic0) ->
    {View1, Socket1} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = lists:reverse(arizona_view:rendered(View1)),
    Template = [template, Static, Dynamic],
    View2 = arizona_view:set_rendered(Template, View1),
    View = arizona_view:merge_changed_assigns(View2),
    Socket = arizona_socket:put_view(View, Socket1),
    {View, Socket}.

render_component_template(View0, Socket0, Static, Dynamic0) ->
    {View1, Socket} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = lists:reverse(arizona_view:rendered(View1)),
    Template = [template, Static, Dynamic],
    View = arizona_view:set_rendered(Template, View1),
    {View, Socket}.

render_nested_template(ParentView0, Socket0, Static, Dynamic0) ->
    Assigns = arizona_view:assigns(ParentView0),
    View0 = arizona_view:new(Assigns),
    {View1, Socket} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = arizona_view:rendered(View1),
    Template = [template, Static, Dynamic],
    ParentView = arizona_view:put_rendered(Template, ParentView0),
    {ParentView, Socket}.

render_view(ParentView0, Socket0, Mod, Assigns) ->
    case arizona_view:mount(Mod, Assigns, Socket0) of
        {ok, View0} ->
            Token = arizona_view:render(Mod, View0, Socket0),
            {View1, Socket1} = render(Token, ParentView0, Socket0),
            Rendered = arizona_view:rendered(View1),
            ParentView = arizona_view:put_rendered(Rendered, ParentView0),
            View = arizona_view:set_rendered([], View1),
            Socket = arizona_socket:put_view(View, Socket1),
            {ParentView, Socket};
        ignore ->
            {ParentView0, Socket0}
    end.

render_component(ParentView0, Socket0, Mod, Fun, Assigns) ->
    View0 = arizona_view:new(Assigns),
    Token = arizona_component:render(Mod, Fun, View0, Socket0),
    {View, Socket1} = render(Token, ParentView0, Socket0),
    Rendered = arizona_view:rendered(View),
    ParentView = arizona_view:put_rendered(Rendered, ParentView0),
    {ParentView, Socket1}.

render_dynamic([], View, Socket) ->
    {View, Socket};
render_dynamic([Payload | T], View0, Socket0) ->
    {View, Socket} = render(Payload, View0, Socket0),
    render_dynamic(T, View, Socket).

parse_template(Bindings, Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    {StaticAst, DynamicAst} = arizona_parser:parse(Tokens),
    Static = eval_static_ast(StaticAst),
    Dynamic = eval_dynamic_ast(Bindings, DynamicAst),
    {Static, Dynamic}.

eval_static_ast(Ast) ->
    [eval_expr(Expr, []) || Expr <- Ast].

eval_dynamic_ast(Bindings, Ast) ->
    [eval_expr(Expr, Bindings) || Expr <- Ast].

eval_expr(Expr, Bindings) ->
    {value, Value, _NewBindings} = erl_eval:expr(Expr, Bindings),
    Value.
