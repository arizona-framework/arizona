-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/4]).
-export([view_template/2]).
-export([component_template/2]).
-export([nested_template/2]).
-export([view/2]).
-export([component/3]).
-export([if_true/2]).

%

-ignore_xref([render/4]).
-ignore_xref([view_template/2]).
-ignore_xref([component_template/2]).
-ignore_xref([nested_template/2]).
-ignore_xref([view/2]).
-ignore_xref([component/3]).
-ignore_xref([if_true/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type static_list() :: [binary()].
-export_type([static_list/0]).

-type dynamic_list() :: [
    fun((ViewAcc :: arizona_view:view(), Socket :: arizona_socket:socket()) -> binary())
].
-export_type([dynamic_list/0]).

-type token() ::
    {view_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {component_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {nested_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {view, Mod :: module(), Assigns :: arizona_view:assigns()}
    | {component, Mod :: module(), Fun :: atom(), Assigns :: arizona_view:assigns()}.
-export_type([token/0]).

-type rendered() ::
    [rendered_value()]
    | [template | Static :: static_list() | Dynamic :: dynamic_list()].
-export_type([rendered/0]).

-type rendered_value() ::
    binary()
    | rendered()
    | {arizona_diff:index(), binary() | rendered()}
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

-spec render(Payload, View, ParentView, ParentSocket) -> {View, Socket} when
    Payload :: Token | Rendered,
    Token :: token(),
    Rendered :: rendered(),
    ParentView :: arizona_view:view(),
    ParentSocket :: arizona_socket:socket(),
    View :: ParentView | arizona_view:view(),
    Socket :: ParentSocket | arizona_socket:socket().
render({view_template, Static, Dynamic}, View, _ParentView, Socket) ->
    render_view_template(View, Socket, Static, Dynamic);
render({component_template, Static, Dynamic}, View, _ParentView, Socket) ->
    render_component_template(View, Socket, Static, Dynamic);
render({nested_template, Static, Dynamic}, _View, ParentView, Socket) ->
    render_nested_template(ParentView, Socket, Static, Dynamic);
render({view, Mod, Assigns}, _View, ParentView, Socket) ->
    render_view(ParentView, Socket, Mod, Assigns);
render({component, Mod, Fun, Assigns}, _View, ParentView, Socket) ->
    render_component(ParentView, Socket, Mod, Fun, Assigns);
render(Rendered, _View, View0, Socket) when is_binary(Rendered); is_list(Rendered) ->
    View = arizona_view:put_rendered(Rendered, View0),
    {View, Socket}.

-spec view_template(View, Template) -> Token when
    View :: arizona_view:view(),
    Template :: binary(),
    Token :: {view_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
view_template(View, Template) when is_binary(Template) ->
    Bindings = #{'View' => View},
    {Static, Dynamic} = parse_template(Bindings, Template),
    view_template({Static, Dynamic}).

-spec component_template(View, Template) -> Token when
    View :: arizona_view:view(),
    Template :: binary(),
    Token :: {component_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
component_template(View, Template) ->
    Bindings = #{'View' => View},
    {Static, Dynamic} = parse_template(Bindings, Template),
    component_template({Static, Dynamic}).

-spec nested_template(ParentView, Template) -> Token when
    ParentView :: arizona_view:view(),
    Template :: binary(),
    Token :: {nested_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
nested_template(ParentView, Template) ->
    Bindings = #{'View' => ParentView},
    {Static, Dynamic} = parse_template(Bindings, Template),
    nested_template({Static, Dynamic}).

-spec view(Mod, Assigns) -> Token when
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Token :: {view, Mod, Assigns}.
view(Mod, Assigns) when is_atom(Mod), is_map(Assigns), is_map_key(id, Assigns) ->
    {view, Mod, Assigns}.

-spec component(Mod, Fun, Assigns) -> Token when
    Mod :: module(),
    Fun :: atom(),
    Assigns :: arizona_view:assigns(),
    Token :: {component, Mod, Fun, Assigns}.
component(Mod, Fun, Assigns) when is_atom(Mod), is_atom(Fun), is_map(Assigns) ->
    {component, Mod, Fun, Assigns}.

-spec if_true(Cond, Callback) -> Rendered when
    Cond :: boolean(),
    Callback :: fun(() -> Rendered),
    Rendered :: rendered_value().
if_true(Cond, Callback) when is_function(Callback, 0) ->
    case Cond of
        true ->
            erlang:apply(Callback, []);
        false ->
            ~""
    end.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

-spec view_template({Static, Dynamic}) -> Token when
    Static :: static_list(),
    Dynamic :: dynamic_list(),
    Token :: {view_template, Static, Dynamic}.
view_template({Static, Dynamic}) when is_list(Static), is_list(Dynamic) ->
    {view_template, Static, Dynamic}.

-spec component_template({Static, Dynamic}) -> Token when
    Static :: static_list(),
    Dynamic :: dynamic_list(),
    Token :: {component_template, Static, Dynamic}.
component_template({Static, Dynamic}) ->
    {component_template, Static, Dynamic}.

-spec nested_template({Static, Dynamic}) -> Token when
    Static :: static_list(),
    Dynamic :: dynamic_list(),
    Token :: {nested_template, Static, Dynamic}.
nested_template({Static, Dynamic}) ->
    {nested_template, Static, Dynamic}.

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
            Token = arizona_view:render(Mod, View0),
            {View1, Socket1} = render(Token, View0, ParentView0, Socket0),
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
    Token = arizona_component:render(Mod, Fun, View0),
    {View, Socket1} = render(Token, View0, ParentView0, Socket0),
    Rendered = arizona_view:rendered(View),
    ParentView = arizona_view:put_rendered(Rendered, ParentView0),
    {ParentView, Socket1}.

render_dynamic([], ViewAcc, Socket) ->
    {ViewAcc, Socket};
render_dynamic([Callback | T], ViewAcc0, Socket0) ->
    {ViewAcc, Socket} = erlang:apply(Callback, [ViewAcc0, Socket0, _Opts = #{}]),
    render_dynamic(T, ViewAcc, Socket).

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
