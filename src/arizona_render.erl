-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/4]).
-export([view_template/2]).
-export([component_template/2]).
-export([nested_template/1]).
-export([nested_template/2]).
-export([view/2]).
-export([component/3]).
-export([if_true/2]).
-export([list/2]).
-export([layout/4]).

%

-ignore_xref([render/4]).
-ignore_xref([view_template/2]).
-ignore_xref([component_template/2]).
-ignore_xref([nested_template/1]).
-ignore_xref([nested_template/2]).
-ignore_xref([view/2]).
-ignore_xref([component/3]).
-ignore_xref([if_true/2]).
-ignore_xref([list/2]).
-ignore_xref([layout/4]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type static_list() :: [binary()].
-export_type([static_list/0]).

-type dynamic_list() :: [
    fun(
        (
            ViewAcc :: arizona_view:view(),
            Socket :: arizona_socket:socket(),
            DiffOpts :: arizona_diff:options()
        ) -> binary()
    )
].
-export_type([dynamic_list/0]).

-type token() ::
    {view_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {component_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {nested_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {list_template, Static :: static_list(), Dynamic :: dynamic_list()}
    | {view, Mod :: module(), Assigns :: arizona_view:assigns()}
    | {component, Mod :: module(), Fun :: atom(), Assigns :: arizona_view:assigns()}
    | {list, Static :: static_list(), Dynamic :: dynamic_list()}.
-export_type([token/0]).

-type rendered() ::
    [rendered_value()]
    | [template | Static :: static_list() | Dynamic :: dynamic_list()]
    | [list_template | Static :: static_list() | DynamicList :: [dynamic_list()]].
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
-include_lib("eunit/include/eunit.hrl").
doctest_test() -> doctest:module(?MODULE).
-endif.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Payload, View, ParentView, ParentSocket) -> {View, Socket} when
    Payload :: Token | Rendered,
    Token :: token(),
    Rendered :: rendered_value(),
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
render({list_template, Static, DynamicCallback, List}, View, ParentView, Socket) ->
    render_list_template(View, ParentView, Socket, Static, DynamicCallback, List);
render({view, Mod, Assigns}, _View, ParentView, Socket) ->
    render_view(ParentView, Socket, Mod, Assigns);
render({component, Mod, Fun, Assigns}, _View, ParentView, Socket) ->
    render_component(ParentView, Socket, Mod, Fun, Assigns);
render({list, Static, DynamicList}, View, ParentView, Socket) ->
    render_list(Static, DynamicList, View, ParentView, Socket);
render(List, View, ParentView, Socket) when is_list(List) ->
    fold(List, View, ParentView, Socket);
render(Bin, _View, View0, Socket) when is_binary(Bin) ->
    View = arizona_view:put_tmp_rendered(Bin, View0),
    {View, Socket}.

-spec view_template(Payload, Template) -> Token when
    Payload :: View | Bindings,
    View :: arizona_view:view(),
    Bindings :: erl_eval:binding_struct(),
    Template :: binary(),
    Token :: {view_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
view_template(Bindings, Template) when is_map(Bindings), is_binary(Template) ->
    {Static, Dynamic} = parse_template(Bindings, Template),
    {view_template, Static, Dynamic};
view_template(View, Template) ->
    Bindings = #{'View' => View},
    view_template(Bindings, Template).

-spec component_template(Payload, Template) -> Token when
    Payload :: View | Bindings,
    View :: arizona_view:view(),
    Bindings :: erl_eval:binding_struct(),
    Template :: binary(),
    Token :: {component_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
component_template(Bindings, Template) when is_map(Bindings), is_binary(Template) ->
    {Static, Dynamic} = parse_template(Bindings, Template),
    {component_template, Static, Dynamic};
component_template(View, Template) ->
    Bindings = #{'View' => View},
    component_template(Bindings, Template).

-spec nested_template(Template) -> Error when
    Template :: binary(),
    Error :: no_return().
nested_template(Template) ->
    missing_parse_transform_error(Template).

-spec nested_template(Payload, Template) -> Token when
    Payload :: ParentView | Bindings,
    ParentView :: arizona_view:view(),
    Bindings :: erl_eval:binding_struct(),
    Template :: binary(),
    Token :: {nested_template, Static, Dynamic},
    Static :: static_list(),
    Dynamic :: dynamic_list().
nested_template(Bindings, Template) when is_map(Bindings), is_binary(Template) ->
    {Static, Dynamic} = parse_template(Bindings, Template),
    {nested_template, Static, Dynamic};
nested_template(ParentView, Template) ->
    Bindings = #{'View' => ParentView},
    nested_template(Bindings, Template).

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

-spec list(Callback, List) -> Token when
    Callback :: fun((Item :: dynamic()) -> token() | rendered_value()),
    List :: list(),
    Token :: {list, Static, DynamicList},
    Static :: static_list(),
    DynamicList :: [dynamic_list()].
list(Callback, []) when is_function(Callback, 1) ->
    {list, [], []};
list(Callback, List) when is_function(Callback, 1), is_list(List) ->
    NestedTemplates = [erlang:apply(Callback, [Item]) || Item <- List],
    {nested_template, Static, _Dynamic} = hd(NestedTemplates),
    DynamicList = [Dynamic || {nested_template, _Static, Dynamic} <- NestedTemplates],
    {list, Static, DynamicList}.

-spec layout(LayoutMod, Assigns, InnerContent, Socket0) -> Layout when
    LayoutMod :: module(),
    Assigns :: arizona_view:assigns(),
    InnerContent :: token(),
    Socket0 :: arizona_socket:socket(),
    Layout :: {LayoutView, Socket1},
    LayoutView :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
layout(LayoutMod, Assigns, InnerContent, Socket) ->
    LayoutView = arizona_view:new(Assigns#{
        inner_content => [InnerContent]
    }),
    Token = arizona_component:render(LayoutMod, render, LayoutView),
    render(Token, LayoutView, LayoutView, Socket).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

missing_parse_transform_error(Template) when is_list(Template) ->
    error(missing_parse_transform, [Template], [
        {error_info, #{
            cause =>
                <<
                    "the attribute '-compile({parse_transform, arizona_transform}).' "
                    "is missing in the template module"
                >>
        }}
    ]).

render_view_template(View0, Socket0, Static, Dynamic0) ->
    {View1, Socket1} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = lists:reverse(arizona_view:tmp_rendered(View1)),
    Template = [template, Static, Dynamic],
    View2 = arizona_view:set_rendered(Template, View1),
    View3 = arizona_view:set_tmp_rendered(Template, View2),
    View = arizona_view:merge_changed_assigns(View3),
    Socket = arizona_socket:put_view(View, Socket1),
    {View, Socket}.

render_component_template(View0, Socket0, Static, Dynamic0) ->
    {View1, Socket} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = lists:reverse(arizona_view:tmp_rendered(View1)),
    Template = [template, Static, Dynamic],
    View2 = arizona_view:set_rendered(Template, View1),
    View = arizona_view:set_tmp_rendered(Template, View2),
    {View, Socket}.

render_nested_template(ParentView0, Socket0, Static, Dynamic0) ->
    Assigns = arizona_view:assigns(ParentView0),
    View0 = arizona_view:new(Assigns),
    {View1, Socket} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = arizona_view:tmp_rendered(View1),
    Template = [template, Static, Dynamic],
    ParentView = arizona_view:put_tmp_rendered(Template, ParentView0),
    {ParentView, Socket}.

render_list_template(View0, ParentView0, Socket, Static, Callback, List) ->
    View = arizona_view:new(arizona_view:assigns(View0)),
    DynamicList = render_dynamic_list_callback(List, Callback, View, View, Socket),
    Template = [list_template, Static, DynamicList],
    ParentView = arizona_view:put_tmp_rendered(Template, ParentView0),
    {ParentView, Socket}.

render_dynamic_list_callback([], _Callback, _View, _ParentView, _Socket) ->
    [];
render_dynamic_list_callback([Item | T], Callback, View, ParentView, Socket) ->
    Dynamic = erlang:apply(Callback, [Item]),
    {RenderedView, _Socket} = render(Dynamic, View, ParentView, Socket),
    [
        arizona_view:tmp_rendered(RenderedView)
        | render_dynamic_list_callback(T, Callback, View, ParentView, Socket)
    ].

render_view(ParentView0, Socket0, Mod, Assigns) ->
    case arizona_view:mount(Mod, Assigns, Socket0) of
        {ok, View0} ->
            Token = arizona_view:render(View0),
            {View, Socket1} = render(Token, View0, ParentView0, Socket0),
            Rendered = arizona_view:tmp_rendered(View),
            ParentView = arizona_view:put_tmp_rendered(Rendered, ParentView0),
            Socket = arizona_socket:put_view(View, Socket1),
            {ParentView, Socket};
        ignore ->
            {ParentView0, Socket0}
    end.

render_component(ParentView0, Socket0, Mod, Fun, Assigns) ->
    View0 = arizona_view:new(Assigns),
    Token = arizona_component:render(Mod, Fun, View0),
    {View, Socket1} = render(Token, View0, ParentView0, Socket0),
    Rendered = arizona_view:tmp_rendered(View),
    ParentView1 = arizona_view:put_rendered(Rendered, ParentView0),
    ParentView = arizona_view:put_tmp_rendered(Rendered, ParentView1),
    {ParentView, Socket1}.

render_list(Static, DynamicList0, View0, ParentView0, Socket) ->
    View = arizona_view:new(arizona_view:assigns(View0)),
    DynamicList = render_dynamic_list(DynamicList0, View, Socket),
    Rendered = [list, Static, DynamicList],
    ParentView1 = arizona_view:put_rendered(Rendered, ParentView0),
    ParentView = arizona_view:put_tmp_rendered(Rendered, ParentView1),
    {ParentView, Socket}.

render_dynamic_list([], _View, _Socket) ->
    [];
render_dynamic_list([Dynamic | T], View, Socket) ->
    {RenderedView, _Socket} = render_dynamic(Dynamic, View, Socket),
    [arizona_view:tmp_rendered(RenderedView) | render_dynamic_list(T, View, Socket)].

render_dynamic([], View, Socket) ->
    {View, Socket};
render_dynamic([Callback | T], View0, Socket0) ->
    {View, Socket} = erlang:apply(Callback, [View0, Socket0, _DiffOpts = #{}]),
    render_dynamic(T, View, Socket).

fold([], View, ParentView0, Socket) ->
    Rendered = arizona_view:tmp_rendered(View),
    ParentView = arizona_view:put_tmp_rendered(Rendered, ParentView0),
    {ParentView, Socket};
fold([Dynamic | T], View0, ParentView, Socket0) ->
    {View, Socket} = render(Dynamic, View0, ParentView, Socket0),
    fold(T, View, ParentView, Socket).

parse_template(Bindings, Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    {StaticAst, DynamicAst} = arizona_parser:parse(Tokens, #{}),
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
