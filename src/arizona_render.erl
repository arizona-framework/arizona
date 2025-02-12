-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([template/3]).

%

-ignore_xref([template/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec template(View0, Socket0, Template) -> {View1, Socket1} when
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()],
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
template(View, Socket, {Static, Dynamic}) ->
    render_view_template(View, Socket, Static, Dynamic);
template(View, Socket, Template) when is_binary(Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    template(View, Socket, {Static, Dynamic}).

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

render_dynamic([], View, Socket) ->
    {View, Socket};
render_dynamic([Value | T], View0, Socket0) ->
    {View, Socket} = put_rendered(Value, View0, Socket0),
    render_dynamic(T, View, Socket).

put_rendered(Value, View0, Socket) ->
    View = arizona_view:put_rendered(Value, View0),
    {View, Socket}.

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
