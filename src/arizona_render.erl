-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([template/3]).
-export([view/2]).

%

-ignore_xref([template/3]).
-ignore_xref([view/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type callback() :: fun(
    (View0 :: arizona_view:view(), Socket0 :: arizona_socket:socket()) -> {
        View1 :: arizona_view:view(), Socket1 :: arizona_socket:socket()
    }
).
-export_type([callback/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~""""
Renders a top level template.

## Examples

```
> ViewMod = mymodule.
> Assigns = #{id => ~"foo", bar => ~"bar"}.
> ChangedAssigns = #{}.
> Rendered = [].
> View = arizona_view:new(ViewMod, Assigns, ChangedAssigns, Rendered).
> Views = #{}.
> Socket = arizona_socket:new(Views).
> arizona_render:template(View, Socket, ~"""
  <div id="{arizona_view:get_assign(id, View)}">
    {arizona_view:get_assign(bar, View)}
  </div>
  """).
{{view,mymodule,
       #{id => <<"foo">>,bar => <<"bar">>},
       #{},
       [template,
        [<<"<div id=\"">>,<<"\">">>,<<"</div>">>],
        [<<"foo">>,<<"bar">>]]},
 {socket,#{<<"foo">> =>
               {view,mymodule,
                     #{id => <<"foo">>,bar => <<"bar">>},
                     #{},
                     [template,
                      [<<"<div id=\"">>,<<"\">">>,<<"</div>">>],
                      [<<"foo">>,<<"bar">>]]}}}}
```

## Returns

It returns `{View, Socket}` where View is `t:arizona_view:view/0` and
Socket is `t:arizona_socket:socket/0`.
"""".
-spec template(View0, Socket0, Template) -> {View1, Socket1} when
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()],
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
template(View, Socket, {Static, Dynamic}) ->
    render_template(View, Socket, Static, Dynamic);
template(View, Socket, Template) when is_binary(Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    template(View, Socket, {Static, Dynamic}).

-doc ~"""
Renders a view template.

Call it when rendering an `t:arizona_view:view/0` inside a top level template.

## Examples

```
> ParentAssigns = #{id => ~"app", count => 0}.
> Socket = arizona_socket:new(#{}).
> {ok, ParentView} = arizona_view:mount(arizona_example_template, ParentAssigns, Socket),
> Callback = arizona_render:view(arizona_example_counter, #{
      id => ~"counter",
      count => arizona_view:get_assign(count, ParentView)
  }).
> erlang:apply(Callback, [ParentView, Socket]).
{{view,arizona_example_template,
       #{count => 0,id => <<"app">>},
       #{},
       [[template,
         [<<"<div id=\"">>,<<"\">">>,<<"</div>">>],
         [<<"counter">>,<<"0">>]]]},
 {socket,#{<<"counter">> =>
               {view,arizona_example_counter,
                     #{count => 0,id => <<"counter">>},
                     #{},[]}}}}
```

## Returns

It returns a `t:callback/0` function that receives the parent view that is
calling it and the updated socket.
""".
-spec view(Mod, Assigns) -> Callback when
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Callback :: callback().
view(Mod, Assigns) when is_atom(Mod), is_map(Assigns) ->
    fun(ParentView, Socket) ->
        render_view(ParentView, Socket, Mod, Assigns)
    end.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

render_template(View0, Socket0, Static, Dynamic0) ->
    {View1, Socket1} = render_dynamic(Dynamic0, View0, Socket0),
    Dynamic = lists:reverse(arizona_view:rendered(View1)),
    Template = [template, Static, Dynamic],
    View2 = arizona_view:set_rendered(Template, View1),
    View = arizona_view:merge_changed_assigns(View2),
    Socket = arizona_socket:put_view(View, Socket1),
    {View, Socket}.

render_view(ParentView0, Socket0, Mod, Assigns) ->
    case arizona_view:mount(Mod, Assigns, Socket0) of
        {ok, View0} ->
            {View1, Socket1} = arizona_view:render(Mod, View0, Socket0),
            Rendered = arizona_view:rendered(View1),
            ParentView = arizona_view:put_rendered(Rendered, ParentView0),
            View = arizona_view:set_rendered([], View1),
            Socket = arizona_socket:put_view(View, Socket1),
            {ParentView, Socket};
        ignore ->
            {ParentView0, Socket0}
    end.

render_dynamic([], View, Socket) ->
    {View, Socket};
render_dynamic([Callback | T], View0, Socket0) when is_function(Callback, 2) ->
    Value = erlang:apply(Callback, [View0, Socket0]),
    {View, Socket} = put_rendered(Value, View0, Socket0),
    render_dynamic(T, View, Socket);
render_dynamic([Value | T], View0, Socket0) ->
    {View, Socket} = put_rendered(Value, View0, Socket0),
    render_dynamic(T, View, Socket).

put_rendered({RenderedView, Socket}, _View, _Socket) ->
    {RenderedView, Socket};
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
