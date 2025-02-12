-module(arizona_render).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([view_template/3]).
-export([component_template/3]).
-export([view/2]).
-export([component/3]).

%

-ignore_xref([view_template/3]).
-ignore_xref([component_template/3]).
-ignore_xref([view/2]).
-ignore_xref([component/3]).

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
Renders a view template.

Call it in the `c:arizona_view:render/2` callback of an `t:arizona_view:view/0`.

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
-spec view_template(View0, Socket0, Template) -> {View1, Socket1} when
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()],
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
view_template(View, Socket, {Static, Dynamic}) ->
    render_view_template(View, Socket, Static, Dynamic);
view_template(View, Socket, Template) when is_binary(Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    view_template(View, Socket, {Static, Dynamic}).

-doc ~""""
Renders a component template.

Call it in a component callback.

## Examples

There is no examples for now.

## Returns

It returns `{View, Socket}` where View is `t:arizona_view:view/0` and
Socket is `t:arizona_socket:socket/0`.
"""".
-spec component_template(View0, Socket0, Template) -> {View1, Socket1} when
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    Template :: binary() | {Static, Dynamic},
    Static :: [binary()],
    Dynamic :: [binary()],
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
component_template(View, Socket, {Static, Dynamic}) ->
    render_component_template(View, Socket, Static, Dynamic);
component_template(View, Socket, Template) ->
    Bindings = #{'View' => View, 'Socket' => Socket},
    {Static, Dynamic} = parse_template(Bindings, Template),
    component_template(View, Socket, {Static, Dynamic}).

-doc ~"""
Renders a nested view.

Call it when rendering an `t:arizona_view:view/0` inside an `arizona_render:view_template/3`
or inside an `arizona_render:component_template/3`.

The `t:arizona_view:id/0` assign is required.

## Examples

```
> ParentAssigns = #{id => ~"app", count => 0}.
> Socket = arizona_socket:new(#{}).
> {ok, ParentView} = arizona_view:mount(arizona_example_template, ParentAssigns, Socket).
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
view(Mod, Assigns) when is_atom(Mod), is_map(Assigns), is_map_key(id, Assigns) ->
    fun(ParentView, Socket) ->
        render_view(ParentView, Socket, Mod, Assigns)
    end.

-doc ~"""
Renders a nested component.

Call it when rendering an `t:arizona_view:view/0` inside an `arizona_render:view_template/3`
or inside an `arizona_render:component_template/3`.

## Examples

```
> ParentAssigns = #{id => ~"app"}.
> Socket = arizona_socket:new(#{}).
> {ok, ParentView} = arizona_view:mount(arizona_example_template, ParentAssigns, Socket).
> Callback = arizona_render:component(arizona_example_components, button, #{
      text => ~"Increment"
  }).
> erlang:apply(Callback, [ParentView, Socket]).
{{view,arizona_example_template,
       #{id => <<"app">>},
       #{},
       [[template,
         [<<"<button>">>,<<"</button>">>],
         [<<"Increment">>]]]},
 {socket,#{}}}
```

## Returns

It returns a `t:callback/0` function that receives the parent view that is
calling it and the updated socket.
""".
-spec component(Mod, Fun, Assigns) -> Callback when
    Mod :: module(),
    Fun :: atom(),
    Assigns :: arizona_view:assigns(),
    Callback :: callback().
component(Mod, Fun, Assigns) when is_atom(Mod), is_atom(Fun), is_map(Assigns) ->
    fun(ParentView, Socket) ->
        render_component(ParentView, Socket, Mod, Fun, Assigns)
    end.

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

render_component(ParentView0, Socket0, Mod, Fun, Assigns) ->
    View0 = arizona_view:new(undefined, Assigns),
    {View, Socket1} = arizona_component:render(Mod, Fun, View0, Socket0),
    Rendered = arizona_view:rendered(View),
    ParentView = arizona_view:put_rendered(Rendered, ParentView0),
    {ParentView, Socket1}.

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
