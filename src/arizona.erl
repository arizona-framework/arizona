-module(arizona).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_view_template/2]).
-export([render_component_template/2]).
-export([render_nested_template/1]).
-export([render_layout_template/2]).
-export([render_view/2]).
-export([render_component/3]).
-export([render_if_true/2]).
-export([render_list/2]).
-export([render_html_scripts/0]).
-export([render_js_event/3]).
-export([new_view/2]).
-export([put_assign/3]).
-export([put_assigns/2]).
-export([get_assign/2]).
-export([get_assign/3]).
-export([generate_static/0]).

%

-ignore_xref([render_view_template/2]).
-ignore_xref([render_component_template/2]).
-ignore_xref([render_nested_template/1]).
-ignore_xref([render_layout_template/2]).
-ignore_xref([render_view/2]).
-ignore_xref([render_component/3]).
-ignore_xref([render_if_true/2]).
-ignore_xref([render_list/2]).
-ignore_xref([render_html_scripts/0]).
-ignore_xref([render_js_event/3]).
-ignore_xref([new_view/2]).
-ignore_xref([put_assign/3]).
-ignore_xref([put_assigns/2]).
-ignore_xref([get_assign/2]).
-ignore_xref([get_assign/3]).
-ignore_xref([generate_static/0]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type view() :: arizona_view:view().
-export_type([view/0]).

-type assigns() :: arizona_view:assigns().
-export_type([assigns/0]).

-type socket() :: arizona_socket:socket().
-export_type([socket/0]).

-type rendered_view_template() ::
    {view_template, Static :: arizona_renderer:static_list(),
        Dynamic :: arizona_renderer:dynamic_list()}.
-export_type([rendered_view_template/0]).

-type rendered_component_template() ::
    {component_template, Static :: arizona_renderer:static_list(),
        Dynamic :: arizona_renderer:dynamic_list()}.
-export_type([rendered_component_template/0]).

-type rendered_nested_template() :: no_return().
-export_type([rendered_nested_template/0]).

-type rendered_layout_template() :: rendered_view_template().
-export_type([rendered_layout_template/0]).

-type rendered_view() :: {view, Mod :: module(), Assigns :: assigns()}.
-export_type([rendered_view/0]).

-type rendered_component() :: {component, Mod :: module(), Fun :: atom(), Assigns :: assigns()}.
-export_type([rendered_component/0]).

-type rendered_value() :: arizona_renderer:rendered_value().
-export_type([rendered_value/0]).

-type rendered_list() ::
    {list, Static :: arizona_renderer:static_list(),
        DynamicList :: [arizona_renderer:dynamic_list()]}.
-export_type([rendered_list/0]).

-type mount_ret() :: arizona_view:mount_ret().
-export_type([mount_ret/0]).

-type event_name() :: arizona_view:event_name().
-export_type([event_name/0]).

-type event_payload() :: arizona_view:event_payload().
-export_type([event_payload/0]).

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

-doc ~""""
Renders a view template.

View is a stateful element.

## Usage

```erlang
render(View) ->
    arizona:render_view_template(View, ~"""
    <div id="{arizona:get_assign(id, View)}">
        Hello, {arizona:get_assign(name, View, ~"World")}!
    </div>
    """).
```
"""".
-spec render_view_template(View, Template) -> Rendered when
    View :: view(),
    Template :: binary() | {file, file:filename_all()},
    Rendered :: rendered_view_template().
render_view_template(Payload, Template) ->
    arizona_renderer:render_view_template(Payload, Template).

-doc ~""""
Renders a component template.

Component is a stateless element.

## Usage

```erlang
button(View) ->
    arizona:render_component_template(View, ~"""
    <button type="{arizona:get_assign(type, View, ~"button")}">
        {arizona:get_assign(text, View)}
    </button>
    """).
```
"""".
-spec render_component_template(View, Template) -> Rendered when
    View :: arizona_view:view(),
    Template :: binary() | {file, file:filename_all()},
    Rendered :: rendered_component_template().
render_component_template(Payload, Template) ->
    arizona_renderer:render_component_template(Payload, Template).

-doc ~"""""
Renders a nested template.

Nested template is a stateless element.

## Usage

```erlang
example(View) ->
    arizona:render_component_template(View, ~""""
    {case arizona:get_assign(status_code, View) of
         200 ->
             arizona:render_nested_template(~"""
             <b>{arizona:get_assign(message, View)}</b>
             """);
         403 ->
             arizona:render_nested_template(~"""
             Forbidden
             """);
         Other ->
             arizona:render_nested_template(~"""
             Error: {Other}
             """)
     end}
    <dialog>
        Hello, {arizona:get_assign(name, View, ~"World")}!
    </div>
    """").
```
""""".
-spec render_nested_template(Template) -> Rendered when
    Template :: binary(),
    Rendered :: rendered_nested_template().
render_nested_template(Template) ->
    arizona_renderer:render_nested_template(Template).

-doc ~""""
Renders a layout template.

Layout is a stateful element.

It receives an assign called `inner_content` to be defined
in any location of the template.

## Usage

```erlang
render(View) ->
    arizona:render_layout_template(View, ~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{arizona:get_assign(title, View)}</title>
        {arizona:render_html_scripts()}
    </head>
    <body>
        {arizona:get_assign(inner_content, View)}
    </body>
    </html>
    """).
```
"""".
-spec render_layout_template(View, Template) -> Rendered when
    View :: view(),
    Template :: binary() | {file, file:filename_all()},
    Rendered :: rendered_layout_template().
render_layout_template(Payload, Template) ->
    arizona_renderer:render_view_template(Payload, Template).

-spec render_view(Mod, Assigns) -> Rendered when
    Mod :: module(),
    Assigns :: assigns(),
    Rendered :: rendered_view().
render_view(Mod, Assigns) ->
    arizona_renderer:render_view(Mod, Assigns).

-spec render_component(Mod, Fun, Assigns) -> Rendered when
    Mod :: module(),
    Fun :: atom(),
    Assigns :: assigns(),
    Rendered :: rendered_component().
render_component(Mod, Fun, Assigns) ->
    arizona_renderer:render_component(Mod, Fun, Assigns).

-spec render_if_true(Cond, Callback) -> Rendered when
    Cond :: boolean(),
    Callback :: fun(() -> Rendered),
    Rendered :: rendered_value().
render_if_true(Cond, Callback) ->
    arizona_renderer:render_if_true(Cond, Callback).

-spec render_list(Callback, List) -> Rendered when
    Callback :: fun(
        (Item :: dynamic()) -> arizona_renderer:token() | arizona_renderer:rendered_value()
    ),
    List :: list(),
    Rendered :: rendered_list().
render_list(Callback, List) ->
    arizona_renderer:render_list(Callback, List).

-spec render_html_scripts() -> binary().
render_html_scripts() ->
    arizona_html:scripts().

-spec render_js_event(ViewId, EventName, Payload) -> Js when
    ViewId :: arizona_view:id(),
    EventName :: binary(),
    Payload :: dynamic(),
    Js :: binary().
render_js_event(ViewId, EventName, Payload) ->
    arizona_js:send_event(ViewId, EventName, Payload).

-spec new_view(Mod, Assigns) -> View when
    Mod :: module(),
    Assigns :: assigns(),
    View :: view().
new_view(Mod, Assigns) ->
    arizona_view:new(Mod, Assigns).

-spec put_assign(Key, Value, View0) -> View1 when
    Key :: atom(),
    Value :: dynamic(),
    View0 :: view(),
    View1 :: view().
put_assign(Key, Value, View) ->
    arizona_view:put_assign(Key, Value, View).

-spec put_assigns(Assigns, View0) -> View1 when
    Assigns :: assigns(),
    View0 :: view(),
    View1 :: view().
put_assigns(Assigns, View) ->
    arizona_view:put_assigns(Assigns, View).

-spec get_assign(Key, View) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: dynamic().
get_assign(Key, View) ->
    arizona_view:get_assign(Key, View).

-spec get_assign(Key, View, Default) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: Default | dynamic().
get_assign(Key, View, Default) ->
    arizona_view:get_assign(Key, View, Default).

-spec generate_static() -> ok.
generate_static() ->
    arizona_static:generate().
