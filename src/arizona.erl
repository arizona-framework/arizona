-module(arizona).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_view_template/2]).
-export([render_component_template/2]).
-export([render_nested_template/1]).
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
%% API function definitions
%% --------------------------------------------------------------------

-spec render_view_template(Payload, Template) -> Token when
    Payload :: View | Bindings,
    View :: arizona_view:view(),
    Bindings :: erl_eval:binding_struct(),
    Template :: binary() | {file, file:filename_all()},
    Token :: {view_template, Static, Dynamic},
    Static :: arizona_renderer:static_list(),
    Dynamic :: arizona_renderer:dynamic_list().
render_view_template(Payload, Template) ->
    arizona_renderer:render_view_template(Payload, Template).

-spec render_component_template(Payload, Template) -> Token when
    Payload :: View | Bindings,
    View :: arizona_view:view(),
    Bindings :: erl_eval:binding_struct(),
    Template :: binary() | {file, file:filename_all()},
    Token :: {component_template, Static, Dynamic},
    Static :: arizona_renderer:static_list(),
    Dynamic :: arizona_renderer:dynamic_list().
render_component_template(Payload, Template) ->
    arizona_renderer:render_component_template(Payload, Template).

-spec render_nested_template(Template) -> Error when
    Template :: binary(),
    Error :: no_return().
render_nested_template(Template) ->
    arizona_renderer:render_nested_template(Template).

-spec render_view(Mod, Assigns) -> Token when
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Token :: {view, Mod, Assigns}.
render_view(Mod, Assigns) ->
    arizona_renderer:render_view(Mod, Assigns).

-spec render_component(Mod, Fun, Assigns) -> Token when
    Mod :: module(),
    Fun :: atom(),
    Assigns :: arizona_view:assigns(),
    Token :: {component, Mod, Fun, Assigns}.
render_component(Mod, Fun, Assigns) ->
    arizona_renderer:render_component(Mod, Fun, Assigns).

-spec render_if_true(Cond, Callback) -> Rendered when
    Cond :: boolean(),
    Callback :: fun(() -> Rendered),
    Rendered :: arizona_renderer:rendered_value().
render_if_true(Cond, Callback) ->
    arizona_renderer:render_if_true(Cond, Callback).

-spec render_list(Callback, List) -> Token when
    Callback :: fun(
        (Item :: dynamic()) -> arizona_renderer:token() | arizona_renderer:rendered_value()
    ),
    List :: list(),
    Token :: {list, Static, [DynamicList]},
    Static :: arizona_renderer:static_list(),
    DynamicList :: arizona_renderer:dynamic_list().
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
    Assigns :: arizona_view:assigns(),
    View :: arizona_view:view().
new_view(Mod, Assigns) ->
    arizona_view:new(Mod, Assigns).

-spec put_assign(Key, Value, View0) -> View1 when
    Key :: atom(),
    Value :: dynamic(),
    View0 :: arizona_view:view(),
    View1 :: arizona_view:view().
put_assign(Key, Value, View) ->
    arizona_view:put_assign(Key, Value, View).

-spec put_assigns(Assigns, View0) -> View1 when
    Assigns :: arizona_view:assigns(),
    View0 :: arizona_view:view(),
    View1 :: arizona_view:view().
put_assigns(Assigns, View) ->
    arizona_view:put_assigns(Assigns, View).

-spec get_assign(Key, View) -> Value when
    Key :: atom(),
    View :: arizona_view:view(),
    Value :: dynamic().
get_assign(Key, View) ->
    arizona_view:get_assign(Key, View).

-spec get_assign(Key, View, Default) -> Value when
    Key :: atom(),
    View :: arizona_view:view(),
    Value :: Default | dynamic().
get_assign(Key, View, Default) ->
    arizona_view:get_assign(Key, View, Default).

-spec generate_static() -> ok.
generate_static() ->
    arizona_static:generate().
