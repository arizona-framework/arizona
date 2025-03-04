-module(arizona).
-moduledoc ~"""
Provides core rendering functionality for building web applications using the
Arizona Framework. It serves as the primary interface for creating stateful views,
reusable components, application layouts, and dynamic template fragments.

## Overview

Arizona follows a component-based architecture where:

- `Views`: are stateful entities that manage dynamic data through bindings
- `Components`: are stateless UI elements that inherit parent view state
- `Nested Templates`: handle conditional blocks and reusable fragments
- `Layout`: define static application structure (rendered once on mount)

## Key Functions

- `render_view_template/2`: Main view renderer with stateful bindings (DOM-patched)
- `render_component_template/2`: Stateless component renderer for UI composition
- `render_nested_template/1`: Dynamic fragment renderer for conditional content
- `render_layout_template/2`: Structural wrapper for views (initial render only)
""".

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
-export([put_binding/3]).
-export([put_bindings/2]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_path_param/2]).
-export([get_path_param/3]).
-export([get_query_param/2]).
-export([get_query_param/3]).
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
-ignore_xref([put_binding/3]).
-ignore_xref([put_bindings/2]).
-ignore_xref([get_binding/2]).
-ignore_xref([get_binding/3]).
-ignore_xref([get_path_param/2]).
-ignore_xref([get_path_param/3]).
-ignore_xref([get_query_param/2]).
-ignore_xref([get_query_param/3]).
-ignore_xref([generate_static/0]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type view() :: arizona_view:view().
-export_type([view/0]).

-type bindings() :: arizona_view:bindings().
-export_type([bindings/0]).

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

-type rendered_layout_template() :: rendered_component_template().
-export_type([rendered_layout_template/0]).

-type rendered_view() :: {view, Mod :: module(), Bindings :: bindings()}.
-export_type([rendered_view/0]).

-type rendered_component() :: {component, Mod :: module(), Fun :: atom(), Bindings :: bindings()}.
-export_type([rendered_component/0]).

-type rendered_value() :: arizona_renderer:rendered_value().
-export_type([rendered_value/0]).

-type rendered_list() ::
    {list, Static :: arizona_renderer:static_list(),
        DynamicList :: [arizona_renderer:dynamic_list()]}.
-export_type([rendered_list/0]).

-type handle_params_ret() :: arizona_view:handle_params_ret().
-export_type([handle_params_ret/0]).

-type mount_ret() :: arizona_view:mount_ret().
-export_type([mount_ret/0]).

-type handle_event_ret() :: arizona_view:handle_event_ret().
-export_type([handle_event_ret/0]).

-type handle_join_ret() :: arizona_view:handle_join_ret().
-export_type([handle_join_ret/0]).

-type event_name() :: arizona_view:event_name().
-export_type([event_name/0]).

-type event_payload() :: arizona_view:event_payload().
-export_type([event_payload/0]).

-type path_params() :: arizona_websocket:path_params().
-export_type([path_params/0]).

-type query_params() :: arizona_websocket:query_params().
-export_type([query_params/0]).

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
Renders a view template, interpolating dynamic data from the view's state (`View`)
into the provided `Template`.

This function is typically called within the `c:arizona_view:render/1` callback
to render the view's template.

## Parameters

- `View`: The current view state (`t:view/0`), which contains the bindings and other
  metadata used to populate the template.
- `Template`: The template to render. This can be either:

  - A binary string containing the template directly.
  - A tuple `{file, Filename}` specifying a file from which to load the template.

## Returns

The rendered template as `t:rendered_view_template/0`.

## Usage Notes

- The template can include placeholders (e.g., `{arizona:get_binding(id, View)}`) to
  dynamically insert data from the view's bindings.
- **Important**: The `id` binding is **required** and must be set by the consumer.
  The `id` should be unique and binded to a top-level HTML element (e.g., a `div`)
  in the template. This ensures proper DOM patching and state management. For example:

  ```erlang
  render(View) ->
      arizona:render_view_template(View, ~"""
      <div id="{arizona:get_binding(id, View)}">
          Hello, {arizona:get_binding(name, View, ~"World")}!
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
Renders a component template, interpolating dynamic data from the provided View state
into the Template.

This function is used within component modules to generate content for stateless UI
elements.

## Parameters

- `View`: The current view state (`t:arizona_view:view/0`), which contains the bindings
  and other metadata used to populate the template. Components inherit their state from
  the parent view that calls them.
- `Template`: The template to render. This can be either:

  - A binary string containing the template directly.
  - A tuple `{file, Filename}` specifying a file from which to load the template.

## Returns

The rendered template as `t:rendered_component_template/0`.

## Usage Notes

- This function is typically called within a component module to render the component's
  template. For example:

  ```erlang
  button(View) ->
      arizona:render_component_template(View, ~"""
      <button type="{arizona:get_binding(type, View, ~"button")}">
          {arizona:get_binding(text, View)}
      </button>
      """).
  ```

- The template can include placeholders (e.g., `{arizona:get_binding(type, View, ~"button")}`)
  to dynamically insert data from the view's bindings.
- Components are **stateless** and rely on the state of the parent view. They are
  designed to be reusable and lightweight.
"""".
-spec render_component_template(View, Template) -> Rendered when
    View :: arizona_view:view(),
    Template :: binary() | {file, file:filename_all()},
    Rendered :: rendered_component_template().
render_component_template(Payload, Template) ->
    arizona_renderer:render_component_template(Payload, Template).

-doc ~"""""
Renders a nested template, which is a stateless fragment of HTML or other content.

## Parameters

- `Template`: A binary string containing the template to render. This template can
  include placeholders for dynamic data.

## Returns

The rendered template as `t:rendered_nested_template/0`.

## Usage Notes

- This function is used to dynamically generate smaller pieces of content within a
  larger template, such as conditional blocks or reusable snippets/fragments.
  For example:

  ```erlang
  render_user_profile(View) ->
      arizona:render_component_template(View, ~""""
      <div class="profile">
          <h1>User Profile</h1>
          {case arizona:get_binding(user_role, View) of
              admin ->
                  arizona:render_nested_template(~"""
                  <div class="admin-badge">
                      <span>Admin</span>
                      <button>Edit Profile</button>
                  </div>
                  """);
              member ->
                  arizona:render_nested_template(~"""
                  <div class="member-badge">
                      <span>Member</span>
                  </div>
                  """);
              guest ->
                  arizona:render_nested_template(~"""
                  <div class="guest-badge">
                      <span>Guest</span>
                      <button>Sign Up</button>
                  </div>
                  """)
          end}
          <p>Welcome, {arizona:get_binding(username, View)}!</p>
      </div>
      """").
  ```

- Nested templates are stateless and do not maintain their own state. They rely on
  data passed through the parent template or view.
- Use this function to keep templates modular and avoid repetition, especially for
  conditional rendering or reusable UI elements.
""""".
-spec render_nested_template(Template) -> Rendered when
    Template :: binary(),
    Rendered :: rendered_nested_template().
render_nested_template(Template) ->
    arizona_renderer:render_nested_template(Template).

-doc ~""""
Renders a layout template, which provides a consistent structure (e.g., HTML
wrappers, headers, footers) for views. Layouts are **stateless** and are rendered
**only once** when the view is first mounted.

They receive a special binding called `inner_content`, which represents the rendered
content of the view being wrapped.

## Parameters

- `View`: The current view state (`t:view/0`), which contains the bindings and other
  metadata used to populate the template.
- `Template`: The template to render. This can be either:

  - A binary string containing the template directly.
  - A tuple `{file, Filename}` specifying a file from which to load the template.

## Returns

The rendered template as `t:rendered_layout_template/0`.

## Usage Notes

- Layouts are used to wrap views in a consistent structure, such as a common HTML
  template with headers, footers, and navigation. For Example:

  ```erlang
  render(View) ->
      arizona:render_layout_template(View, ~"""
      <!DOCTYPE html>
      <html lang="en">
      <head>
          <meta charset="UTF-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>{arizona:get_binding(title, View)}</title>
          {arizona:render_html_scripts()}
      </head>
      <body>
          {arizona:get_binding(inner_content, View)}
      </body>
      </html>
      """).
  ```

- The inner_content binding is automatically provided by Arizona and represents the
  rendered content of the view being wrapped. It should be placed in the layout
  template where the view's content should appear.
- Layouts are rendered only once and do not re-renders after their initial render.
"""".
-spec render_layout_template(View, Template) -> Rendered when
    View :: view(),
    Template :: binary() | {file, file:filename_all()},
    Rendered :: rendered_layout_template().
render_layout_template(Payload, Template) ->
    arizona_renderer:render_component_template(Payload, Template).

-spec render_view(Mod, Bindings) -> Rendered when
    Mod :: module(),
    Bindings :: bindings(),
    Rendered :: rendered_view().
render_view(Mod, Bindings) ->
    arizona_renderer:render_view(Mod, Bindings).

-spec render_component(Mod, Fun, Bindings) -> Rendered when
    Mod :: module(),
    Fun :: atom(),
    Bindings :: bindings(),
    Rendered :: rendered_component().
render_component(Mod, Fun, Bindings) ->
    arizona_renderer:render_component(Mod, Fun, Bindings).

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

-spec new_view(Mod, Bindings) -> View when
    Mod :: module(),
    Bindings :: bindings(),
    View :: view().
new_view(Mod, Bindings) ->
    arizona_view:new(Mod, Bindings).

-spec put_binding(Key, Value, View0) -> View1 when
    Key :: atom(),
    Value :: dynamic(),
    View0 :: view(),
    View1 :: view().
put_binding(Key, Value, View) ->
    arizona_view:put_binding(Key, Value, View).

-spec put_bindings(Bindings, View0) -> View1 when
    Bindings :: bindings(),
    View0 :: view(),
    View1 :: view().
put_bindings(Bindings, View) ->
    arizona_view:put_bindings(Bindings, View).

-spec get_binding(Key, View) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: dynamic().
get_binding(Key, View) ->
    arizona_view:get_binding(Key, View).

-spec get_binding(Key, View, Default) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: Default | dynamic().
get_binding(Key, View, Default) ->
    arizona_view:get_binding(Key, View, Default).

-spec get_path_param(Key, PathParams) -> Value when
    Key :: atom(),
    PathParams :: path_params(),
    Value :: dynamic().
get_path_param(Key, PathParams) when is_atom(Key), is_map(PathParams) ->
    maps:get(Key, PathParams).

-spec get_path_param(Key, PathParams, Default) -> Value when
    Key :: atom(),
    PathParams :: path_params(),
    Value :: dynamic() | Default.
get_path_param(Key, PathParams, Default) when is_atom(Key), is_map(PathParams) ->
    maps:get(Key, PathParams, Default).

-spec get_query_param(Key, QueryParams) -> Value when
    Key :: atom(),
    QueryParams :: query_params(),
    Value :: binary().
get_query_param(Key0, QueryParams) when is_atom(Key0), is_list(QueryParams) ->
    Key = atom_to_binary(Key0, utf8),
    {Key, Value} = proplists:lookup(Key, QueryParams),
    Value.

-spec get_query_param(Key, QueryParams, Default) -> Value when
    Key :: atom(),
    QueryParams :: query_params(),
    Value :: binary() | Default.
get_query_param(Key0, QueryParams, Default) when is_atom(Key0), is_list(QueryParams) ->
    Key = atom_to_binary(Key0, utf8),
    proplists:get_value(Key, QueryParams, Default).

-spec generate_static() -> ok.
generate_static() ->
    arizona_static:generate().
