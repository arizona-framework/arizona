-module(arizona_renderer).
-moduledoc ~"""
Provides template rendering functionality for Arizona LiveView components.

## Overview

The renderer module handles the rendering of parsed template data into HTML output.
It supports different rendering modes optimized for stateful components, stateless
components, and list rendering with efficient diff-based updates.

## Features

- **Stateful Rendering**: Optimized rendering for stateful components with element indexing
- **Stateless Rendering**: Lightweight rendering for stateless components
- **List Rendering**: Efficient list rendering with static/dynamic separation
- **Error Handling**: Comprehensive error reporting with line number information
- **Socket Management**: Proper socket state management throughout rendering
- **Binding Resolution**: Dynamic binding evaluation with error context

## Key Functions

- `render_stateful/2`: Render stateful component templates with indexed elements
- `render_stateless/2`: Render stateless component templates as linear lists
- `render_list/3`: Render list templates with static/dynamic optimization
- `render_element/2`: Render individual template elements
- `evaluate_single_dynamic_element/4`: Evaluate dynamic elements for list items
- `format_error/2`: Format rendering errors with context information

## Template Data Types

The module works with parsed template data from the Arizona parse transform,
which provides optimized data structures for different rendering scenarios.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([render_list/3]).
-export([render_element/2]).
-export([evaluate_single_dynamic_element/4]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([stateful_template_data/0]).
-export_type([stateless_template_data/0]).
-export_type([list_template_data/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Template data from parse transform optimized for stateful component rendering.

Contains indexed elements with order information and variable mappings for
efficient rendering and diff operations.
""".
-type stateful_template_data() :: #{
    elems_order := [Index :: non_neg_integer()],
    elems := #{
        Index ::
            non_neg_integer() =>
                {
                    Category :: static, Line :: pos_integer(), Content :: binary()
                }
                | {
                    Category :: dynamic,
                    Line :: pos_integer(),
                    Content :: fun((arizona_socket:socket()) -> term())
                }
    },
    vars_indexes := #{VarName :: atom() => [Index :: non_neg_integer()]}
}.

-doc ~"""
Template data from parse transform optimized for stateless component rendering.

Simple list structure with static and dynamic elements for lightweight rendering.
""".
-type stateless_template_data() :: [
    {static, pos_integer(), binary()}
    | {dynamic, pos_integer(), fun((arizona_socket:socket()) -> term())}
].

-doc ~"""
Template data from parse transform optimized for list rendering with static/dynamic separation.

Separates static template parts from dynamic data evaluation for efficient
list rendering and updates.
""".
-type list_template_data() :: #{
    static := [binary()],
    dynamic := #{
        elems_order := [Index :: non_neg_integer()],
        elems := #{
            Index ::
                non_neg_integer() => {
                    Category :: dynamic,
                    Line :: pos_integer(),
                    ElementFun :: fun((Item :: term(), Socket :: arizona_socket:socket()) -> term())
                }
        },
        vars_indexes := #{VarName :: atom() => [Index :: non_neg_integer()]}
    }
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Render stateful component template data into HTML.

Processes stateful template data with indexed elements for efficient rendering
and diff operations. Updates the socket with the rendered HTML.

## Examples

```erlang
1> TemplateData = #{elems_order => [0, 1], elems => #{0 => {static, 1, <<"Hello">>}}}.
#{...}
2> arizona_renderer:render_stateful(TemplateData, Socket).
{[<<"Hello">>], UpdatedSocket}
```
""".
-spec render_stateful(TemplateData, Socket) -> {Html, Socket1} when
    TemplateData :: stateful_template_data(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateful(#{elems_order := Order, elems := Elements}, Socket) ->
    {Html, UpdatedSocket} = render_elements(Order, Elements, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

-doc ~"""
Render stateless component template data into HTML.

Processes stateless template data as a linear list of elements for lightweight
rendering without indexed access. Updates the socket with the rendered HTML.

## Examples

```erlang
1> TemplateData = [{static, 1, <<"Hello">>}, {static, 2, <<"World">>}].
[...]
2> arizona_renderer:render_stateless(TemplateData, Socket).
{[<<"Hello">>, <<"World">>], UpdatedSocket}
```
""".
-spec render_stateless(StructuredList, Socket) -> {Html, Socket1} when
    StructuredList :: stateless_template_data(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {Html, UpdatedSocket} = render_iolist(StructuredList, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

-doc ~"""
Render list template with static/dynamic optimization for efficient list rendering.

Processes list template data with separated static and dynamic parts, rendering
each item against the template structure for optimal performance.

## Examples

```erlang
1> ListData = #{static => [<<"<li>">>], dynamic => #{...}}.
#{...}
2> Items = [item1, item2, item3].
[...]
3> arizona_renderer:render_list(ListData, Items, Socket).
{[<<"<li>item1</li>">>, <<"<li>item2</li>">>, <<"<li>item3</li>">>], UpdatedSocket}
```
""".
-spec render_list(ListData, Items, Socket) -> {Html, Socket1} when
    ListData :: list_template_data(),
    Items :: [term()],
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_list(ListData, Items, Socket) when is_map(ListData), is_list(Items) ->
    #{static := StaticParts, dynamic := DynamicSpec} = ListData,
    #{elems_order := ElemsOrder, elems := ElemsFuns} = DynamicSpec,

    %% Just accumulate socket through each item render
    %% Note: List diffing works automatically through existing vars_indexes system
    %% When arizona_socket:get_binding(list, Socket) changes, the parent
    %% stateful component detects this and rerenders this dynamic element
    {Html, FinalSocket} = lists:foldl(
        fun(Item, {HtmlAcc, AccSocket}) ->
            {ItemHtml, UpdatedSocket} = render_list_item(
                StaticParts, ElemsOrder, ElemsFuns, Item, AccSocket
            ),
            {[HtmlAcc, ItemHtml], UpdatedSocket}
        end,
        {[], Socket},
        Items
    ),

    FinalSocket1 = arizona_socket:set_html_acc(Html, FinalSocket),
    {Html, FinalSocket1}.

-doc ~"""
Render a single template element (static or dynamic).

Processes individual template elements, handling both static content and
dynamic function evaluation with comprehensive error handling.

## Examples

```erlang
1> arizona_renderer:render_element({static, 1, <<"Hello">>}, Socket).
{<<"Hello">>, Socket}
2> arizona_renderer:render_element({dynamic, 1, Fun}, Socket).
{<<"Dynamic content">>, UpdatedSocket}
```
""".
-spec render_element(Element, Socket) -> {Content, Socket1} when
    Element :: {static, pos_integer(), binary()} | {dynamic, pos_integer(), function()},
    Socket :: arizona_socket:socket(),
    Content :: term(),
    Socket1 :: arizona_socket:socket().
render_element({static, _Line, Content}, Socket) when is_binary(Content) ->
    {Content, Socket};
render_element({dynamic, Line, Fun}, Socket) when is_function(Fun, 1) ->
    try
        Result = arizona_stateful:call_dynamic_function(Fun, Socket),
        arizona_html:to_html(Result, Socket)
    catch
        throw:{binding_not_found, Key} ->
            error({binding_not_found, Key}, none, binding_error_info(Line, Key, Socket));
        _:Error ->
            error({template_render_error, Error, Line})
    end.

-doc ~"""
Evaluate a single dynamic element for list item rendering.

Extracted function for evaluating dynamic elements within list templates,
with proper error handling and context information.

## Examples

```erlang
1> arizona_renderer:evaluate_single_dynamic_element(0, ElemsFuns, Item, Socket).
{<<"Rendered item content">>, UpdatedSocket}
```
""".
-spec evaluate_single_dynamic_element(ElemIndex, ElemsFuns, Item, Socket) -> {Content, Socket1} when
    ElemIndex :: non_neg_integer(),
    ElemsFuns :: map(),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Content :: term(),
    Socket1 :: arizona_socket:socket().
evaluate_single_dynamic_element(ElemIndex, ElemsFuns, Item, Socket) ->
    %% Evaluate element function - let it crash if index doesn't exist
    {dynamic, Line, Fun} = maps:get(ElemIndex, ElemsFuns),
    try
        Result = arizona_list:call_element_function(Fun, Item, Socket),
        arizona_html:to_html(Result, Socket)
    catch
        throw:{binding_not_found, Key} ->
            error({binding_not_found, Key}, none, binding_error_info(Line, Key, Socket));
        _:Error ->
            error({list_item_render_error, Error, Item, Line})
    end.

-doc ~"""
Format rendering errors with context information for better debugging.

OTP error_info callback for enhanced error formatting. Provides detailed error
messages including line numbers, template modules, and binding information
when available.

## Examples

```erlang
1> arizona_renderer:format_error(Reason, StackTrace).
#{general => "Template rendering error", reason => "arizona_renderer: ..."}
```
""".
-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: term(),
    StackTrace :: [term()],
    ErrorMap :: #{atom() => term()}.
format_error(Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    CauseMap = maps:get(cause, ErrorInfo, #{}),
    CauseMap#{
        general => "Template rendering error",
        reason => io_lib:format("arizona_renderer: ~p", [Reason])
    };
format_error(Reason, _StackTrace) ->
    #{
        general => "Template rendering error",
        reason => io_lib:format("arizona_renderer: ~p", [Reason])
    }.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Render elements in order for stateful templates
render_elements([], _Elements, Socket, Acc) ->
    Html = lists:reverse(Acc),
    {Html, Socket};
render_elements([Index | Rest], Elements, Socket, Acc) ->
    #{Index := Element} = Elements,
    {RenderedElement, UpdatedSocket} = render_element(Element, Socket),
    render_elements(Rest, Elements, UpdatedSocket, [RenderedElement | Acc]).

%% Render stateless iolist
render_iolist([], Socket, Acc) ->
    Html = lists:reverse(Acc),
    {Html, Socket};
render_iolist([Element | Rest], Socket, Acc) ->
    {RenderedElement, UpdatedSocket} = render_element(Element, Socket),
    render_iolist(Rest, UpdatedSocket, [RenderedElement | Acc]).

%% Render a single list item using template structure
render_list_item(StaticParts, ElemsOrder, ElemsFuns, Item, Socket) ->
    %% Evaluate dynamic elements for this item
    {DynamicValues, UpdatedSocket} = evaluate_dynamic_elements_for_item(
        ElemsOrder, ElemsFuns, Item, Socket
    ),

    %% Zip static and dynamic parts together
    ItemHtml = zip_static_dynamic(StaticParts, DynamicValues),

    {ItemHtml, UpdatedSocket}.

%% Evaluate dynamic elements for a list item
evaluate_dynamic_elements_for_item([], _ElemsFuns, _Item, Socket) ->
    {[], Socket};
evaluate_dynamic_elements_for_item([ElemIndex | Rest], ElemsFuns, Item, Socket) ->
    %% Evaluate current element using extracted function
    {Value, UpdatedSocket} = evaluate_single_dynamic_element(ElemIndex, ElemsFuns, Item, Socket),

    %% Recursively evaluate rest
    {RestValues, FinalSocket} = evaluate_dynamic_elements_for_item(
        Rest, ElemsFuns, Item, UpdatedSocket
    ),

    {[Value | RestValues], FinalSocket}.

%% Zip static and dynamic parts for list item
zip_static_dynamic([], []) ->
    [];
zip_static_dynamic([S | Static], [D | Dynamic]) ->
    [S, D | zip_static_dynamic(Static, Dynamic)];
zip_static_dynamic([S | Static], []) ->
    [S | zip_static_dynamic(Static, [])];
zip_static_dynamic([], [D | Dynamic]) ->
    [D | zip_static_dynamic([], Dynamic)].

%% Error info for binding errors following OTP pattern
binding_error_info(Line, Key, Socket) ->
    CurrentState = arizona_socket:get_current_stateful_state(Socket),
    TemplateModule = arizona_stateful:get_module(CurrentState),
    [
        {error_info, #{
            cause => #{binding => Key, line => Line, template_module => TemplateModule},
            module => arizona_renderer
        }}
    ].
