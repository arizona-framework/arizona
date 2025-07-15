-module(arizona_html).
-moduledoc ~"""
Provides HTML rendering functionality for Arizona LiveView components.

## Overview

The HTML module serves as the central rendering engine for Arizona LiveView applications,
providing a unified interface for rendering stateful components, stateless components,
and lists. It handles different rendering modes and integrates with the template
processing pipeline.

## Features

- **Multi-mode Rendering**: Supports render, diff, and hierarchical rendering modes
- **Component Types**: Handles stateful, stateless, and list component rendering
- **Template Processing**: Runtime template parsing and compilation integration
- **Layout Support**: Provides layout injection and slot rendering capabilities
- **Type Conversion**: Safe conversion of Erlang terms to HTML-safe iodata
- **Live Rendering**: Specialized rendering for arizona_live processes
- **Slot Management**: Comprehensive slot rendering with fallback support

## Key Functions

- `render_stateful/2`: Render stateful components with mode-specific optimization
- `render_stateless/2`: Render stateless components for lightweight templates
- `render_list/3`: Render lists with item functions or template data
- `render_live/2`: Render live content with optional layout injection
- `to_html/2`: Convert any Erlang term to HTML-safe iodata
- `render_slot/2`, `render_slot/3`: Render template slots with fallback support

## Rendering Modes

- **render**: Standard HTML output generation
- **diff**: Differential rendering for change detection and efficient updates
- **hierarchical**: Structured data generation for real-time WebSocket updates

## Template Types

- **Stateful Templates**: Component templates with state management and diffing
- **Stateless Templates**: Lightweight templates without state tracking
- **List Templates**: Optimized templates for rendering collections of items

The module provides runtime template compilation for dynamic content while maintaining
compatibility with compile-time optimized templates from the parse transform.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([render_list/3]).
-export([render_live/2]).
-export([to_html/2]).
-export([render_slot/2]).
-export([render_slot/3]).
-export([extract_list_item_parameter_name/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([render_stateful/2]).
-ignore_xref([render_stateless/2]).
-ignore_xref([render_list/3]).
-ignore_xref([render_live/2]).
-ignore_xref([render_slot/2]).
-ignore_xref([render_slot/3]).
-ignore_xref([extract_list_item_parameter_name/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([html/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
HTML content representation as iodata.

Can be binary strings, lists of binaries, or nested iodata structures.
Used throughout Arizona for efficient HTML generation and manipulation.
""".
-type html() :: iodata().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Render stateful component templates with mode-specific optimization.

Processes stateful component template data or HTML content, applying the appropriate
rendering strategy based on the socket's current mode. Supports render mode for
HTML generation, diff mode for change detection, and hierarchical mode for structured
data generation.

## Examples

```erlang
1> TemplateData = #{elems_order => [0, 1], elems => #{0 => {static, 1, ~"Hello"}}}.
#{...}
2> Socket = arizona_socket:new(#{mode => render}).
#socket{...}
3> arizona_html:render_stateful(TemplateData, Socket).
#socket{html_acc = [~"Hello"], ...}
```
""".
-spec render_stateful(Template, Socket) -> Socket1 when
    Template :: arizona_renderer:stateful_template_data() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateful(TemplateData, Socket) when is_map(TemplateData) ->
    case arizona_socket:get_mode(Socket) of
        render ->
            {_Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),
            UpdatedSocket;
        diff ->
            % Get current stateful state for diffing
            CurrentState = arizona_socket:get_current_stateful_state(Socket),
            arizona_differ:diff_stateful(TemplateData, CurrentState, Socket);
        hierarchical ->
            % Generate hierarchical structure
            {_ComponentStructure, UpdatedSocket} = arizona_hierarchical:stateful_structure(
                TemplateData, Socket
            ),
            UpdatedSocket
    end;
render_stateful(Html, Socket) ->
    render_stateful_html(Html, #{}, Socket).

-doc ~"""
Render stateless component templates for lightweight processing.

Processes stateless template data or HTML content optimized for components without
state management. Supports hierarchical mode for structured data generation and
standard rendering for HTML output.

## Examples

```erlang
1> TemplateData = #{
    elems_order => [0, 1],
    elems => #{0 => {static, 1, ~"Hello"}, 1 => {static, 2, ~"World"}},
    vars_indexes => #{}
}.
#{...}
2> Socket = arizona_socket:new(#{mode => render}).
#socket{...}
3> arizona_html:render_stateless(TemplateData, Socket).
#socket{html_acc = [~"Hello", ~"World"], ...}
```
""".
-spec render_stateless(Template, Socket) -> Socket1 when
    Template :: arizona_renderer:stateless_template_data() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless(TemplateData, Socket) when is_map(TemplateData) ->
    case arizona_socket:get_mode(Socket) of
        render ->
            {_Html, UpdatedSocket} = arizona_renderer:render_stateless(TemplateData, Socket),
            UpdatedSocket;
        diff ->
            % In diff mode, use stateless diffing for hierarchical updates
            CurrentState = arizona_socket:get_current_stateful_state(Socket),
            arizona_differ:diff_stateless(TemplateData, CurrentState, Socket);
        hierarchical ->
            % Generate stateless hierarchical structure
            {_ComponentStructure, UpdatedSocket} = arizona_hierarchical:stateless_structure(
                TemplateData, Socket
            ),
            UpdatedSocket
    end;
render_stateless(Html, Socket) ->
    render_stateless_html(Html, #{}, Socket).

-doc ~"""
Render list templates with item functions or optimized template data.

Processes list template data for efficient rendering of collections. Supports both
optimized template data from parse transform and runtime item functions with automatic
parameter name extraction.

## Examples

```erlang
1> ListData = #{static => [~"<li>"], dynamic => #{...}}.
#{...}
2> Items = [item1, item2, item3].
[...]
3> arizona_html:render_list(ListData, Items, Socket).
#socket{html_acc = [~"<li>item1</li>", ~"<li>item2</li>", ...], ...}
4> ItemFun = fun(I) -> <<"<p>", I/binary, "</p>">> end.
#Fun<...>
5> arizona_html:render_list(ItemFun, [~"test"], Socket).
#socket{html_acc = [~"<p>test</p>"], ...}
```
""".
-spec render_list(Template, [Item], Socket) -> Socket1 when
    Template :: fun((Item) -> html()) | arizona_renderer:list_template_data(),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_list(ListData, Items, Socket) when
    is_map(ListData), is_list(Items)
->
    case arizona_socket:get_mode(Socket) of
        hierarchical ->
            {_ListElement, UpdatedSocket} = arizona_hierarchical:list_structure(
                ListData, Items, Socket
            ),
            UpdatedSocket;
        _ ->
            {_Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),
            UpdatedSocket
    end;
render_list(ItemFun, Items, Socket) when
    is_function(ItemFun, 1), is_list(Items)
->
    ListItemParameterName = extract_list_item_parameter_name(ItemFun),
    %% Render each list item and accumulate the resulting HTML
    {AccumulatedHtml, FinalSocket} = lists:foldl(
        fun(CurrentItem, {HtmlAccumulator, CurrentSocket}) ->
            %% Generate template HTML by calling the item function
            ItemTemplateHtml = arizona_list:call_item_function(ItemFun, CurrentItem),

            %% Render the item template with the current item bound to the extracted parameter name
            %% This allows templates like ~"""<li>{I}</li>""" to access the current item as 'I'
            ItemSocket = render_stateless_html(
                ItemTemplateHtml,
                #{ListItemParameterName => CurrentItem},
                CurrentSocket
            ),

            %% Extract rendered HTML and accumulate it
            RenderedItemHtml = arizona_socket:get_html(ItemSocket),
            {[HtmlAccumulator, RenderedItemHtml], ItemSocket}
        end,
        {[], Socket},
        Items
    ),

    %% Return socket with all rendered list items
    arizona_socket:set_html_acc(AccumulatedHtml, FinalSocket).

-doc ~"""
Render live content with optional layout injection.

Specialized rendering function for arizona_live processes that supports layout
integration. When a layout is configured, injects the live content into the
specified slot, otherwise renders the content directly.

## Examples

```erlang
1> Template = #{elems_order => [0], elems => #{0 => {static, 1, ~"Content"}}}.
#{...}
2> Socket = arizona_socket:new(#{mode => render}).
#socket{...}
3> arizona_html:render_live(Template, Socket).
#socket{html_acc = [~"Content"], ...}
```
""".
-spec render_live(Template, Socket) -> Socket1 when
    Template :: arizona_renderer:stateful_template_data() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_live(Template, Socket) ->
    % Check if layout is configured
    case arizona_socket:get_layout(Socket) of
        undefined ->
            % No layout, return live content directly
            render_stateful(Template, Socket);
        {LayoutModule, LayoutRenderFun, SlotName} ->
            % Layout configured, inject live content into layout
            case arizona_socket:get_mode(Socket) of
                render ->
                    % Put the live content as the specified slot binding
                    SocketWithSlot = arizona_socket:put_bindings(
                        #{
                            SlotName => {stateful, Template}
                        },
                        Socket
                    ),

                    % Render the layout with the live content injected
                    arizona_stateless:call_render_callback(
                        LayoutModule, LayoutRenderFun, SocketWithSlot
                    );
                diff ->
                    % When diffing, we only return the live content changes
                    render_stateful(Template, Socket);
                hierarchical ->
                    % When in hierarchical mode, accumulate structure without layout processing
                    render_stateful(Template, Socket)
            end
    end.

-doc ~"""
Convert any Erlang term to HTML-safe iodata.

Safely converts various Erlang terms to HTML-compatible iodata, handling
binaries, lists, atoms, numbers, and complex terms. Provides special handling
for socket returns from nested template calls.

## Examples

```erlang
1> arizona_html:to_html(~"Hello", Socket).
{~"Hello", Socket}
2> arizona_html:to_html(42, Socket).
{~"42", Socket}
3> arizona_html:to_html([~"Hello", ~" ", ~"World"], Socket).
{[~"Hello", ~" ", ~"World"], Socket}
```
""".
-spec to_html(Value, Socket) -> {Html, Socket1} when
    Value :: term(),
    Socket :: arizona_socket:socket(),
    Html :: html(),
    Socket1 :: arizona_socket:socket().
to_html(Value, Socket) when is_binary(Value) ->
    {Value, Socket};
to_html(Value, Socket) when is_list(Value) ->
    lists:foldl(
        fun(Item, {HtmlAcc, AccSocket}) ->
            {HtmlAcc1, AccSocket1} = to_html(Item, AccSocket),
            case HtmlAcc of
                [] -> {HtmlAcc1, AccSocket1};
                _ -> {[HtmlAcc, HtmlAcc1], AccSocket1}
            end
        end,
        {[], Socket},
        Value
    );
to_html(Value, Socket) when is_atom(Value) ->
    {atom_to_binary(Value, utf8), Socket};
to_html(Value, Socket) when is_integer(Value) ->
    {integer_to_binary(Value), Socket};
to_html(Value, Socket) when is_float(Value) ->
    {list_to_binary(io_lib:format("~p", [Value])), Socket};
to_html(Value, Socket) ->
    case arizona_socket:is_socket(Value) of
        true ->
            % Function returned a socket (from arizona_html calls)
            Html = arizona_socket:get_html(Value),
            {Html, Value};
        false ->
            Html = list_to_binary(io_lib:format("~tp", [Value])),
            {Html, Socket}
    end.

-doc ~"""
Render a required slot from component bindings.

Retrieves and renders slot content from the socket's bindings, throwing an error
if the slot is not found. Use render_slot/3 for optional slots with fallbacks.

## Examples

```erlang
1> Socket = arizona_socket:put_binding(content, ~"Hello World", Socket).
#socket{...}
2> arizona_html:render_slot(content, Socket).
#socket{html_acc = [~"Hello World"], ...}
```
""".
-spec render_slot(SlotName, Socket) -> Socket1 when
    SlotName :: atom(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_slot(SlotName, Socket) ->
    SlotContent = arizona_socket:get_binding(SlotName, Socket),
    render_slot_content_smart(SlotContent, Socket).

-doc ~"""
Render an optional slot from component bindings with fallback.

Retrieves and renders slot content from the socket's bindings, using the default
value if the slot is not found. Provides safe slot rendering for optional content.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_html:render_slot(missing_slot, Socket, ~"Default content").
#socket{html_acc = [~"Default content"], ...}
```
""".
-spec render_slot(SlotName, Socket, Default) -> Socket1 when
    SlotName :: atom(),
    Socket :: arizona_socket:socket(),
    Default :: term(),
    Socket1 :: arizona_socket:socket().
render_slot(SlotName, Socket, Default) ->
    SlotContent = arizona_socket:get_binding(SlotName, Socket, Default),
    render_slot_content_smart(SlotContent, Socket).

-doc ~"""
Extract parameter variable name from a list item function for template binding.

Analyzes a list item function's AST to determine the parameter name used,
enabling proper variable binding in template rendering. Requires debug_info
to be enabled during compilation for AST extraction.

## Examples

```erlang
1> ItemFun = fun(I) -> <<"<li>", I/binary, "</li>">> end.
#Fun<...>
2> arizona_html:extract_list_item_parameter_name(ItemFun).
'I'
3> ItemFun2 = fun(Item) -> <<"<p>", Item/binary, "</p>">> end.
#Fun<...>
4> arizona_html:extract_list_item_parameter_name(ItemFun2).
'Item'
```
""".
-spec extract_list_item_parameter_name(ListItemFunction) -> ParameterName when
    ListItemFunction :: fun((term()) -> term()),
    ParameterName :: atom().
extract_list_item_parameter_name(ListItemFunction) when is_function(ListItemFunction, 1) ->
    case erlang:fun_info(ListItemFunction, env) of
        {env, []} ->
            %% No debug_info available - use conventional parameter name
            %% This fallback ensures compatibility when modules are compiled without debug_info
            'Item';
        {env, [FunctionEnvironment | _]} ->
            %% Extract parameter name from function's AST clause
            {_, _, _, _, _, [FirstClause | _]} = FunctionEnvironment,
            {clause, _Line, [FirstParameter | _], _Guards, _Body} = FirstClause,
            {var, _VarLine, ParameterName} = FirstParameter,
            ParameterName
    end.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Render stateful HTML with runtime template processing
-spec render_stateful_html(Html, Bindings, Socket) -> Socket1 when
    Html :: html(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateful_html(Html, Bindings, Socket) when
    (is_binary(Html) orelse is_list(Html)), is_map(Bindings)
->
    %% Parse template at runtime
    Tokens = arizona_scanner:scan(#{}, Html),
    ParsedResult = arizona_parser:parse_stateful_tokens(Tokens),

    %% Transform to optimized format using same logic as parse transform
    OptimizedAST = arizona_parse_transform:transform_stateful_to_ast(ParsedResult),

    %% Evaluate AST to get optimized template data with Socket binding
    {value, OptimizedTemplateData, _NewBindings} = erl_eval:expr(
        erl_syntax:revert(OptimizedAST),
        Bindings
    ),

    %% Render using optimized data (same as compile-time path)
    render_stateful(OptimizedTemplateData, Socket).

%% Render stateless HTML with runtime template processing
-spec render_stateless_html(Html, Bindings, Socket) -> Socket1 when
    Html :: html(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless_html(Html, Bindings, Socket) when
    (is_binary(Html) orelse is_list(Html)), is_map(Bindings)
->
    %% Parse template at runtime
    Tokens = arizona_scanner:scan(#{}, Html),
    ParsedResult = arizona_parser:parse_stateless_tokens(Tokens),

    %% Transform to optimized format using same logic as parse transform
    OptimizedAST = arizona_parse_transform:transform_stateless_to_ast(ParsedResult),

    %% Evaluate AST to get optimized template data with Socket binding
    {value, OptimizedStructuredList, _NewBindings} = erl_eval:expr(
        erl_syntax:revert(OptimizedAST),
        Bindings
    ),

    %% Render using optimized data (same as compile-time path)
    render_stateless(OptimizedStructuredList, Socket).

%% Smart slot content renderer - handles both single and list slots
-spec render_slot_content_smart(Content, Socket) -> Socket1 when
    Content :: term(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_slot_content_smart(Content, Socket) when is_list(Content) ->
    % List of slots - render each item, threading socket through
    lists:foldl(
        fun(Item, SocketAcc) ->
            render_slot_content(Item, SocketAcc)
        end,
        Socket,
        Content
    );
render_slot_content_smart(Content, Socket) ->
    render_slot_content(Content, Socket).

%% Render individual slot content based on type
-spec render_slot_content(SlotContent, Socket) -> Socket1 when
    SlotContent :: term(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_slot_content(Html, Socket) when is_binary(Html) ->
    CurrentHtml = arizona_socket:get_html(Socket),
    arizona_socket:set_html_acc([CurrentHtml, Html], Socket);
render_slot_content({stateless, Template}, Socket) ->
    render_stateless(Template, Socket);
render_slot_content({stateless, Mod, Fun, Bindings}, Socket) ->
    SocketWithTempBindings = arizona_socket:with_temp_bindings(Bindings, Socket),
    arizona_stateless:call_render_callback(Mod, Fun, SocketWithTempBindings);
render_slot_content({stateful, Template}, Socket) ->
    render_stateful(Template, Socket);
render_slot_content({stateful, Module, Bindings}, Socket) ->
    SocketWithBindings = arizona_socket:put_bindings(Bindings, Socket),
    arizona_stateful:call_render_callback(Module, SocketWithBindings).
