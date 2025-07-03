-module(arizona_html).

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([render_list/3]).
-export([to_html/2]).
-export([render_slot/2, render_slot/3]).

%% Types
-type html() :: iodata().
-export_type([html/0]).

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
            arizona_differ:diff_stateful(TemplateData, CurrentState, Socket)
    end;
render_stateful(Html, Socket) ->
    render_stateful_html(Html, #{}, Socket).

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

-spec render_stateless(Template, Socket) -> Socket1 when
    Template :: arizona_renderer:stateless_template_data() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),
    UpdatedSocket;
render_stateless(Html, Socket) ->
    render_stateless_html(Html, #{}, Socket).

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

-spec render_list(Template, [Item], Socket) -> Socket1 when
    Template :: fun((Item) -> html()) | arizona_renderer:list_template_data(),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_list(ListData, Items, Socket) when
    is_map(ListData), is_list(Items)
->
    {_Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),
    UpdatedSocket;
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

%% Convert any value to HTML-safe iodata
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
            {[HtmlAcc, HtmlAcc1], AccSocket1}
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

%% Extract the parameter variable name from a list item function's AST
%% This is used to properly bind list items in template rendering with correct variable names
%%
%% Example: fun(I) -> template end -> returns 'I'
%%          fun(Item) -> template end -> returns 'Item'
%%
%% Note: Requires debug_info to be enabled during compilation for AST extraction
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
%% Slot rendering functions
%% --------------------------------------------------------------------

%% Render a required slot from bindings
-spec render_slot(SlotName, Socket) -> Socket1 when
    SlotName :: atom(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_slot(SlotName, Socket) ->
    SlotContent = arizona_socket:get_binding(SlotName, Socket),
    render_slot_content_smart(SlotContent, Socket).

%% Render an optional slot from bindings with fallback
-spec render_slot(SlotName, Socket, Default) -> Socket1 when
    SlotName :: atom(),
    Socket :: arizona_socket:socket(),
    Default :: term(),
    Socket1 :: arizona_socket:socket().
render_slot(SlotName, Socket, Default) ->
    SlotContent = arizona_socket:get_binding(SlotName, Socket, Default),
    render_slot_content_smart(SlotContent, Socket).

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
render_slot_content({stateless, ParsedTemplate}, Socket) when is_list(ParsedTemplate) ->
    % Handle parse-transform optimized stateless template
    arizona_html:render_stateless(ParsedTemplate, Socket);
render_slot_content({stateless, Mod, Fun, Bindings}, Socket) ->
    SocketWithTempBindings = arizona_socket:with_temp_bindings(Bindings, Socket),
    arizona_stateless:call_render_callback(Mod, Fun, SocketWithTempBindings);
render_slot_content({stateful, Module, Bindings}, Socket) ->
    SocketWithBindings = arizona_socket:put_bindings(Bindings, Socket),
    arizona_stateful:call_render_callback(Module, SocketWithBindings).
