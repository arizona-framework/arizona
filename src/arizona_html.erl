-module(arizona_html).

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([render_list/4]).
-export([to_html/1]).

%% Types
-type html() :: iodata().
-export_type([html/0]).

-spec render_stateful(Template, Socket) -> Socket1 when
    Template :: arizona_renderer:stateful_template_data() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateful(TemplateData, Socket) when is_map(TemplateData) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),
    UpdatedSocket;
render_stateful(Html, Socket) when is_binary(Html); is_list(Html) ->
    %% Parse template at runtime
    Tokens = arizona_scanner:scan(#{}, Html),
    ParsedResult = arizona_parser:parse_stateful_tokens(Tokens),
    
    %% Transform to optimized format using same logic as parse transform
    OptimizedAST = arizona_parse_transform:transform_stateful_to_ast(ParsedResult),
    
    %% Evaluate AST to get optimized template data with Socket binding
    {value, OptimizedTemplateData, _NewBindings} = erl_eval:expr(
        erl_syntax:revert(OptimizedAST), 
        #{'Socket' => Socket}
    ),
    
    %% Render using optimized data (same as compile-time path)
    render_stateful(OptimizedTemplateData, Socket).

-spec render_stateless(Template, Socket) -> Socket1 when
    Template :: arizona_parser:stateless_result() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),
    UpdatedSocket;
render_stateless(Html, Socket) when is_binary(Html); is_list(Html) ->
    %% Parse template at runtime
    Tokens = arizona_scanner:scan(#{}, Html),
    ParsedResult = arizona_parser:parse_stateless_tokens(Tokens),
    
    %% Transform to optimized format using same logic as parse transform
    OptimizedAST = arizona_parse_transform:transform_stateless_to_ast(ParsedResult),
    
    %% Evaluate AST to get optimized template data with Socket binding
    {value, OptimizedStructuredList, _NewBindings} = erl_eval:expr(
        erl_syntax:revert(OptimizedAST), 
        #{'Socket' => Socket}
    ),
    
    %% Render using optimized data (same as compile-time path)
    render_stateless(OptimizedStructuredList, Socket).

-spec render_list(Template, [Item], KeyFun, Socket) -> Socket1 when
    Template :: fun((Item) -> html()) | arizona_parser:list_result(),
    Item :: term(),
    KeyFun :: fun((Item) -> term()),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_list(ListData, Items, KeyFun, Socket) when
    is_map(ListData), is_list(Items), is_function(KeyFun, 1)
->
    {_Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, KeyFun, Socket),
    UpdatedSocket;
render_list(ItemFun, Items, KeyFun, Socket) when
    is_function(ItemFun, 1), is_list(Items), is_function(KeyFun, 1)
->
    %% Accumulate HTML from all items
    {AllHtml, FinalSocket} = lists:foldl(
        fun(Item, {HtmlAcc, AccSocket}) ->
            %% Call item function to get template HTML
            ItemHtml = to_html(arizona_list:call_item_function(ItemFun, Item)),
            {[HtmlAcc, ItemHtml], AccSocket}
        end,
        {[], Socket},
        Items
    ),

    %% Set final accumulated HTML
    arizona_socket:set_html_acc(AllHtml, FinalSocket).

%% Convert any value to HTML-safe iodata
-spec to_html(term()) -> html().
to_html(Value) when is_binary(Value) -> Value;
to_html(Value) when is_list(Value) ->
    try iolist_to_binary(Value) of
        Binary -> Binary
    catch
        _:_ -> iolist_to_binary([to_html(Term) || Term <- Value])
    end;
to_html(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_html(Value) when is_integer(Value) -> integer_to_binary(Value);
to_html(Value) when is_float(Value) -> list_to_binary(io_lib:format("~p", [Value]));
to_html(Value) ->
    list_to_binary(io_lib:format("~tp", [Value])).
