-module(arizona_renderer).

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([render_list/3]).
-export([render_element/2]).
-export([format_error/2]).

-doc ~"Template data from parse transform with optimized functions for stateful rendering.".
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
-export_type([stateful_template_data/0]).

-doc ~"Template data from parse transform with optimized functions for stateless rendering.".
-type stateless_template_data() :: [
    {static, pos_integer(), binary()}
    | {dynamic, pos_integer(), fun((arizona_socket:socket()) -> term())}
].
-export_type([stateless_template_data/0]).

-doc ~"Template data from parse transform with optimized functions for list rendering.".
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
-export_type([list_template_data/0]).

%% Render structured template data (from parse transform)
-spec render_stateful(TemplateData, Socket) -> {Html, Socket1} when
    TemplateData :: stateful_template_data(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateful(#{elems_order := Order, elems := Elements}, Socket) ->
    {Html, UpdatedSocket} = render_elements(Order, Elements, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

%% Render stateless structured list
-spec render_stateless(StructuredList, Socket) -> {Html, Socket1} when
    StructuredList :: stateless_template_data(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {Html, UpdatedSocket} = render_iolist(StructuredList, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

%% Render list using parsed list template structure
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

%% Render individual elements
-spec render_element(Element, Socket) -> {Html, Socket1} when
    Element ::
        {static, pos_integer(), binary()}
        | {dynamic, pos_integer(), fun((arizona_socket:socket()) -> term())},
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
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
    %% Evaluate current element function - let it crash if index doesn't exist
    {dynamic, Line, Fun} = maps:get(ElemIndex, ElemsFuns),
    {Value, UpdatedSocket} =
        try
            Result = arizona_list:call_element_function(Fun, Item, Socket),
            arizona_html:to_html(Result, Socket)
        catch
            throw:{binding_not_found, Key} ->
                error({binding_not_found, Key}, none, binding_error_info(Line, Key, Socket));
            _:Error ->
                error({list_item_render_error, Error, Item, Line})
        end,

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

%% OTP error_info callback for enhanced error formatting
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
