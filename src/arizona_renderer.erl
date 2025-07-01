-module(arizona_renderer).

-export([render_stateful/2]).
-export([render_stateless/2]).
-export([format_error/2]).

%% Render structured template data (from parse transform or parser)
-spec render_stateful(TemplateData, Socket) -> {Html, Socket1} when
    TemplateData :: arizona_parser:stateful_result(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateful(#{elems_order := Order, elems := Elements}, Socket) ->
    {Html, UpdatedSocket} = render_elements(Order, Elements, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

%% Render stateless structured list
-spec render_stateless(StructuredList, Socket) -> {Html, Socket1} when
    StructuredList :: arizona_parser:stateless_result(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {Html, UpdatedSocket} = render_iolist(StructuredList, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

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
render_element({static, _Line, Content}, Socket) when is_binary(Content) ->
    {Content, Socket};
render_element({dynamic, Line, Fun}, Socket) when is_function(Fun, 1) ->
    try
        Result = Fun(Socket),
        {iolist_to_binary(Result), Socket}
    catch
        throw:{binding_not_found, Key} ->
            error({binding_not_found, Key}, none, binding_error_info(Line, Key, Socket))
    end;
render_element({dynamic, _Line, Content}, Socket) when is_binary(Content) ->
    %% For simple dynamic content (like from parse transform)
    {Content, Socket}.

%% Error info for binding errors following OTP pattern
binding_error_info(Line, Key, Socket) ->
    CurrentState = arizona_socket:get_current_stateful_state(Socket),
    TemplateModule = arizona_stateful:get_module(CurrentState),
    [{error_info, #{cause => #{binding => Key, line => Line, template_module => TemplateModule},
                    module => arizona_renderer}}].

%% OTP error_info callback for enhanced error formatting
-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: term(),
    StackTrace :: [term()],
    ErrorMap :: #{atom() => term()}.
format_error(Reason, [{_M,_F,_As,Info}|_]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    CauseMap = maps:get(cause, ErrorInfo, #{}),
    CauseMap#{general => "Template rendering error",
              reason => io_lib:format("arizona_renderer: ~p", [Reason])};
format_error(Reason, _StackTrace) ->
    #{general => "Template rendering error",
      reason => io_lib:format("arizona_renderer: ~p", [Reason])}.
