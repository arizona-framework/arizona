-module(arizona_renderer).

-export([render_stateful/2]).
-export([render_stateless/2]).

%% Render structured template data (from parse transform or parser)
render_stateful(#{elems_order := Order, elems := Elements}, Socket) ->
    {Html, UpdatedSocket} = render_elements(Order, Elements, Socket, []),
    UpdatedSocket1 = arizona_socket:set_html_acc(Html, UpdatedSocket),
    {Html, UpdatedSocket1}.

%% Render stateless structured list
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
render_element({dynamic, _Line, Fun}, Socket) when is_function(Fun, 1) ->
    Result = Fun(Socket),
    {iolist_to_binary(Result), Socket};
render_element({dynamic, _Line, Content}, Socket) when is_binary(Content) ->
    %% For simple dynamic content (like from parse transform)
    {Content, Socket}.
