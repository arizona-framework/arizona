-module(arizona_html).

-export([render_stateful/2]).
-export([render_stateless/2]).

-spec render_stateful(Html, Socket) -> Socket1 when
    Html :: arizona_parser:stateful_result() | binary(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateful(TemplateData, Socket) when is_map(TemplateData) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),
    UpdatedSocket;
render_stateful(Template, Socket) when is_binary(Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    TemplateData = arizona_parser:parse_stateful_tokens(Tokens),
    render_stateful(TemplateData, Socket).

-spec render_stateless(Html, Socket) -> Socket1 when
    Html :: arizona_parser:stateless_result() | binary(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),
    UpdatedSocket;
render_stateless(Template, Socket) when is_binary(Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    StructuredList = arizona_parser:parse_stateless_tokens(Tokens),
    render_stateless(StructuredList, Socket).
