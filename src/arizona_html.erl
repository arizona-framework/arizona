-module(arizona_html).

-export([render_stateful/2]).
-export([render_stateless/2]).

%% Types
-type html() :: iodata().
-export_type([html/0]).

-spec render_stateful(Template, Socket) -> Socket1 when
    Template :: arizona_parser:stateful_result() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateful(TemplateData, Socket) when is_map(TemplateData) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),
    UpdatedSocket;
render_stateful(Html, Socket) when is_binary(Html); is_list(Html) ->
    Tokens = arizona_scanner:scan(#{}, Html),
    TemplateData = arizona_parser:parse_stateful_tokens(Tokens),
    render_stateful(TemplateData, Socket).

-spec render_stateless(Template, Socket) -> Socket1 when
    Template :: arizona_parser:stateless_result() | html(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
render_stateless(StructuredList, Socket) when is_list(StructuredList) ->
    {_Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),
    UpdatedSocket;
render_stateless(Html, Socket) when is_binary(Html); is_list(Html) ->
    Tokens = arizona_scanner:scan(#{}, Html),
    StructuredList = arizona_parser:parse_stateless_tokens(Tokens),
    render_stateless(StructuredList, Socket).
