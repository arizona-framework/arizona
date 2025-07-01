-module(arizona_html).

-export([render_stateful/2]).
-export([render_stateless/2]).

render_stateful(Template, Socket) when is_binary(Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    TemplateData = arizona_parser:parse_stateful_tokens(Tokens),
    arizona_renderer:render_stateful(TemplateData, Socket).

render_stateless(Template, Socket) when is_binary(Template) ->
    Tokens = arizona_scanner:scan(#{}, Template),
    StructuredList = arizona_parser:parse_stateless_tokens(Tokens),
    arizona_renderer:render_stateless(StructuredList, Socket).

