-module(arizona_template).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([from_string/2]).
-export([from_string/4]).
-export([static/1]).
-export([dynamic/1]).
-export([dynamic_sequence/1]).
-export([dynamic_anno/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([find_binding/2]).
-export([render_stateful/2]).
-export([render_stateless/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: [StaticContent :: binary()],
    dynamic :: tuple(),
    dynamic_sequence :: [pos_integer()],
    dynamic_anno :: tuple()
}).

-opaque template() :: #template{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~""""
Create template from string content.

Processes template string through scanner and parser pipeline to create
an optimized template structure ready for rendering. Handles static content,
dynamic expressions, and comments.

## Example

```erlang
Template = arizona_template:from_string(~"""
<div class="user">
    Hello {Username}!
</div>
"""),
Static = arizona_template:static(Template),
Dynamic = arizona_template:dynamic(Template).
```
"""".
-spec from_string(TemplateContent, Bindings) -> template() when
    TemplateContent :: binary(),
    Bindings :: map().
from_string(TemplateContent, Bindings) ->
    from_string(erlang, 1, TemplateContent, Bindings).

-doc ~"""
Create template from string with module context.

Internal function used by parse transforms and compile-time processing.
Provides full control over evaluation context including module name,
line number, and bindings.
""".
-spec from_string(Module, Line, TemplateContent, Bindings) -> template() when
    Module :: atom(),
    Line :: pos_integer(),
    TemplateContent :: binary(),
    Bindings :: map().
from_string(Module, Line, TemplateContent, Bindings) when
    is_atom(Module), is_integer(Line), is_binary(TemplateContent), is_map(Bindings)
->
    % Scan template content into tokens
    Tokens = arizona_scanner:scan(#{line => Line}, TemplateContent),

    % Parse tokens into AST
    AST = arizona_parser:parse_tokens(Tokens),

    % Evaluate AST to get template record
    erl_eval:expr(
        erl_syntax:revert(AST),
        Bindings#{'Bindings' => Bindings},
        {value, fun(Function, Args) ->
            apply(Module, Function, Args)
        end},
        none,
        value
    ).

-spec static(template()) -> [binary()].
static(#template{static = Static}) ->
    Static.

-spec dynamic(template()) -> tuple().
dynamic(#template{dynamic = Dynamic}) ->
    Dynamic.

-spec dynamic_sequence(template()) -> [pos_integer()].
dynamic_sequence(#template{dynamic_sequence = Sequence}) ->
    Sequence.

-spec dynamic_anno(template()) -> tuple().
dynamic_anno(#template{dynamic_anno = Anno}) ->
    Anno.

-spec get_binding(Key, Bindings) -> Value when
    Key :: atom(),
    Bindings :: map(),
    Value :: dynamic().
get_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    % Send cast to self (the live process)
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:get(Key, Bindings).

-spec get_binding(Key, Bindings, Default) -> Value when
    Key :: atom(),
    Bindings :: map(),
    Default :: fun(() -> Value),
    Value :: dynamic().
get_binding(Key, Bindings, Default) ->
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:get(Key, Bindings, Default).

-spec find_binding(Key, Bindings) -> {ok, Value} | error when
    Key :: atom(),
    Bindings :: map(),
    Value :: dynamic().
find_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    % Send cast to self (the live process)
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:find(Key, Bindings).

-spec render_stateful(Module, Bindings) -> Callback when
    Module :: atom(),
    Bindings :: map(),
    Callback :: fun((arizona_socket:socket()) -> {iodata() | term(), arizona_socket:socket()}).
render_stateful(Mod, Bindings) ->
    fun(Socket) ->
        case arizona_socket:get_mode(Socket) of
            render ->
                arizona_template_renderer:render_stateful(Mod, Bindings, Socket);
            diff ->
                arizona_template_differ:diff_stateful(Mod, Bindings, Socket);
            hierarchical ->
                arizona_template_hierarchical:hierarchical_stateful(Mod, Bindings, Socket)
        end
    end.

-spec render_stateless(Module, Function, Bindings) -> Callback when
    Module :: atom(),
    Function :: atom(),
    Bindings :: map(),
    Callback :: fun((arizona_socket:socket()) -> {iodata() | term(), arizona_socket:socket()}).
render_stateless(Mod, Fun, Bindings) ->
    fun(Socket) ->
        case arizona_socket:get_mode(Socket) of
            render ->
                arizona_template_renderer:render_stateless(Mod, Fun, Bindings, Socket);
            diff ->
                arizona_template_differ:diff_stateless(Mod, Fun, Bindings, Socket);
            hierarchical ->
                arizona_template_hierarchical:hierarchical_stateless(Mod, Fun, Bindings, Socket)
        end
    end.
