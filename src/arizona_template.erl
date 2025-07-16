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
    maps:get(Key, Bindings).

-spec render_stateful(Module, Bindings) -> {StatefulTemplate, Template} when
    Module :: atom(),
    Bindings :: map(),
    StatefulTemplate :: '$arizona_stateful_template',
    Template :: template().
render_stateful(Mod, Bindings) ->
    fun(Socket) -> call_stateful(Mod, Bindings, Socket) end.

-spec call_stateful(Mod, Bindings, Socket) -> {Template, Socket1} when
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Template :: arizona_template:template(),
    Socket1 :: arizona_socket:socket().
call_stateful(Mod, Bindings, Socket) ->
    Id = maps:get(id, Bindings),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            %% Apply new bindings to existing state before checking remount
            UpdatedState = arizona_stateful:put_bindings(Bindings, State),
            %% Update socket with new state and call render callback (which handles diffing)
            Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
            UpdatedBindings = arizona_stateful:get_bindings(UpdatedState),
            Template = arizona_stateful:call_render_callback(Mod, UpdatedBindings),
            {Template, Socket1};
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(State, Socket),
            %% Call mount callback for new components
            Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
            %% Call the component's render callback which handles
            %% rendering and returns updated socket
            MountedState = arizona_socket:get_stateful_state(Id, Socket2),
            MountedBindings = arizona_stateful:get_bindings(MountedState),
            Template = arizona_stateful:call_render_callback(Mod, MountedBindings),
            {Template, Socket2}
    end.

-spec render_stateless(Module, Function, Bindings) -> {StatelessTemplate, Template, Bindings} when
    Module :: atom(),
    Function :: atom(),
    Bindings :: map(),
    StatelessTemplate :: '$arizona_stateless_template',
    Template :: template().
render_stateless(Mod, Fun, Bindings) ->
    fun(Socket) -> call_stateless(Mod, Fun, Bindings, Socket) end.

call_stateless(Mod, Fun, Bindings, Socket) ->
    Template = arizona_stateless:call_render_callback(Mod, Fun, Bindings),
    TempSocket = arizona_socket:with_temp_bindings(Bindings, Socket),
    {Template, TempSocket}.
