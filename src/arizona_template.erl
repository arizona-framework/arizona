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
    % Record variable dependency for runtime tracking
    % Send cast to self (the live process)
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    maps:get(Key, Bindings).

-spec render_stateful(Module, Bindings) -> Callback when
    Module :: atom(),
    Bindings :: map(),
    Callback :: fun((arizona_socket:socket()) -> {iodata() | term(), arizona_socket:socket()}).
render_stateful(Mod, Bindings) ->
    fun(Socket) ->
        case arizona_socket:get_mode(Socket) of
            render ->
                render_stateful(Mod, Bindings, Socket);
            diff ->
                diff_stateful(Mod, Bindings, Socket);
            hierarchical ->
                hierarchical_stateful(Mod, Bindings, Socket)
        end
    end.

prepare_stateful_render(Mod, Bindings, Socket) ->
    Id = maps:get(id, Bindings),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            %% Apply new bindings to existing state before checking remount
            UpdatedState = arizona_stateful:put_bindings(Bindings, State),
            %% Update socket with new state and call render callback (which handles diffing)
            Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
            UpdatedBindings = arizona_stateful:get_bindings(UpdatedState),
            Template = arizona_stateful:call_render_callback(Mod, UpdatedBindings),
            {Id, Template, Socket1};
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
            {Id, Template, Socket2}
    end.

render_stateful(Mod, Bindings, Socket) ->
    {Id, Template, Socket1} = prepare_stateful_render(Mod, Bindings, Socket),
    % Notify live process of current stateful component
    ok = arizona_socket:notify_current_stateful_id(Id, Socket1),
    resolve_template(Template, Socket1).

diff_stateful(Mod, Bindings, Socket) ->
    % Get runtime-tracked variable dependencies from live process
    case arizona_socket:get_live_pid(Socket) of
        undefined ->
            % No live process, can't do runtime diffing
            {[], Socket};
        LivePid ->
            {Id, Template, Socket1} = prepare_stateful_render(Mod, Bindings, Socket),
            StatefulState = arizona_socket:get_stateful_state(Id, Socket1),
            ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),

            case has_changes(ChangedBindings) of
                false ->
                    {[], Socket1};
                true ->
                    Dependencies = arizona_live:get_component_dependencies(LivePid, Id),
                    % Find affected elements using runtime dependencies
                    AffectedElements = get_affected_elements(ChangedBindings, Dependencies),
                    % Generate diff for affected elements
                    generate_element_diff(AffectedElements, Template, Socket1)
            end
    end.

%% Check if there are any changes in the bindings
-spec has_changes(ChangedBindings) -> boolean() when
    ChangedBindings :: map().
has_changes(ChangedBindings) ->
    maps:size(ChangedBindings) > 0.

%% Get affected elements from changed bindings and variable dependencies
-spec get_affected_elements(ChangedBindings, Dependencies) -> AffectedElements when
    ChangedBindings :: map(),
    Dependencies :: #{atom() => [non_neg_integer()]},
    AffectedElements :: sets:set(non_neg_integer()).
get_affected_elements(ChangedBindings, Dependencies) ->
    ChangedVarNames = maps:keys(ChangedBindings),
    AffectedIndexLists = [
        maps:get(VarName, Dependencies, [])
     || VarName <- ChangedVarNames
    ],
    sets:from_list(lists:flatten(AffectedIndexLists)).

%% Generate diff for affected elements
-spec generate_element_diff(AffectedElements, Template, Socket) -> {Diff, Socket1} when
    AffectedElements :: sets:set(non_neg_integer()),
    Template :: template(),
    Socket :: term(),
    Diff :: [term()],
    Socket1 :: term().
generate_element_diff(AffectedElements, Template, Socket) ->
    case sets:size(AffectedElements) of
        0 ->
            {[], Socket};
        _ ->
            DynamicSequence = sets:to_list(AffectedElements),
            DynamicTuple = arizona_template:dynamic(Template),
            process_affected_elements(DynamicSequence, DynamicTuple, Socket)
    end.

%% Process affected elements to create diff changes
-spec process_affected_elements(DynamicSequence, DynamicTuple, Socket) ->
    {Changes, Socket1}
when
    DynamicSequence :: [pos_integer()],
    DynamicTuple :: tuple(),
    Socket :: term(),
    Changes :: [term()],
    Socket1 :: term().
process_affected_elements([], _DynamicTuple, Socket) ->
    {[], Socket};
process_affected_elements([ElementIndex | T], DynamicTuple, Socket) ->
    % Notify live process of current element index
    ok = arizona_socket:notify_current_element_index(ElementIndex, Socket),

    DynamicCallback = element(ElementIndex, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {StatefulHtml, StatefulSocket} = Callback(Socket),
            {Html, HtmlSocket} = arizona_html:to_html(StatefulHtml, StatefulSocket),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalSocket} = process_affected_elements(T, DynamicTuple, HtmlSocket),
            {[ElementChange | RestChanges], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalSocket} = process_affected_elements(T, DynamicTuple, NewSocket),
            {[ElementChange | RestChanges], FinalSocket}
    end.

hierarchical_stateful(Mod, Bindings, Socket) ->
    {Id, Template, RenderSocket} = prepare_stateful_render(Mod, Bindings, Socket),
    % Notify live process of current stateful component
    ok = arizona_socket:notify_current_stateful_id(Id, RenderSocket),
    {Dynamic, DynamicSocket} = resolve_template_dynamic(Template, RenderSocket),
    HierarchicalSocket = arizona_socket:put_hierarchical_acc(
        Id,
        #{
            type => stateful,
            static => Template#template.static,
            dynamic => Dynamic
        },
        DynamicSocket
    ),
    Struct = #{
        type => stateful,
        id => Id
    },
    {Struct, HierarchicalSocket}.

% resolver copy
resolve_template_dynamic(Template, Socket) ->
    DynamicSequence = arizona_template:dynamic_sequence(Template),
    DynamicTuple = arizona_template:dynamic(Template),
    resolve_dynamic_callbacks(DynamicSequence, DynamicTuple, Socket).
% resolver copy
resolve_dynamic_callbacks([], _DynamicTuple, Socket) ->
    {[], Socket};
resolve_dynamic_callbacks([ElementIndex | T], DynamicTuple, Socket) ->
    % Notify live process of current element index
    ok = arizona_socket:notify_current_element_index(ElementIndex, Socket),

    DynamicCallback = element(ElementIndex, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {StatefulHtml, StatefulSocket} = Callback(Socket),
            {Html, HtmlSocket} = arizona_html:to_html(StatefulHtml, StatefulSocket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, HtmlSocket),
            {[Html | RestHtml], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
            {[Html | RestHtml], FinalSocket}
    end.

-spec resolve_template(Template, Socket) -> {Html, Socket1} when
    Template :: term(),
    Socket :: term(),
    Html :: iodata(),
    Socket1 :: term().
resolve_template(Template, Socket) ->
    Static = arizona_template:static(Template),
    {Dynamic, FinalSocket} = resolve_template_dynamic(Template, Socket),
    Html = zip_static_dynamic(Static, Dynamic),
    {Html, FinalSocket}.

%% Zip static and dynamic parts for list item
zip_static_dynamic([], []) ->
    [];
zip_static_dynamic([S | Static], [D | Dynamic]) ->
    [S, D | zip_static_dynamic(Static, Dynamic)];
zip_static_dynamic([S | Static], []) ->
    [S | zip_static_dynamic(Static, [])];
zip_static_dynamic([], [D | Dynamic]) ->
    [D | zip_static_dynamic([], Dynamic)].

-spec render_stateless(Module, Function, Bindings) -> Callback when
    Module :: atom(),
    Function :: atom(),
    Bindings :: map(),
    Callback :: fun((arizona_socket:socket()) -> {iodata() | term(), arizona_socket:socket()}).
render_stateless(Mod, Fun, Bindings) ->
    fun(Socket) ->
        case arizona_socket:get_mode(Socket) of
            render ->
                render_stateless(Mod, Fun, Bindings, Socket);
            diff ->
                diff_stateless(Mod, Fun, Bindings, Socket);
            hierarchical ->
                hierarchical_stateless(Mod, Fun, Bindings, Socket)
        end
    end.

prepare_stateless_render(Mod, Fun, Bindings, Socket) ->
    Template = arizona_stateless:call_render_callback(Mod, Fun, Bindings),
    TempSocket = arizona_socket:with_temp_bindings(Bindings, Socket),
    {Template, TempSocket}.

render_stateless(Mod, Fun, Bindings, Socket) ->
    {Template, TempSocket} = prepare_stateless_render(Mod, Fun, Bindings, Socket),
    resolve_template(Template, TempSocket).

diff_stateless(Mod, Fun, Bindings, Socket) ->
    % Stateless components don't have state tracking like stateful components
    % For now, we can implement a simple approach that re-renders the entire
    % stateless component when any changes occur, since stateless components
    % are typically small and don't benefit from fine-grained diffing

    % Get runtime-tracked variable dependencies from live process
    case arizona_socket:get_live_pid(Socket) of
        undefined ->
            % No live process, can't do runtime diffing
            {[], Socket};
        LivePid ->
            ElementIndex = arizona_live:get_current_element_index(LivePid),

            % Re-render the entire stateless component
            {Template, TempSocket} = prepare_stateless_render(Mod, Fun, Bindings, Socket),

            % Convert template to HTML for diff
            {Html, FinalSocket} = resolve_template(Template, TempSocket),

            ElementChange = {ElementIndex, Html},

            % Return the complete HTML as a change
            % In a more sophisticated implementation, we could track element-level
            % changes within stateless components, but for now this provides
            % a working diff mechanism
            {ElementChange, FinalSocket}
    end.

hierarchical_stateless(Mod, Fun, Bindings, Socket) ->
    {Template, TempSocket} = prepare_stateless_render(Mod, Fun, Bindings, Socket),
    {Dynamic, DynamicSocket} = resolve_template_dynamic(Template, TempSocket),
    Struct = #{
        type => stateless,
        static => Template#template.static,
        dynamic => Dynamic
    },
    {Struct, DynamicSocket}.
