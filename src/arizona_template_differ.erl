-module(arizona_template_differ).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_stateful/3]).
-export([diff_stateless/4]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec diff_stateful(Module, Bindings, Socket) -> {Diff, Socket1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Diff :: [term()],
    Socket1 :: arizona_socket:socket().
diff_stateful(Mod, Bindings, Socket) ->
    % Get runtime-tracked variable dependencies from live process
    case arizona_socket:get_live_pid(Socket) of
        undefined ->
            % No live process, can't do runtime diffing
            {[], Socket};
        LivePid ->
            {Id, Template, Socket1} = arizona_stateful:prepare_render(Mod, Bindings, Socket),
            StatefulState = arizona_socket:get_stateful_state(Id, Socket1),
            ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),

            case arizona_binder:is_empty(ChangedBindings) of
                true ->
                    % Clear dependencies for this component before starting new render
                    ok = arizona_socket:clear_component_dependencies(Id, Socket1),

                    {[], Socket1};
                true ->
                    Dependencies = arizona_live:get_component_dependencies(LivePid, Id),

                    % Clear dependencies for this component before starting new render
                    ok = arizona_socket:clear_component_dependencies(Id, Socket1),

                    % Find affected elements using runtime dependencies
                    AffectedElements = get_affected_elements(ChangedBindings, Dependencies),
                    % Generate diff for affected elements
                    generate_element_diff(AffectedElements, Template, Socket1)
            end
    end.

-spec diff_stateless(Module, Function, Bindings, Socket) -> {Html, Socket1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
diff_stateless(Mod, Fun, Bindings, Socket) ->
    % Stateless components don't have state tracking like stateful components
    % For now, we can implement a simple approach that re-renders the entire
    % stateless component when any changes occur, since stateless components
    % are typically small and don't benefit from fine-grained diffing

    % Re-render the entire stateless component
    {Template, TempSocket} = arizona_stateless:prepare_render(Mod, Fun, Bindings, Socket),

    % Convert template to HTML for diff
    {Html, FinalSocket} = arizona_template_renderer:render_template(Template, TempSocket),

    % Return the complete HTML as a change
    % In a more sophisticated implementation, we could track element-level
    % changes within stateless components, but for now this provides
    % a working diff mechanism
    {Html, FinalSocket}.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Get affected elements from changed bindings and variable dependencies
get_affected_elements(ChangedBindings, Dependencies) ->
    ChangedVarNames = arizona_binder:keys(ChangedBindings),
    AffectedIndexLists = [
        arizona_binder:get(VarName, Dependencies, [])
     || VarName <- ChangedVarNames
    ],
    sets:from_list(lists:flatten(AffectedIndexLists)).

%% Generate diff for affected elements
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
process_affected_elements([], _DynamicTuple, Socket) ->
    {[], Socket};
process_affected_elements([ElementIndex | T], DynamicTuple, Socket) ->
    % Notify live process of current element index
    ok = arizona_socket:notify_current_element_index(ElementIndex, Socket),

    DynamicCallback = element(ElementIndex, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Html, CallbackSocket} = Callback(Socket),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalSocket} = process_affected_elements(T, DynamicTuple, CallbackSocket),
            {[ElementChange | RestChanges], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalSocket} = process_affected_elements(T, DynamicTuple, NewSocket),
            {[ElementChange | RestChanges], FinalSocket}
    end.
