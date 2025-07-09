-module(arizona_socket).

-export([new/1, is_socket/1]).
-export([get_mode/1, set_mode/2]).
-export([get_current_stateful_id/1]).
-export([get_current_stateful_state/1]).
-export([get_stateful_states/1]).
-export([get_stateful_state/2, find_stateful_state/2]).

-export([set_current_stateful_id/2]).
-export([set_html_acc/2, get_html/1]).
-export([append_changes/2, get_changes/1, clear_changes/1]).
-export([set_hierarchical_acc/2, get_hierarchical_acc/1]).
-export([
    set_hierarchical_pending_element/2,
    get_hierarchical_pending_element/1,
    clear_hierarchical_pending_element/1
]).

-export([put_stateful_state/2]).

%% Binding functions
-export([get_binding/2, get_binding/3, put_binding/3, put_bindings/2]).
-export([with_temp_bindings/2, get_temp_binding/2]).

%% Layout functions
-export([set_layout/2, get_layout/1]).

%% Types
-type bindings() :: #{atom() => term()}.
-type mode() :: render | diff | hierarchical.
-type layout() :: {LayoutModule :: atom(), LayoutRenderFun :: atom(), SlotName :: atom()}.
-type socket_opts() :: #{
    mode => mode(),
    current_stateful_parent_id => arizona_stateful:id() | undefined,
    current_stateful_id => arizona_stateful:id()
}.
-export_type([bindings/0, mode/0, layout/0, socket_opts/0]).

-record(socket, {
    mode :: mode(),
    html_acc :: iolist(),
    changes_acc :: arizona_differ:diff_changes(),
    hierarchical_acc :: arizona_hierarchical:hierarchical_structure(),
    hierarchical_pending_element :: arizona_hierarchical:element_content() | undefined,
    current_stateful_parent_id :: arizona_stateful:id() | undefined,
    current_stateful_id :: arizona_stateful:id(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:state()},
    % For stateless component bindings, always a map
    temp_bindings :: map(),
    layout :: layout() | undefined
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

-spec new(Opts) -> Socket when
    Opts :: socket_opts(),
    Socket :: socket().
new(Opts) when is_map(Opts) ->
    #socket{
        mode = maps:get(mode, Opts, render),
        html_acc = [],
        changes_acc = [],
        hierarchical_acc = #{},
        hierarchical_pending_element = undefined,
        current_stateful_parent_id = maps:get(current_stateful_parent_id, Opts, undefined),
        current_stateful_id = maps:get(current_stateful_id, Opts, root),
        stateful_states = #{},
        temp_bindings = #{},
        layout = undefined
    }.

-spec is_socket(Term) -> boolean() when
    Term :: term().
is_socket(#socket{}) -> true;
is_socket(_) -> false.

-spec get_mode(Socket) -> Mode when
    Socket :: socket(),
    Mode :: mode().
get_mode(#socket{} = Socket) ->
    Socket#socket.mode.

-spec set_mode(Mode, Socket) -> Socket1 when
    Mode :: mode(),
    Socket :: socket(),
    Socket1 :: socket().
set_mode(Mode, #socket{} = Socket) when Mode =:= render; Mode =:= diff; Mode =:= hierarchical ->
    Socket#socket{mode = Mode}.

-spec get_current_stateful_id(Socket) -> Id when
    Socket :: socket(),
    Id :: arizona_stateful:id().
get_current_stateful_id(#socket{} = Socket) ->
    Socket#socket.current_stateful_id.

-spec get_current_stateful_state(Socket) -> StatefulState when
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
get_current_stateful_state(#socket{} = Socket) ->
    Id = get_current_stateful_id(Socket),
    get_stateful_state(Id, Socket).

-spec get_stateful_state(Id, Socket) -> StatefulState when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
get_stateful_state(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    maps:get(Id, Socket#socket.stateful_states).

-spec get_stateful_states(Socket) -> States when
    Socket :: socket(),
    States :: #{arizona_stateful:id() => arizona_stateful:state()}.
get_stateful_states(#socket{} = Socket) ->
    Socket#socket.stateful_states.

-spec find_stateful_state(Id, Socket) -> {ok, StatefulState} | error when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    StatefulState :: arizona_stateful:state().
find_stateful_state(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    maps:find(Id, Socket#socket.stateful_states).

-spec set_current_stateful_id(Id, Socket) -> Socket1 when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    Socket1 :: socket().
set_current_stateful_id(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    Socket#socket{current_stateful_id = Id}.

-spec set_html_acc(Html, Socket) -> Socket1 when
    Html :: arizona_html:html(),
    Socket :: socket(),
    Socket1 :: socket().
set_html_acc(Html, #socket{} = Socket) when is_list(Html) ->
    Socket#socket{html_acc = Html}.

-spec get_html(Socket) -> Html when
    Socket :: socket(),
    Html :: arizona_html:html().
get_html(#socket{} = Socket) ->
    Socket#socket.html_acc.

-spec set_hierarchical_acc(HierarchicalStructure, Socket) -> Socket1 when
    HierarchicalStructure :: arizona_hierarchical:hierarchical_structure(),
    Socket :: socket(),
    Socket1 :: socket().
set_hierarchical_acc(HierarchicalStructure, #socket{} = Socket) when
    is_map(HierarchicalStructure)
->
    Socket#socket{hierarchical_acc = HierarchicalStructure}.

-spec get_hierarchical_acc(Socket) -> HierarchicalStructure when
    Socket :: socket(),
    HierarchicalStructure :: arizona_hierarchical:hierarchical_structure().
get_hierarchical_acc(#socket{} = Socket) ->
    Socket#socket.hierarchical_acc.

-spec set_hierarchical_pending_element(Element, Socket) -> Socket1 when
    Element :: arizona_hierarchical:element_content(),
    Socket :: socket(),
    Socket1 :: socket().
set_hierarchical_pending_element(Element, #socket{} = Socket) ->
    Socket#socket{hierarchical_pending_element = Element}.

-spec get_hierarchical_pending_element(Socket) -> Element when
    Socket :: socket(),
    Element :: arizona_hierarchical:element_content() | undefined.
get_hierarchical_pending_element(#socket{} = Socket) ->
    Socket#socket.hierarchical_pending_element.

-spec clear_hierarchical_pending_element(Socket) -> Socket1 when
    Socket :: socket(),
    Socket1 :: socket().
clear_hierarchical_pending_element(#socket{} = Socket) ->
    Socket#socket{hierarchical_pending_element = undefined}.

-spec put_stateful_state(StatefulState, Socket) -> Socket1 when
    StatefulState :: arizona_stateful:state(),
    Socket :: socket(),
    Socket1 :: socket().
put_stateful_state(State, #socket{} = Socket) ->
    Id = arizona_stateful:get_id(State),
    States = Socket#socket.stateful_states,
    Socket#socket{stateful_states = States#{Id => State}}.

-spec get_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    %% First try temporary bindings (for stateless components)
    case Socket#socket.temp_bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            %% Fall back to stateful bindings
            CurrentState = get_current_stateful_state(Socket),
            arizona_stateful:get_binding(Key, CurrentState)
    end.

-spec get_binding(Key, Socket, Default) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Default :: term(),
    Value :: term() | Default.
get_binding(Key, #socket{} = Socket, Default) when is_atom(Key) ->
    %% First try temporary bindings (for stateless components)
    case Socket#socket.temp_bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            %% Fall back to stateful bindings
            CurrentState = get_current_stateful_state(Socket),
            arizona_stateful:get_binding(Key, CurrentState, Default)
    end.

%% Temporary binding functions for stateless components
-spec with_temp_bindings(Bindings, Socket) -> Socket1 when
    Bindings :: bindings(),
    Socket :: socket(),
    Socket1 :: socket().
with_temp_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    Socket#socket{temp_bindings = Bindings}.

-spec get_temp_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_temp_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    case Socket#socket.temp_bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.

-spec put_binding(Key, Value, Socket) -> Socket1 when
    Key :: atom(),
    Value :: term(),
    Socket :: socket(),
    Socket1 :: socket().
put_binding(Key, Value, #socket{} = Socket) when is_atom(Key) ->
    CurrentState = get_current_stateful_state(Socket),
    UpdatedState = arizona_stateful:put_binding(Key, Value, CurrentState),
    put_stateful_state(UpdatedState, Socket).

-spec put_bindings(Bindings, Socket) -> Socket1 when
    Bindings :: bindings(),
    Socket :: socket(),
    Socket1 :: socket().
put_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    CurrentState = get_current_stateful_state(Socket),
    UpdatedState = arizona_stateful:put_bindings(Bindings, CurrentState),
    put_stateful_state(UpdatedState, Socket).

%% Changes accumulator functions (for diff mode)

-spec append_changes(Changes, Socket) -> Socket1 when
    Changes :: arizona_differ:diff_changes(),
    Socket :: socket(),
    Socket1 :: socket().
append_changes(Changes, #socket{} = Socket) when is_list(Changes) ->
    CurrentChanges = Socket#socket.changes_acc,
    % Merge changes at the correct hierarchical path
    MergedChanges = merge_changes(Changes, CurrentChanges),
    Socket#socket{changes_acc = MergedChanges}.

-spec get_changes(Socket) -> Changes when
    Socket :: socket(),
    Changes :: arizona_differ:diff_changes().
get_changes(#socket{} = Socket) ->
    % Reverse to get changes in chronological order
    lists:reverse(Socket#socket.changes_acc).

-spec clear_changes(Socket) -> Socket1 when
    Socket :: socket(),
    Socket1 :: socket().
clear_changes(#socket{} = Socket) ->
    Socket#socket{changes_acc = []}.

%% Merge new changes with existing changes, maintaining hierarchical structure
%% Format: [{StatefulId, [{ElementIndex, Changes}]}]
-spec merge_changes(NewChanges, ExistingChanges) -> MergedChanges when
    NewChanges :: arizona_differ:diff_changes(),
    ExistingChanges :: arizona_differ:diff_changes(),
    MergedChanges :: arizona_differ:diff_changes().
merge_changes([], ExistingChanges) ->
    ExistingChanges;
merge_changes(NewChanges, []) ->
    NewChanges;
merge_changes([{ComponentId, NewElementChanges} | RestNew], ExistingChanges) ->
    % Find if this component already has changes
    case lists:keyfind(ComponentId, 1, ExistingChanges) of
        false ->
            % Component not found, add it
            UpdatedExisting = [{ComponentId, NewElementChanges} | ExistingChanges],
            merge_changes(RestNew, UpdatedExisting);
        {ComponentId, ExistingElementChanges} ->
            % Component found, merge element changes
            MergedElementChanges = merge_element_changes(NewElementChanges, ExistingElementChanges),
            UpdatedExisting = lists:keyreplace(
                ComponentId,
                1,
                ExistingChanges,
                {ComponentId, MergedElementChanges}
            ),
            merge_changes(RestNew, UpdatedExisting)
    end.

%% Merge element changes within the same component
-spec merge_element_changes(NewElementChanges, ExistingElementChanges) ->
    MergedElementChanges
when
    NewElementChanges :: [arizona_differ:element_change_entry()],
    ExistingElementChanges :: [arizona_differ:element_change_entry()],
    MergedElementChanges :: [arizona_differ:element_change_entry()].
merge_element_changes([], ExistingElements) ->
    ExistingElements;
merge_element_changes([{ElementIndex, NewChange} | RestNew], ExistingElements) ->
    case lists:keyfind(ElementIndex, 1, ExistingElements) of
        false ->
            % Element not found, add it
            UpdatedExisting = [{ElementIndex, NewChange} | ExistingElements],
            merge_element_changes(RestNew, UpdatedExisting);
        {ElementIndex, ExistingChange} ->
            % Element found, merge the changes (for nested structures)
            MergedChange = merge_nested_changes(NewChange, ExistingChange),
            UpdatedExisting = lists:keyreplace(
                ElementIndex,
                1,
                ExistingElements,
                {ElementIndex, MergedChange}
            ),
            merge_element_changes(RestNew, UpdatedExisting)
    end.

%% Merge nested changes (recursive for deep nesting)
-spec merge_nested_changes(NewChange, ExistingChange) -> MergedChange when
    NewChange :: arizona_differ:element_change(),
    ExistingChange :: arizona_differ:element_change(),
    MergedChange :: arizona_differ:element_change().
merge_nested_changes(NewChange, ExistingChange) when is_list(NewChange), is_list(ExistingChange) ->
    % Check if these are actually component changes vs HTML data
    % Component changes are lists of {ComponentId, ElementChanges} tuples where ComponentId is an atom
    % HTML data typically contains binaries, nested lists of binaries, empty lists, etc.
    case
        looks_like_component_changes(NewChange) andalso looks_like_component_changes(ExistingChange)
    of
        true ->
            % Both are lists of component changes, merge recursively
            merge_changes(NewChange, ExistingChange);
        false ->
            % These are HTML data lists, new change takes precedence
            NewChange
    end;
merge_nested_changes(NewChange, _ExistingChange) ->
    % New change takes precedence (overwrite)
    NewChange.

%% Simple heuristic to distinguish component changes from HTML data
%% Component changes: [{ComponentId, ElementChanges}] where ComponentId is atom() | binary()
%% HTML data: typically contains binaries directly, empty lists, nested structures without tuple format
-spec looks_like_component_changes(List) -> boolean() when
    List :: list().
looks_like_component_changes([]) ->
    % Empty list could be component changes
    true;
looks_like_component_changes([{ComponentId, ElementChanges} | Rest]) when
    (is_atom(ComponentId) orelse is_binary(ComponentId)) andalso is_list(ElementChanges)
->
    % If element is {ComponentId, ElementChanges} tuple, it's likely component changes
    looks_like_component_changes(Rest);
looks_like_component_changes([Binary | _]) when is_binary(Binary) ->
    % If we see a binary directly (not in a tuple), it's likely HTML data
    false;
looks_like_component_changes([[] | Rest]) ->
    % Empty lists in HTML data are common, but check the rest
    looks_like_component_changes(Rest);
looks_like_component_changes([List | Rest]) when is_list(List) ->
    % Nested list - check if it's component changes or HTML data
    case looks_like_component_changes(List) of
        true -> looks_like_component_changes(Rest);
        false -> false
    end;
looks_like_component_changes(_) ->
    false.

%% Layout functions

-spec set_layout(Layout, Socket) -> Socket1 when
    Layout :: layout(),
    Socket :: socket(),
    Socket1 :: socket().
set_layout({LayoutModule, LayoutRenderFun, SlotName} = Layout, #socket{} = Socket) when
    is_atom(LayoutModule), is_atom(LayoutRenderFun), is_atom(SlotName)
->
    Socket#socket{layout = Layout}.

-spec get_layout(Socket) -> Layout when
    Socket :: socket(),
    Layout :: layout() | undefined.
get_layout(#socket{} = Socket) ->
    Socket#socket.layout.
