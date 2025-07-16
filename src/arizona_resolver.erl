-module(arizona_resolver).
-export([resolve_stateful/2, resolve_template/2, zip_static_dynamic/2]).

-spec resolve_stateful(StatefulId, Socket) -> {Html, Socket1} when
    StatefulId :: term(),
    Socket :: term(),
    Html :: iodata(),
    Socket1 :: term().
resolve_stateful(StatefulId, Socket) ->
    State = arizona_socket:get_stateful_state(StatefulId, Socket),
    Mod = arizona_stateful:get_module(State),
    Bindings = arizona_stateful:get_bindings(State),
    Template = arizona_stateful:call_render_callback(Mod, Bindings),
    resolve_template(Template, Socket).

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

resolve_template_dynamic(Template, Socket) ->
    DynamicTuple = arizona_template:dynamic(Template),
    DynamicSequence = arizona_template:dynamic_sequence(Template),
    resolve_dynamic_callbacks(DynamicSequence, DynamicTuple, Socket).

resolve_dynamic_callbacks([], _DynamicTuple, Socket) ->
    {[], Socket};
resolve_dynamic_callbacks([Index | T], DynamicTuple, Socket) ->
    case apply(element(Index, DynamicTuple), []) of
        {'$arizona_render_stateful_template', Mod, Bindings} ->
            {StatefulTemplate, UpdatedSocket} = call_stateful(Mod, Bindings, Socket),
            {Html, StatefulSocket} = resolve_template(StatefulTemplate, UpdatedSocket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, StatefulSocket),
            {[Html | RestHtml], FinalSocket};
        {'$arizona_render_stateless_template', Mod, Fun, Bindings} ->
            StatelessTemplate = arizona_stateless:call_render_callback(Mod, Fun, Bindings),
            TempSocket = arizona_socket:with_temp_bindings(Bindings, Socket),
            {Html, StatelessSocket} = resolve_template(StatelessTemplate, TempSocket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, StatelessSocket),
            {[Html | RestHtml], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
            {[Html | RestHtml], FinalSocket}
    end.

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
            case arizona_stateful:should_remount(UpdatedState) of
                true ->
                    Socket1 = arizona_stateful:call_unmount_callback(Mod, Socket),
                    %% Call mount callback after unmount for remount
                    Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
                    %% Call the component's render callback which handles
                    %% rendering and returns updated socket
                    MountedState = arizona_socket:get_current_stateful_state(Socket2),
                    MountedBindings = arizona_stateful:get_bindings(MountedState),
                    Template = arizona_stateful:call_render_callback(Mod, MountedBindings),
                    {Template, Socket2};
                false ->
                    %% Update socket with new state and call render callback (which handles diffing)
                    Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
                    UpdatedBindings = arizona_stateful:get_bindings(UpdatedState),
                    Template = arizona_stateful:call_render_callback(Mod, UpdatedBindings),
                    {Template, Socket1}
            end;
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(State, Socket),
            %% Call mount callback for new components
            Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
            %% Call the component's render callback which handles
            %% rendering and returns updated socket
            MountedState = arizona_socket:get_current_stateful_state(Socket2),
            MountedBindings = arizona_stateful:get_bindings(MountedState),
            Template = arizona_stateful:call_render_callback(Mod, MountedBindings),
            {Template, Socket2}
    end.

%% Zip static and dynamic parts for list item
zip_static_dynamic([], []) ->
    [];
zip_static_dynamic([S | Static], [D | Dynamic]) ->
    [S, D | zip_static_dynamic(Static, Dynamic)];
zip_static_dynamic([S | Static], []) ->
    [S | zip_static_dynamic(Static, [])];
zip_static_dynamic([], [D | Dynamic]) ->
    [D | zip_static_dynamic([], Dynamic)].
