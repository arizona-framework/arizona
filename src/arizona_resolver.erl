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
    DynamicCallback = element(Index, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Template, TemplateSocket} = Callback(Socket),
            {Html, StatefulSocket} = resolve_template(Template, TemplateSocket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, StatefulSocket),
            {[Html | RestHtml], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
            {[Html | RestHtml], FinalSocket}
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
