-module(arizona_resolver).
-export([resolve_stateful/2, resolve_template/2]).

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
    DynamicTuple = arizona_template:dynamic(Template),
    DynamicSequence = arizona_template:dynamic_sequence(Template),
    {Dynamic, NewSocket} = resolve_dynamic_callbacks(DynamicSequence, DynamicTuple, Socket),
    Html = zip_static_dynamic(Static, Dynamic),
    {Html, NewSocket}.

resolve_dynamic_callbacks([], _DynamicTuple, Socket) ->
    {[], Socket};
resolve_dynamic_callbacks([Index | T], DynamicTuple, Socket) ->
    case apply(element(Index, DynamicTuple), []) of
        {'$arizona_stateful_template', StatefulTemplate} ->
            {Html, NewSocket} = resolve_template(StatefulTemplate, Socket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
            {[Html | RestHtml], FinalSocket};
        {'$arizona_stateless_template', StatelessTemplate} ->
            {Html, NewSocket} = resolve_template(StatelessTemplate, Socket),
            {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
            {[Html | RestHtml], FinalSocket};
        Result ->
            case arizona_socket:is_socket(Result) of
                true ->
                    % If a callback returns a socket, extract the HTML from it
                    Html = arizona_socket:get_html(Result),
                    {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, Result),
                    {[Html | RestHtml], FinalSocket};
                false ->
                    % Simple value - convert to HTML
                    {Html, NewSocket} = arizona_html:to_html(Result, Socket),
                    {RestHtml, FinalSocket} = resolve_dynamic_callbacks(T, DynamicTuple, NewSocket),
                    {[Html | RestHtml], FinalSocket}
            end
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
