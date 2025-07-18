-module(arizona_template_renderer).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_stateful/3]).
-export([render_stateless/4]).
-export([render_template/2]).
-export([render_dynamic_content/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec render_stateful(Module, Bindings, Socket) -> {Html, Socket1} when
    Module :: atom(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateful(Mod, Bindings, Socket) ->
    {Id, Template, Socket1} = arizona_stateful:prepare_render(Mod, Bindings, Socket),
    % Clear dependencies for this component before starting new render
    ok = arizona_socket:clear_component_dependencies(Id, Socket1),
    % Notify live process of current stateful component
    ok = arizona_socket:notify_current_stateful_id(Id, Socket1),
    render_template(Template, Socket1).

-spec render_stateless(Module, Function, Bindings, Socket) -> {Html, Socket1} when
    Module :: atom(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_stateless(Mod, Fun, Bindings, Socket) ->
    {Template, TempSocket} = arizona_stateless:prepare_render(Mod, Fun, Bindings, Socket),
    render_template(Template, TempSocket).

-spec render_template(Template, Socket) -> {Html, Socket1} when
    Template :: arizona_template:template(),
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html(),
    Socket1 :: arizona_socket:socket().
render_template(Template, Socket) ->
    Static = arizona_template:static(Template),
    {Dynamic, FinalSocket} = render_dynamic_content(Template, Socket),
    Html = zip_static_dynamic(Static, Dynamic),
    {Html, FinalSocket}.

-spec render_dynamic_content(Template, Socket) -> {Dynamic, Socket1} when
    Template :: arizona_template:template(),
    Socket :: arizona_socket:socket(),
    Dynamic :: [arizona_html:html()],
    Socket1 :: arizona_socket:socket().
render_dynamic_content(Template, Socket) ->
    DynamicSequence = arizona_template:dynamic_sequence(Template),
    DynamicTuple = arizona_template:dynamic(Template),
    render_dynamic_callbacks(DynamicSequence, DynamicTuple, Socket).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

render_dynamic_callbacks([], _DynamicTuple, Socket) ->
    {[], Socket};
render_dynamic_callbacks([ElementIndex | T], DynamicTuple, Socket) ->
    % Notify live process of current element index
    ok = arizona_socket:notify_current_element_index(ElementIndex, Socket),

    DynamicCallback = element(ElementIndex, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Html, CallbackSocket} = Callback(Socket),
            {RestHtml, FinalSocket} = render_dynamic_callbacks(T, DynamicTuple, CallbackSocket),
            {[Html | RestHtml], FinalSocket};
        Result ->
            {Html, NewSocket} = arizona_html:to_html(Result, Socket),
            {RestHtml, FinalSocket} = render_dynamic_callbacks(T, DynamicTuple, NewSocket),
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
