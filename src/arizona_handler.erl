-module(arizona_handler).

-behaviour(cowboy_handler).

-export([init/2]).

%% @doc Cowboy handler entry point
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req, State) ->
    % Extract route information from state
    Handler = maps:get(handler, State),
    Opts = maps:get(opts, State, #{}),
    handle_live_request(Handler, Opts, Req, State).

%% @doc Handle LiveView requests
-spec handle_live_request(atom(), map(), cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()}.
handle_live_request(LiveModule, Opts, Req, State) ->
    try
        % Extract path parameters
        TempBindings = cowboy_req:bindings(Req),

        % Create Arizona socket
        Socket = arizona_socket:new(#{}),
        RootState = arizona_stateful:new(root, LiveModule, #{}),
        Socket1 = arizona_socket:put_stateful_state(RootState, Socket),
        Socket2 = arizona_socket:with_temp_bindings(TempBindings, Socket1),

        % Handle LiveView with layout
        Html = handle_live_with_layout(LiveModule, Opts, Socket2),
        Req1 = cowboy_req:reply(200, #{~"content-type" => ~"text/html"}, Html, Req),
        {ok, Req1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("LiveView Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            Req2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), Req),
            {ok, Req2, State}
    end.

%% @doc Handle LiveView with layout injection
-spec handle_live_with_layout(atom(), map(), arizona_socket:socket()) -> arizona_html:html().
handle_live_with_layout(LiveModule, Opts, Socket) ->
    % Mount and render the LiveView component
    Socket1 = arizona_stateful:call_mount_callback(LiveModule, Socket),
    Socket2 = arizona_stateful:call_render_callback(LiveModule, Socket1),
    ComponentHtml = arizona_socket:get_html(Socket2),

    % Check if layout is specified
    case Opts of
        #{layout := {LayoutModule, LayoutFun, SlotName}} when
            is_atom(LayoutModule), is_atom(LayoutFun), is_atom(SlotName)
        ->
            % Inject component into layout
            Socket2 = arizona_socket:put_binding(SlotName, ComponentHtml, Socket1),

            % Render layout
            Socket3 = arizona_stateless:call_render_callback(LayoutModule, LayoutFun, Socket2),
            arizona_socket:get_html(Socket3);
        #{} when not is_map_key(layout, Opts) ->
            % No layout, return component HTML directly
            ComponentHtml
    end.
