-module(arizona_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%% @doc Cowboy handler entry point
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req, State) when is_map(State) ->
    % Extract route information from state
    Handler = maps:get(handler, State),
    handle_live_request(Handler, Req, State).

%% @doc Handle LiveView requests
-spec handle_live_request(atom(), cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()}.
handle_live_request(LiveModule, Req, State) when is_atom(LiveModule), is_map(State) ->
    try
        % Create arizona request abstraction
        ArizonaReq = arizona_request:from_cowboy(Req),

        % Create Arizona socket and call mount via arizona_live callback wrapper
        Socket = arizona_socket:new(#{}),
        StatefulState = arizona_stateful:new(root, LiveModule, #{}),
        Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),
        Socket2 = arizona_live:call_mount_callback(LiveModule, ArizonaReq, Socket1),

        % Render the LiveView via arizona_live callback wrapper
        Socket3 = arizona_live:call_render_callback(LiveModule, Socket2),

        % Get final HTML
        Html = arizona_socket:get_html(Socket3),
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
