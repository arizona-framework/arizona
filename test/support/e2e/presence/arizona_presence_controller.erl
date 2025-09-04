-module(arizona_presence_controller).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    handle_json_post/2,
    handle_json_get/2
]).

-record(state, {}).

init(Req, _State) ->
    {cowboy_rest, Req, #state{}}.

allowed_methods(Req, State) ->
    {[~"GET", ~"POST"], Req, State}.

content_types_accepted(Req, State) ->
    {[{~"application/json", handle_json_post}], Req, State}.

content_types_provided(Req, State) ->
    {[{~"application/json", handle_json_get}], Req, State}.

handle_json_post(Req, State) ->
    try
        Path = cowboy_req:path(Req),
        case Path of
            ~"/api/presence/join" ->
                handle_join(Req, State);
            ~"/api/presence/leave" ->
                handle_leave(Req, State);
            _ ->
                {false, Req, State}
        end
    catch
        Error:Reason:Stacktrace ->
            logger:error("Presence API error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            {false, Req, State}
    end.

handle_json_get(Req, State) ->
    try
        OnlineUsers = arizona_presence_server:get_online_users(),
        OnlineUsersCount = arizona_presence_server:get_online_users_count(),

        Response = json:encode(#{
            online_users => OnlineUsers,
            online_users_count => OnlineUsersCount
        }),

        {Response, Req, State}
    catch
        Error:Reason:Stacktrace ->
            logger:error("Presence API GET error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            ErrorResponse = json:encode(#{error => ~"Internal server error"}),
            {ErrorResponse, Req, State}
    end.

handle_join(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    UserData = json:decode(Body),

    UserId = maps:get(~"user_id", UserData),
    UserName = maps:get(~"user_name", UserData, ~"Anonymous"),

    % Use presence server to manage state
    Result = arizona_presence_server:join_user(UserId, UserName),
    Response =
        case Result of
            ok ->
                json:encode(#{
                    status => ~"success",
                    message => ~"User joined successfully",
                    user_id => UserId
                });
            {error, already_joined} ->
                json:encode(#{
                    status => ~"error",
                    message => ~"User is already in presence",
                    user_id => UserId
                })
        end,

    Req2 = cowboy_req:set_resp_body(Response, Req1),
    {true, Req2, State}.

handle_leave(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    UserData = json:decode(Body),

    UserId = maps:get(~"user_id", UserData),

    % Use presence server to manage state - handle user not found gracefully
    Result = arizona_presence_server:leave_user(UserId),
    Response =
        case Result of
            ok ->
                json:encode(#{
                    status => ~"success",
                    message => ~"User left successfully",
                    user_id => UserId
                });
            {error, user_not_found} ->
                json:encode(#{
                    status => ~"success",
                    message => ~"User was not in presence list",
                    user_id => UserId
                })
        end,

    Req2 = cowboy_req:set_resp_body(Response, Req1),
    {true, Req2, State}.
