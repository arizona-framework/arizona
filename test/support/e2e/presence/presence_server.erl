-module(presence_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([join_user/2, leave_user/1, get_online_users/0, get_online_users_count/0, is_user_joined/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    % UserId => #{user_name, timestamp}
    online_users = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API functions
join_user(UserId, UserName) ->
    gen_server:call(?MODULE, {join_user, UserId, UserName}).

leave_user(UserId) ->
    gen_server:call(?MODULE, {leave_user, UserId}).

get_online_users() ->
    gen_server:call(?MODULE, get_online_users).

get_online_users_count() ->
    gen_server:call(?MODULE, get_online_users_count).

is_user_joined(UserId) ->
    gen_server:call(?MODULE, {is_user_joined, UserId}).

%% GenServer callbacks
init([]) ->
    {ok, #state{}}.

handle_call({join_user, UserId, UserName}, _From, State) ->
    case maps:is_key(UserId, State#state.online_users) of
        true ->
            % User already joined
            {reply, {error, already_joined}, State};
        false ->
            Timestamp = erlang:system_time(second),
            UserInfo = #{user_name => UserName, timestamp => Timestamp},
            UpdatedUsers = maps:put(UserId, UserInfo, State#state.online_users),
            % Broadcast join event
            JoinEvent = #{
                type => ~"user_joined",
                user_id => UserId,
                user_name => UserName,
                timestamp => Timestamp,
                online_users => format_users_for_broadcast(UpdatedUsers),
                online_users_count => maps:size(UpdatedUsers)
            },
            arizona_pubsub:broadcast(~"presence", JoinEvent),

            NewState = State#state{online_users = UpdatedUsers},
            {reply, ok, NewState}
    end;
handle_call({leave_user, UserId}, _From, State) ->
    case maps:find(UserId, State#state.online_users) of
        {ok, #{user_name := UserName}} ->
            UpdatedUsers = maps:remove(UserId, State#state.online_users),
            % Broadcast leave event
            LeaveEvent = #{
                type => ~"user_left",
                user_id => UserId,
                user_name => UserName,
                timestamp => erlang:system_time(second),
                online_users => format_users_for_broadcast(UpdatedUsers),
                online_users_count => maps:size(UpdatedUsers)
            },
            arizona_pubsub:broadcast(~"presence", LeaveEvent),

            NewState = State#state{online_users = UpdatedUsers},
            {reply, ok, NewState};
        error ->
            {reply, {error, user_not_found}, State}
    end;
handle_call(get_online_users, _From, State) ->
    Users = format_users_for_broadcast(State#state.online_users),
    {reply, Users, State};
handle_call(get_online_users_count, _From, State) ->
    Count = maps:size(State#state.online_users),
    {reply, Count, State};
handle_call({is_user_joined, UserId}, _From, State) ->
    IsJoined = maps:is_key(UserId, State#state.online_users),
    {reply, IsJoined, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Internal functions
format_users_for_broadcast(UsersMap) ->
    maps:fold(
        fun(UserId, #{user_name := UserName, timestamp := Timestamp}, Acc) ->
            [#{user_id => UserId, user_name => UserName, timestamp => Timestamp} | Acc]
        end,
        [],
        UsersMap
    ).
