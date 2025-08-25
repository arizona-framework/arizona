-module(arizona_pubsub).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([broadcast/2]).
-export([broadcast_from/3]).
-export([join/2]).
-export([leave/2]).
-export([get_members/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([topic/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal topic() :: binary().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec broadcast(Topic, Data) -> ok when
    Topic :: topic(),
    Data :: dynamic().
broadcast(Topic, Data) when is_binary(Topic) ->
    Members = pg:get_members(?MODULE, Topic),
    lists:foreach(
        fun(Pid) ->
            Pid ! {pubsub_message, Topic, Data}
        end,
        Members
    ),
    ok.

-spec broadcast_from(From, Topic, Data) -> ok when
    From :: pid(),
    Topic :: topic(),
    Data :: dynamic().
broadcast_from(From, Topic, Data) when is_binary(Topic) ->
    Members = pg:get_members(?MODULE, Topic),
    lists:foreach(
        fun
            (Pid) when Pid =:= From ->
                ok;
            (Pid) ->
                Pid ! {pubsub_message, Topic, Data}
        end,
        Members
    ).

-spec join(Topic, Pid) -> ok when
    Topic :: topic(),
    Pid :: pid().
join(Topic, Pid) when is_binary(Topic), is_pid(Pid) ->
    pg:join(?MODULE, Topic, Pid).

-spec leave(Topic, Pid) -> ok | not_joined when
    Topic :: topic(),
    Pid :: pid().
leave(Topic, Pid) when is_binary(Topic), is_pid(Pid) ->
    pg:leave(?MODULE, Topic, Pid).

-spec get_members(Topic) -> [pid()] when
    Topic :: topic().
get_members(Topic) when is_binary(Topic) ->
    pg:get_members(?MODULE, Topic).
