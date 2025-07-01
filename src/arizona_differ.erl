-module(arizona_differ).

-export([diff_stateful/2]).

%% Mock implementation - returns empty diff for now
diff_stateful(_State, Socket) ->
    %% TODO: implement proper stateful diffing
    %% For now, return socket unchanged with empty diff
    Socket.