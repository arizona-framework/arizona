-module(arizona_differ).

-export([diff_stateful/2]).

%% Mock implementation - returns empty diff for now
-spec diff_stateful(Stateful, Socket) -> Socket1 when
    Stateful :: arizona_stateful:stateful(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateful(_Stateful, Socket) ->
    %% TODO: implement proper stateful diffing
    %% For now, return socket unchanged with empty diff
    Socket.