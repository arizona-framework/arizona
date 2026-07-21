-module(arizona_test_log_handler).
-moduledoc false.

%% A logger handler that forwards each log event to a test pid carried in the
%% handler's own config, so a suite can assert a specific log was emitted.
%% Add it with:
%%   logger:add_handler(Id, arizona_test_log_handler,
%%                      #{level => error, config => #{pid => self()}}).

-export([log/2]).

-ignore_xref([log/2]).

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(LogEvent, #{config := #{pid := Pid}}) ->
    Pid ! {?MODULE, LogEvent},
    ok.
