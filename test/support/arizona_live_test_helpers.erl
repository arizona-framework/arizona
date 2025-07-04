-module(arizona_live_test_helpers).

%% Test helpers for arizona_live components
-export([start_live/1, start_live/2]).
-export([mount_live/2, render_live/1]).
-export([send_event/3, send_info/2]).
-export([get_html/1, get_socket/1]).
-export([assert_html_contains/2, assert_binding/3, assert_binding_contains/3]).

%% Types
-type live_test_pid() :: pid().

%% Start arizona_live process for testing
-spec start_live(Module) -> {ok, Pid} when
    Module :: atom(),
    Pid :: live_test_pid().
start_live(Module) ->
    start_live(Module, #{}).

-spec start_live(Module, ReqOpts) -> {ok, Pid} when
    Module :: atom(),
    ReqOpts :: map(),
    Pid :: live_test_pid().
start_live(Module, ReqOpts) when is_atom(Module), is_map(ReqOpts) ->
    Socket = arizona_socket:new(#{mode => render}),
    {ok, Pid} = arizona_live:start_link(Module, Socket),

    % Create arizona_request from options
    Req = arizona_request:new(ReqOpts),
    _Socket = arizona_live:mount(Pid, Req),
    {ok, Pid}.

%% Mount the live component (useful for re-mounting with different request)
-spec mount_live(Pid, ReqOpts) -> Socket when
    Pid :: live_test_pid(),
    ReqOpts :: map(),
    Socket :: arizona_socket:socket().
mount_live(Pid, ReqOpts) when is_map(ReqOpts) ->
    Req = arizona_request:new(ReqOpts),
    arizona_live:mount(Pid, Req).

%% Render the live component
-spec render_live(Pid) -> Socket when
    Pid :: live_test_pid(),
    Socket :: arizona_socket:socket().
render_live(Pid) ->
    arizona_live:render(Pid).

%% Send an event to the live component
-spec send_event(Pid, Event, Params) -> {noreply, Socket} | {reply, Reply, Socket} when
    Pid :: live_test_pid(),
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().
send_event(Pid, Event, Params) ->
    arizona_live:handle_event(Pid, Event, Params).

%% Send an info message to the live component
-spec send_info(Pid, Info) -> ok when
    Pid :: live_test_pid(),
    Info :: term().
send_info(Pid, Info) ->
    Pid ! Info,
    ok.

%% Get the HTML from a rendered socket
-spec get_html(Socket) -> Html when
    Socket :: arizona_socket:socket(),
    Html :: arizona_html:html().
get_html(Socket) ->
    arizona_socket:get_html(Socket).

%% Get the socket (identity function for consistency)
-spec get_socket(Socket) -> Socket when
    Socket :: arizona_socket:socket().
get_socket(Socket) ->
    Socket.

%% Test assertions

%% Assert that rendered HTML contains a specific string
-spec assert_html_contains(Socket, ExpectedContent) -> ok when
    Socket :: arizona_socket:socket(),
    ExpectedContent :: binary() | string().
assert_html_contains(Socket, ExpectedContent) ->
    Html = get_html(Socket),
    HtmlBinary = iolist_to_binary(Html),
    ExpectedBinary =
        case ExpectedContent of
            Bin when is_binary(Bin) -> Bin;
            Str when is_list(Str) -> list_to_binary(Str)
        end,
    case binary:match(HtmlBinary, ExpectedBinary) of
        nomatch ->
            error({assertion_failed, {html_does_not_contain, ExpectedBinary, HtmlBinary}});
        _ ->
            ok
    end.

%% Assert that a socket has a specific binding value
-spec assert_binding(Socket, Key, ExpectedValue) -> ok when
    Socket :: arizona_socket:socket(),
    Key :: atom(),
    ExpectedValue :: term().
assert_binding(Socket, Key, ExpectedValue) ->
    ActualValue = arizona_socket:get_binding(Key, Socket),
    case ActualValue =:= ExpectedValue of
        true ->
            ok;
        false ->
            error({assertion_failed, {binding_mismatch, Key, ExpectedValue, ActualValue}})
    end.

%% Assert that a socket binding (which should be a list) contains a specific value
-spec assert_binding_contains(Socket, Key, ExpectedValue) -> ok when
    Socket :: arizona_socket:socket(),
    Key :: atom(),
    ExpectedValue :: term().
assert_binding_contains(Socket, Key, ExpectedValue) ->
    ActualList = arizona_socket:get_binding(Key, Socket),
    case lists:member(ExpectedValue, ActualList) of
        true ->
            ok;
        false ->
            error({assertion_failed, {binding_does_not_contain, Key, ExpectedValue, ActualList}})
    end.
