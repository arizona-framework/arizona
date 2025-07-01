-module(arizona_socket).

-export([new/1]).
-export([get_mode/1]).
-export([get_current_stateful_id/1]).
-export([get_current_stateful_state/1]).
-export([get_stateful_states/1]).
-export([get_stateful_state/2, find_stateful_state/2]).

-export([set_current_stateful_id/2]).
-export([set_html_acc/2, get_html/1]).

-export([put_stateful_state/3]).

%% Binding functions
-export([get_binding/2]).
-export([with_temp_bindings/2, get_temp_binding/2]).

%% Types
-type bindings() :: #{atom() => term()}.
-type mode() :: render | diff.
-type socket_opts() :: #{
    mode => mode(),
    current_stateful_parent_id => arizona_stateful:id() | undefined,
    current_stateful_id => arizona_stateful:id()
}.
-export_type([bindings/0, mode/0, socket_opts/0]).

-record(socket, {
    mode :: mode(),
    html_acc :: iolist(),
    changes_acc :: [{binary(), iodata()}],
    current_stateful_parent_id :: arizona_stateful:id() | undefined,
    current_stateful_id :: arizona_stateful:id(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:stateful()},
    temp_bindings :: map()  % For stateless component bindings, always a map
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

-spec new(Opts) -> Socket when
    Opts :: socket_opts(),
    Socket :: socket().
new(Opts) when is_map(Opts) ->
    #socket{
        mode = maps:get(mode, Opts, render),
        html_acc = [],
        changes_acc = [],
        current_stateful_parent_id = maps:get(current_stateful_parent_id, Opts, undefined),
        current_stateful_id = maps:get(current_stateful_id, Opts, root),
        stateful_states = #{},
        temp_bindings = #{}
    }.

-spec get_mode(Socket) -> Mode when
    Socket :: socket(),
    Mode :: mode().
get_mode(#socket{} = Socket) ->
    Socket#socket.mode.

-spec get_current_stateful_id(Socket) -> Id when
    Socket :: socket(),
    Id :: arizona_stateful:id().
get_current_stateful_id(#socket{} = Socket) ->
    Socket#socket.current_stateful_id.

-spec get_current_stateful_state(Socket) -> Stateful when
    Socket :: socket(),
    Stateful :: arizona_stateful:stateful().
get_current_stateful_state(#socket{} = Socket) ->
    Id = get_current_stateful_id(Socket),
    get_stateful_state(Id, Socket).

-spec get_stateful_state(Id, Socket) -> Stateful when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    Stateful :: arizona_stateful:stateful().
get_stateful_state(Id, #socket{} = Socket) ->
    maps:get(Id, Socket#socket.stateful_states).

-spec get_stateful_states(Socket) -> States when
    Socket :: socket(),
    States :: #{arizona_stateful:id() => arizona_stateful:stateful()}.
get_stateful_states(#socket{} = Socket) ->
    Socket#socket.stateful_states.

-spec find_stateful_state(Id, Socket) -> {ok, Stateful} | error when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    Stateful :: arizona_stateful:stateful().
find_stateful_state(Id, #socket{} = Socket) ->
    maps:find(Id, Socket#socket.stateful_states).

-spec set_current_stateful_id(Id, Socket) -> Socket1 when
    Id :: arizona_stateful:id(),
    Socket :: socket(),
    Socket1 :: socket().
set_current_stateful_id(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    Socket#socket{current_stateful_id = Id}.

-spec set_html_acc(Html, Socket) -> Socket1 when
    Html :: arizona_html:html(),
    Socket :: socket(),
    Socket1 :: socket().
set_html_acc(Html, #socket{} = Socket) when is_list(Html) ->
    Socket#socket{html_acc = Html}.

-spec get_html(Socket) -> Html when
    Socket :: socket(),
    Html :: arizona_html:html().
get_html(#socket{} = Socket) ->
    Socket#socket.html_acc.

-spec put_stateful_state(Id, Stateful, Socket) -> Socket1 when
    Id :: arizona_stateful:id(),
    Stateful :: arizona_stateful:stateful(),
    Socket :: socket(),
    Socket1 :: socket().
put_stateful_state(Id, State, Socket) when Id =:= root; is_binary(Id) ->
    States = Socket#socket.stateful_states,
    Socket#socket{stateful_states = States#{Id => State}}.

-spec get_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    %% First try temporary bindings (for stateless components)
    case Socket#socket.temp_bindings of
        #{Key := Value} -> Value;
        #{} ->
            %% Fall back to stateful bindings
            CurrentState = get_current_stateful_state(Socket),
            arizona_stateful:get_binding(Key, CurrentState)
    end.

%% Temporary binding functions for stateless components
-spec with_temp_bindings(Bindings, Socket) -> Socket1 when
    Bindings :: bindings(),
    Socket :: socket(),
    Socket1 :: socket().
with_temp_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    Socket#socket{temp_bindings = Bindings}.

-spec get_temp_binding(Key, Socket) -> Value when
    Key :: atom(),
    Socket :: socket(),
    Value :: term().
get_temp_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    case Socket#socket.temp_bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.
