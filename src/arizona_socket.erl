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

-record(socket, {
    mode :: render | diff,
    html_acc :: iolist(),
    changes_acc :: [{binary(), iodata()}],
    current_stateful_parent_id :: arizona_stateful:id() | undefined,
    current_stateful_id :: arizona_stateful:id(),
    stateful_states :: #{arizona_stateful:id() => arizona_stateful:stateful()},
    temp_bindings :: map()  % For stateless component bindings, always a map
}).
-opaque socket() :: #socket{}.
-export_type([socket/0]).

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

get_mode(#socket{} = Socket) ->
    Socket#socket.mode.

get_current_stateful_id(#socket{} = Socket) ->
    Socket#socket.current_stateful_id.

get_current_stateful_state(#socket{} = Socket) ->
    Id = get_current_stateful_id(Socket),
    get_stateful_state(Id, Socket).

get_stateful_state(Id, #socket{} = Socket) ->
    maps:get(Id, Socket#socket.stateful_states).

get_stateful_states(#socket{} = Socket) ->
    Socket#socket.stateful_states.

find_stateful_state(Id, #socket{} = Socket) ->
    maps:find(Id, Socket#socket.stateful_states).

set_current_stateful_id(Id, #socket{} = Socket) when Id =:= root; is_binary(Id) ->
    Socket#socket{current_stateful_id = Id}.

set_html_acc(Html, #socket{} = Socket) when is_list(Html) ->
    Socket#socket{html_acc = Html}.

get_html(#socket{} = Socket) ->
    Socket#socket.html_acc.

put_stateful_state(Id, State, Socket) when Id =:= root; is_binary(Id) ->
    States = Socket#socket.stateful_states,
    Socket#socket{stateful_states = States#{Id => State}}.

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
with_temp_bindings(Bindings, #socket{} = Socket) when is_map(Bindings) ->
    Socket#socket{temp_bindings = Bindings}.

get_temp_binding(Key, #socket{} = Socket) when is_atom(Key) ->
    case Socket#socket.temp_bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.
