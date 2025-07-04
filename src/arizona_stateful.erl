-module(arizona_stateful).

%% Behavior callbacks
-callback mount(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

-callback render(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

-callback unmount(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().

-optional_callbacks([unmount/1]).

-export([call_mount_callback/2]).
-export([call_unmount_callback/2]).
-export([call_render_callback/2]).
-export([call_dynamic_function/2]).

-export([new/3]).
-export([get_module/1, get_id/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([put_binding/3]).
-export([put_bindings/2]).
-export([get_changed_bindings/1]).

-export([should_remount/1]).

-record(state, {
    id :: id(),
    module :: module(),
    bindings :: map(),
    % Track which bindings changed
    changed_bindings :: map(),
    fingerprint :: arizona_fingerprint:fingerprint()
}).
-opaque state() :: #state{}.
-export_type([state/0]).

-type id() :: root | binary().
-export_type([id/0]).

-spec call_mount_callback(Mod, Socket) -> Socket1 when
    Mod :: module(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_mount_callback(Mod, Socket) when is_atom(Mod) ->
    apply(Mod, mount, [Socket]).

-spec call_unmount_callback(Mod, Socket) -> Socket1 when
    Mod :: module(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_unmount_callback(Mod, Socket) when is_atom(Mod) ->
    case erlang:function_exported(Mod, unmount, 1) of
        true ->
            apply(Mod, unmount, [Socket]);
        false ->
            Socket
    end.

-spec call_render_callback(Mod, Socket) -> Socket1 when
    Mod :: module(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_render_callback(Mod, Socket) when is_atom(Mod) ->
    apply(Mod, render, [Socket]).

-spec call_dynamic_function(Fun, Socket) -> Result when
    Fun :: fun((arizona_socket:socket()) -> term()),
    Socket :: arizona_socket:socket(),
    Result :: term().
call_dynamic_function(Fun, Socket) when is_function(Fun, 1) ->
    apply(Fun, [Socket]).

-spec new(Id, Mod, Bindings) -> State when
    Id :: id(),
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    State :: state().
new(Id, Mod, Bindings) when (Id =:= root orelse is_binary(Id)), is_atom(Mod), is_map(Bindings) ->
    #state{
        id = Id,
        module = Mod,
        bindings = Bindings,
        changed_bindings = #{},
        fingerprint = generate_fingerprint(Mod, Bindings)
    }.

generate_fingerprint(#state{} = State) ->
    Mod = State#state.module,
    Bindings = State#state.bindings,
    generate_fingerprint(Mod, Bindings).

generate_fingerprint(Mod, Bindings) ->
    arizona_fingerprint:generate({Mod, Bindings}).

-spec get_module(State) -> Mod when
    State :: state(),
    Mod :: module().
get_module(#state{} = State) ->
    State#state.module.

-spec get_id(State) -> Id when
    State :: state(),
    Id :: id().
get_id(#state{} = State) ->
    State#state.id.

-spec get_binding(Key, State) -> Value when
    Key :: atom(),
    State :: state(),
    Value :: term().
get_binding(Key, #state{} = State) when is_atom(Key) ->
    case State#state.bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.

-spec get_binding(Key, State, Default) -> Value when
    Key :: atom(),
    State :: state(),
    Default :: term(),
    Value :: term() | Default.
get_binding(Key, #state{} = State, Default) when is_atom(Key) ->
    case State#state.bindings of
        #{Key := Value} -> Value;
        #{} -> Default
    end.

-spec put_binding(Key, Value, State) -> State1 when
    Key :: atom(),
    Value :: term(),
    State :: state(),
    State1 :: state().
put_binding(Key, Value, #state{} = State) when is_atom(Key) ->
    case State#state.bindings of
        #{Key := Value} ->
            State;
        Bindings ->
            ChangedBindings = State#state.changed_bindings,
            State#state{
                bindings = Bindings#{Key => Value},
                changed_bindings = ChangedBindings#{Key => Value}
            }
    end.

-spec put_bindings(Bindings, State) -> State1 when
    Bindings :: arizona_socket:bindings(),
    State :: state(),
    State1 :: state().
put_bindings(Bindings, #state{} = State) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, State, Bindings).

-spec get_changed_bindings(State) -> ChangedBindings when
    State :: state(),
    ChangedBindings :: map().
get_changed_bindings(#state{} = State) ->
    State#state.changed_bindings.

-spec should_remount(State) -> ShouldRemount when
    State :: state(),
    ShouldRemount :: boolean().
should_remount(#state{} = State) ->
    OldFingerprint = State#state.fingerprint,
    NewFingerprint = generate_fingerprint(State),
    not arizona_fingerprint:match(OldFingerprint, NewFingerprint).
