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
-export([put_binding/3]).
-export([put_bindings/2]).

-export([should_remount/1]).

-record(stateful, {
    id :: id(),
    module :: module(),
    bindings :: map(),
    % Track which bindings changed
    changed_bindings :: map(),
    %last_html :: iolist(),
    %last_elements :: map(),  % Track last rendered elements #{index => content}
    %template_data :: map(),  % Store parsed template structure
    fingerprint :: arizona_fingerprint:fingerprint()
}).
-opaque stateful() :: #stateful{}.
-export_type([stateful/0]).

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
call_dynamic_function(Fun, Socket) ->
    apply(Fun, [Socket]).

-spec new(Id, Mod, Bindings) -> Stateful when
    Id :: id(),
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    Stateful :: stateful().
new(Id, Mod, Bindings) when (Id =:= root orelse is_binary(Id)), is_atom(Mod), is_map(Bindings) ->
    #stateful{
        id = Id,
        module = Mod,
        bindings = Bindings,
        changed_bindings = #{},
        fingerprint = generate_fingerprint(Mod, Bindings)
    }.

generate_fingerprint(#stateful{} = State) ->
    Mod = State#stateful.module,
    Bindings = State#stateful.bindings,
    generate_fingerprint(Mod, Bindings).

generate_fingerprint(Mod, Bindings) ->
    arizona_fingerprint:generate({Mod, Bindings}).

-spec get_module(Stateful) -> Mod when
    Stateful :: stateful(),
    Mod :: module().
get_module(#stateful{} = Stateful) ->
    Stateful#stateful.module.

-spec get_id(Stateful) -> Id when
    Stateful :: stateful(),
    Id :: id().
get_id(#stateful{} = Stateful) ->
    Stateful#stateful.id.

-spec get_binding(Key, Stateful) -> Value when
    Key :: atom(),
    Stateful :: stateful(),
    Value :: term().
get_binding(Key, #stateful{} = Stateful) when is_atom(Key) ->
    case Stateful#stateful.bindings of
        #{Key := Value} -> Value;
        #{} -> throw({binding_not_found, Key})
    end.

-spec put_binding(Key, Value, Stateful) -> Stateful1 when
    Key :: atom(),
    Value :: term(),
    Stateful :: stateful(),
    Stateful1 :: stateful().
put_binding(Key, Value, #stateful{} = Stateful) when is_atom(Key) ->
    case Stateful#stateful.bindings of
        #{Key := Value} ->
            Stateful;
        Bindings ->
            ChangedBindings = Stateful#stateful.changed_bindings,
            Stateful#stateful{
                bindings = Bindings#{Key => Value},
                changed_bindings = ChangedBindings#{Key => Value}
            }
    end.

-spec put_bindings(Bindings, Stateful) -> Stateful1 when
    Bindings :: arizona_socket:bindings(),
    Stateful :: stateful(),
    Stateful1 :: stateful().
put_bindings(Bindings, #stateful{} = Stateful) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, Stateful, Bindings).

-spec should_remount(Stateful) -> ShouldRemount when
    Stateful :: stateful(),
    ShouldRemount :: boolean().
should_remount(#stateful{} = Stateful) ->
    case Stateful#stateful.fingerprint of
        undefined ->
            true;
        OldFingerprint ->
            NewFingerprint = generate_fingerprint(Stateful),
            not arizona_fingerprint:match(OldFingerprint, NewFingerprint)
    end.
