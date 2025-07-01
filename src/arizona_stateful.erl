-module(arizona_stateful).

-export([call_mount_callback/2]).
-export([call_unmount_callback/2]).
-export([call_render_callback/2]).

-export([new/3]).
-export([get_module/1]).
-export([get_binding/2]).
-export([put_binding/3]).
-export([put_bindings/2]).

-export([should_remount/1]).

-record(stateful, {
    id :: id(),
    module :: module(),
    bindings :: map(),
    changed_bindings :: map(),  % Track which bindings changed
    %last_html :: iolist(),
    %last_elements :: map(),  % Track last rendered elements #{index => content}
    %template_data :: map(),  % Store parsed template structure
    fingerprint :: arizona_fingerprint:fingerprint()
}).
-opaque stateful() :: #stateful{}.
-export_type([stateful/0]).

-type id() :: root | binary().
-export_type([id/0]).

call_mount_callback(Mod, Socket) when is_atom(Mod) ->
    apply(Mod, mount, [Socket]).

call_unmount_callback(Mod, Socket) when is_atom(Mod) ->
    case erlang:function_exported(Mod, unmount, 1) of
        true ->
            apply(Mod, unmount, [Socket]);
        false ->
            Socket
    end.

call_render_callback(Mod, Socket) when is_atom(Mod) ->
    apply(Mod, render, [Socket]).

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

get_module(#stateful{} = State) ->
    State#stateful.module.

get_binding(Key, #stateful{} = State) when is_atom(Key) ->
    maps:get(Key, State#stateful.bindings).

put_binding(Key, Value, #stateful{} = State) when is_atom(Key) ->
    case State#stateful.bindings of
        #{Key := Value} ->
            State;
        Bindings ->
            ChangedBindings = State#stateful.changed_bindings,
            State#stateful{
              bindings = Bindings#{Key => Value},
              changed_bindings = ChangedBindings#{Key => Value}
            }
    end.

put_bindings(Bindings, #stateful{} = State) when is_map(Bindings) ->
    maps:fold(fun put_binding/3, State, Bindings).

should_remount(#stateful{} = State) ->
    case State#stateful.fingerprint of
        undefined ->
            true;
        OldFingerprint ->
            NewFingerprint = generate_fingerprint(State),
            not arizona_fingerprint:match(OldFingerprint, NewFingerprint)
    end.
