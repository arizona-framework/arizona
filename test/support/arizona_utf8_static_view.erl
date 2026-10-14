-module(arizona_utf8_static_view).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Fixture: a non-ASCII `<<"...">>` literal with no `/utf8` is Latin-1 bytes, but
%% the parse transform folds one that is a template static as UTF-8 text -- content,
%% attribute value, and the static content of a conditional branch alike -- so this
%% module renders UTF-8 and compiles without the `latin1_binary_literal` warning.
%% A charlist literal is chardata, which renders UTF-8 at runtime.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"u8s", open => maps:get(open, Bindings, false)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [{title, <<"atributo é">>}], [<<"café">>]},
            {p, [{title, "lista é"}], "ação"},
            case ?get(open) of
                true -> {p, [{class, <<"ramo ü">>}], [<<"aberto ü">>]};
                false -> ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"open", _Payload, Bindings) ->
    {Bindings#{open => true}, #{}, []}.
