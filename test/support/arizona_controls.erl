-module(arizona_controls).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

%% Server-owned controls: a handler that refuses a value leaves its binding alone, so
%% no op answers the event, and the control must still show what the server rendered.
-spec mount(az:bindings()) -> az:mount_ret().
mount(_Init) ->
    Bindings = #{id => ~"controls", qty => 5, locked => false, note => ~"start"},
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {input, [
                {id, ~"qty"},
                {name, ~"qty"},
                {value, integer_to_binary(?get(qty))},
                {az_change, arizona_js:push_event(~"set_qty")}
            ]},
            {input, [
                {id, ~"locked"},
                {type, ~"checkbox"},
                {checked, ?get(locked)},
                {az_change, arizona_js:push_event(~"toggle_locked")}
            ]},
            {input, [{id, ~"note"}, {value, ?get(note)}]},
            {button, [{id, ~"arm"}, {az_click, arizona_js:push_event(~"arm")}], [~"arm"]},
            {p, [{id, ~"shown"}], [integer_to_binary(?get(qty))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"set_qty", #{~"value" := V}, Bindings) ->
    case string:to_integer(V) of
        {N, <<>>} when N >= 0, N =< 100 -> {Bindings#{qty => N}, #{}, []};
        _Refused -> {Bindings, #{}, []}
    end;
%% The box stays unchecked whatever the user does.
handle_event(~"toggle_locked", _Payload, Bindings) ->
    {Bindings, #{}, []};
%% A server push the user does not trigger, while they type.
handle_event(~"arm", _Payload, Bindings) ->
    _Timer = ?send_after(400, push_note),
    {Bindings, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(push_note, Bindings) ->
    {Bindings#{note => ~"pushed"}, #{}, []}.
