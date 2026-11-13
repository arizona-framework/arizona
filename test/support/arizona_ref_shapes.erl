-module(arizona_ref_shapes).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% A view whose events reply with each combination of ops, effects and newly proven
%% `az-*` names, so a suite can check the reply ref splices beside every one.
-spec mount(az:bindings()) -> az:mount_ret().
mount(_Init) ->
    {#{id => ~"shapes", count => 0, cmd => ~""}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{az_probe, ?get(cmd)}], [integer_to_binary(?get(count))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"ops", _Payload, Bindings) ->
    {bump(Bindings), #{}, []};
handle_event(~"effects", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:set_title(~"T")]};
handle_event(~"ops_effects", _Payload, Bindings) ->
    {bump(Bindings), #{}, [arizona_js:set_title(~"T")]};
handle_event(~"names", _Payload, Bindings) ->
    {prove(Bindings), #{}, []};
handle_event(~"all", _Payload, Bindings) ->
    {prove(bump(Bindings)), #{}, [arizona_js:set_title(~"T")]}.

bump(#{count := N} = Bindings) ->
    Bindings#{count => N + 1}.

%% An opaque attribute value that evaluates to a command proves its name at render.
prove(Bindings) ->
    Bindings#{cmd => arizona_js:push_event(~"probe")}.
