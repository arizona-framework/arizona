-module(arizona_json_toggle).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A JSON view with a dynamic and a static boolean prop, for comparing the
%% request-free document with the client's view of the live first frame.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"json_toggle", on => maps:get(on, Bindings, false)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            {'Button', [{enabled, ?get(on)}, {visible, false}], [~"t"]}
        ]}
    ).
