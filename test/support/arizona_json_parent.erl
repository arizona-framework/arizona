-module(arizona_json_parent).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% JSON parent view embedding a JSON stateful child, for testing
%% nested stateful rendering through the live mount_and_render frame path.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"json_parent", count => maps:get(count, Bindings, 5)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            ?stateful(arizona_json_badge, #{id => ~"badge", count => ?get(count)})
        ]}
    ).
