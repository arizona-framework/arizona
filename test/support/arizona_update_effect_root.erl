-module(arizona_update_effect_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Root embedding arizona_update_effect_parent as a MID child (which itself
%% embeds arizona_update_effect_child as a grandchild). An event/info targeted
%% at the mid changes the grandchild's prop, so its handle_update/3 effects
%% must ride the CHILD-targeted update's reply/push -- the depth-2 topology
%% process_child_change diffs.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"uer"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_update_effect_parent, #{id => ~"uep"})
        ]}
    ).
