-module(arizona_patch_view).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_update/3]).

%% A route-level view that reacts to a `patch` (in-place navigation) via
%% handle_update/3: it sets `section` from the delivered params and emits a
%% set_title effect -- exercising the patch reaction + effects-on-patch path.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"patchview", section => maps:get(section, Bindings, ~"home")}, #{}}.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Params, Bindings, Effects) ->
    Section = maps:get(section, Params, maps:get(section, Bindings)),
    {Bindings#{section => Section}, #{}, [arizona_js:set_title(Section) | Effects]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({main, [{id, ?get(id)}], [?get(section)]}).
