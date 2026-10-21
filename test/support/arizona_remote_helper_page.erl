-module(arizona_remote_helper_page).
-moduledoc """
A live page whose only `az-*` markup comes from a remote helper module's
template, spliced into a content slot.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> {az:bindings(), az:resets()}.
mount(_Bindings) ->
    {#{id => ~"remote_helper_page"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [arizona_remote_helper:button()]}).
