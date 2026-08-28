-module(arizona_opaque_layout_page).
-moduledoc """
Pass-through page for `arizona_opaque_layout`: layouts render with the
handler's post-mount bindings, so the opaque command must survive the mount.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    {#{id => ~"page", chrome_cmd => maps:get(chrome_cmd, Init)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({main, [{id, ?get(id)}], [~"page"]}).
