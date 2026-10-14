-module(arizona_utf8_mount_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Fixture: the `?stateful` child of arizona_utf8_mount_page, reporting its
%% `mount/1` to the `notify` pid in its props.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    maps:get(notify, Props) ! {mounted, ?MODULE},
    {#{id => maps:get(id, Props), text => maps:get(text, Props)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({section, [{id, ?get(id)}], [{em, [], [?get(text)]}]}).
