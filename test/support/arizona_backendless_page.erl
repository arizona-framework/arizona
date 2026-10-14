-module(arizona_backendless_page).
-behaviour(arizona_stateful).
-export([mount/1]).
-export([render/1]).

%% A page whose render returns a hand-built template: no parse transform, so no
%% backend is recorded. Drives the backendless side of the layout target check.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"backendless"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    #{s => [~"<p id=\"backendless\">plain</p>"], d => [], f => ~"backendless"}.
