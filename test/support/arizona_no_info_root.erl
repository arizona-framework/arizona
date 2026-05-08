-module(arizona_no_info_root).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

%% View without handle_info/2 -- used to assert arizona_live silently
%% drops inbox messages when the handler doesn't export the callback.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"noinfo"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"x"]}).
