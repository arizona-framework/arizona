-module(arizona_connected_bad_value).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Holds a value `to_bin/1` rejects only once connected: SSR renders a plain
%% value, so only a connected render can meet the bad one.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"bad", v => connected_value(?connected)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [{p, [], [?get(v)]}]}).

connected_value(true) -> {a, b};
connected_value(false) -> ~"ssr".
