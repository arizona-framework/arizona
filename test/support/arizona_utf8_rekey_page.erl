-module(arizona_utf8_rekey_page).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Fixture: a page whose `?stateful` child (arizona_utf8_rekey_child) rewrites its
%% restricted `id` in mount/1, so a suite can compare the error the dev-mode render
%% raises with the production one.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"rp"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [?stateful(arizona_utf8_rekey_child, #{id => ~"rc"})]}).
