-module(arizona_utf8_rekey_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Fixture: a `?stateful` child whose mount/1 rewrites the restricted `id` key
%% (see arizona_utf8_rekey_page).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Props) ->
    {#{id => ~"rewritten"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({section, [{id, ?get(id)}], [~"child"]}).
