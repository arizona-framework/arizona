-module(arizona_hash_patch).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).

%% A page whose patch reaction answers with a navigate to a fragment of the page
%% it now shows: the reply put that page on screen, the effect only scrolls it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Params = maps:get(params, Bindings, []),
    {#{id => ~"hash_patch", section => proplists:get_value(~"s", Params, ~"one")}, #{}}.

-spec handle_update(map(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Params, Bindings, Effects) ->
    Section = proplists:get_value(~"s", maps:get(params, Params, []), ~"one"),
    {Bindings#{section => Section}, #{}, [arizona_js:navigate(~"#bottom") | Effects]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [{id, ~"sec"}], [?get(section)]},
            {'div', [{id, ~"bottom"}, {style, ~"margin-top:3000px"}], [~"bottom"]}
        ]}
    ).
