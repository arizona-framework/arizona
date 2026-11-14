-module(arizona_static_raw_attr_page).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"raw_attr_page",
            x => maps:get(x, Bindings, ~"v"),
            h => maps:get(h, Bindings, ~"c\"d")
        },
        #{}
    }.

%% A `"` in a literal `?raw` attribute value, as a part and whole from a binding.
-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {title, [?raw(~"a\"b"), ?get(x)]}, {'data-r', ?raw(?get(h))}], []}
    ).
