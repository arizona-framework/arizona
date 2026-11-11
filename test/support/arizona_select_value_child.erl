-module(arizona_select_value_child).
-moduledoc """
A child view rendering options for a select in its parent
(`arizona_select_value_page`): the parent's value names one of them, and the parent
can hand it one more, so the e2e can check an option that appears after the value
was set is the one selected.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    {#{id => maps:get(id, Init), opts => maps:get(opts, Init)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {optgroup, [{id, ?get(id)}, {label, ~"Child"}], [
            ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts))
        ]}
    ).
