-module(arizona_nested_leaf_stateful).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div
        id="{arizona_template:get_binding(id, Bindings)}"
        class="component stateful"
    >
        <div class="component-label">
            LEAF STATEFUL (id: {arizona_template:get_binding(id, Bindings)})
        </div>
        <div class="counter" data-testid="leaf-stateful-counter">
            Counter: {arizona_template:get_binding(counter, Bindings)}
        </div>
        <button
            data-testid="leaf-increment"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment')"
        >
            Increment Leaf
        </button>
    </div>
    """).

%% Event handlers
handle_event(~"increment", _Params, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    {[], arizona_stateful:put_binding(counter, Counter + 1, State)};
handle_event(~"receive_from_parent", #{~"value" := Value}, State) ->
    {[], arizona_stateful:put_binding(counter, Value, State)}.
