-module(arizona_nested_grandchild_stateful).
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
            GRANDCHILD STATEFUL (id: {arizona_template:get_binding(id, Bindings)})
        </div>
        <div
            class="counter"
            data-testid="grandchild-stateful-counter-{arizona_template:get_binding(id, Bindings)}"
        >
            Counter: {arizona_template:get_binding(counter, Bindings)}
        </div>
        <button
            data-testid="grandchild-increment-{arizona_template:get_binding(id, Bindings)}"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment')"
        >
            Increment Grandchild
        </button>
        <button
            data-testid="grandchild-notify-parent-{arizona_template:get_binding(parent_id, Bindings)}"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'notify_parent')"
        >
            Notify Parent
        </button>
    </div>
    """).

%% Event handlers
handle_event(~"increment", _Params, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    {[], arizona_stateful:put_binding(counter, Counter + 1, State)};
handle_event(~"notify_parent", _Params, State) ->
    ParentId = arizona_stateful:get_binding(parent_id, State),
    Counter = arizona_stateful:get_binding(counter, State),
    ok = arizona_live:handle_event(self(), ParentId, ~"child_notification", #{
        ~"value" => Counter
    }),
    {[], State};
handle_event(~"receive_from_parent", #{~"value" := Value}, State) ->
    {[], arizona_stateful:put_binding(counter, Value, State)}.
