-module(arizona_nested_child_stateful).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([render_nested_stateless/1]).
-export([handle_event/3]).

mount(Bindings) ->
    % Store grandchild ID for propagation
    BindingsWithChildren = maps:merge(Bindings, #{
        grandchild_id => ~"grandchild-stateful-2"
    }),
    arizona_stateful:new(?MODULE, BindingsWithChildren).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="component stateful">
        <div class="component-label">
            CHILD STATEFUL (id: {arizona_template:get_binding(id, Bindings)})
        </div>
        <div class="counter" data-testid="child-stateful-counter">
            Counter: {arizona_template:get_binding(counter, Bindings)}
        </div>
        <button
            data-testid="child-increment"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment')"
        >
            Increment Child
        </button>
        <button
            data-testid="child-notify-parent"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'notify_parent')"
        >
            Notify Parent
        </button>

        <div class="nested">
            {arizona_template:render_stateless(arizona_nested_child_stateful, render_nested_stateless, #{
                child_counter => arizona_template:get_binding(counter, Bindings)
            })}
        </div>

        <div class="nested">
            {arizona_template:render_stateful(arizona_nested_grandchild_stateful, #{
                id => arizona_template:get_binding(grandchild_id, Bindings),
                counter => 0,
                parent_id => arizona_template:get_binding(id, Bindings)
            })}
        </div>
    </div>
    """).

%% Nested stateless component
render_nested_stateless(Bindings) ->
    arizona_template:from_html(~"""
    <div class="component stateless">
        <div class="component-label">STATELESS (from Child Stateful)</div>
        <div data-testid="child-nested-stateless-counter">
            Child Counter: {arizona_template:get_binding(child_counter, Bindings)}
        </div>
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
    {[], arizona_stateful:put_binding(counter, Value, State)};
handle_event(~"child_notification", #{~"value" := Value}, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    NewCounter = Counter + Value,
    {[], arizona_stateful:put_binding(counter, NewCounter, State)}.
