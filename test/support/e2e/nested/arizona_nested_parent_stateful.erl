-module(arizona_nested_parent_stateful).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([render_nested_stateless/1]).
-export([render_deep_stateless/1]).
-export([handle_event/3]).

mount(Bindings) ->
    % Store child component IDs for propagation
    BindingsWithChildren = maps:merge(Bindings, #{
        grandchild_id => ~"grandchild-stateful-1",
        leaf_id => ~"leaf-stateful-1"
    }),
    arizona_stateful:new(?MODULE, BindingsWithChildren).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="component stateful">
        <div class="component-label">
            PARENT STATEFUL (id: {arizona_template:get_binding(id, Bindings)})
        </div>
        <div class="counter" data-testid="parent-stateful-counter">
            Counter: {arizona_template:get_binding(counter, Bindings)}
        </div>
        <button
            data-testid="parent-increment"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'increment')"
        >
            Increment Parent
        </button>
        <button
            data-testid="parent-propagate"
            onclick="arizona.pushEventTo('{arizona_template:get_binding(id, Bindings)}', 'propagate_down')"
        >
            Propagate to Children
        </button>

        <div class="nested">
            {arizona_template:render_stateless(arizona_nested_parent_stateful, render_nested_stateless, #{
                parent_counter => arizona_template:get_binding(counter, Bindings),
                parent_id => arizona_template:get_binding(id, Bindings),
                grandchild_id => arizona_template:get_binding(grandchild_id, Bindings),
                leaf_id => arizona_template:get_binding(leaf_id, Bindings)
            })}
        </div>
    </div>
    """).

%% Nested stateless component inside parent stateful
render_nested_stateless(Bindings) ->
    arizona_template:from_html(~"""
    <div class="component stateless">
        <div class="component-label">STATELESS (from Parent Stateful)</div>
        <div data-testid="nested-stateless-parent-counter">
            Parent Counter: {arizona_template:get_binding(parent_counter, Bindings)}
        </div>

        <div class="nested">
            {arizona_template:render_stateful(arizona_nested_grandchild_stateful, #{
                id => arizona_template:get_binding(grandchild_id, Bindings),
                counter => 0,
                parent_id => arizona_template:get_binding(parent_id, Bindings)
            })}
        </div>

        <div class="nested">
            {arizona_template:render_stateless(arizona_nested_parent_stateful, render_deep_stateless, #{
                parent_counter => arizona_template:get_binding(parent_counter, Bindings),
                leaf_id => arizona_template:get_binding(leaf_id, Bindings)
            })}
        </div>
    </div>
    """).

%% Deep nested stateless component
render_deep_stateless(Bindings) ->
    arizona_template:from_html(~"""
    <div class="component stateless">
        <div class="component-label">DEEP STATELESS (from Nested Stateless)</div>
        <div data-testid="deep-stateless-parent-counter">
            Parent Counter: {arizona_template:get_binding(parent_counter, Bindings)}
        </div>

        <div class="nested">
            {arizona_template:render_stateful(arizona_nested_leaf_stateful, #{
                id => arizona_template:get_binding(leaf_id, Bindings),
                counter => 0
            })}
        </div>
    </div>
    """).

%% Event handlers
handle_event(~"increment", _Params, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    {[], arizona_stateful:put_binding(counter, Counter + 1, State)};
handle_event(~"propagate_down", _Params, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    NewCounter = Counter + 10,

    % Get child component IDs from state
    GrandchildId = arizona_stateful:get_binding(grandchild_id, State),
    LeafId = arizona_stateful:get_binding(leaf_id, State),

    % Propagate to all descendant stateful components
    ok = arizona_live:handle_event(self(), GrandchildId, ~"receive_from_parent", #{
        ~"value" => NewCounter
    }),
    ok = arizona_live:handle_event(self(), LeafId, ~"receive_from_parent", #{
        ~"value" => NewCounter
    }),

    {[], arizona_stateful:put_binding(counter, NewCounter, State)};
handle_event(~"child_notification", #{~"value" := Value}, State) ->
    Counter = arizona_stateful:get_binding(counter, State),
    NewCounter = Counter + Value,
    {[], arizona_stateful:put_binding(counter, NewCounter, State)}.
