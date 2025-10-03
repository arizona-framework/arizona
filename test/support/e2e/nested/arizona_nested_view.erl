-module(arizona_nested_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([render_stateless_wrapper/1]).
-export([handle_event/3]).

mount(_Arg, Req) ->
    Layout =
        {arizona_nested_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"nested-view",
            view_counter => 0,
            parent_stateful_id => ~"parent-stateful",
            child_stateful_id => ~"child-stateful-1"
        },
        Layout
    ).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="component view">
        <div class="component-label">VIEW (id: {arizona_template:get_binding(id, Bindings)})</div>
        <div class="counter" data-testid="view-counter">
            View Counter: {arizona_template:get_binding(view_counter, Bindings)}
        </div>
        <button data-testid="view-increment" onclick="arizona.pushEvent('increment_view')">
            Increment View
        </button>
        <button data-testid="view-broadcast" onclick="arizona.pushEvent('broadcast_from_view')">
            Broadcast from View
        </button>

        <div class="nested">
            {arizona_template:render_stateless(arizona_nested_view, render_stateless_wrapper, #{
                parent_counter => arizona_template:get_binding(view_counter, Bindings),
                parent_stateful_id => arizona_template:get_binding(id, Bindings),
                child_stateful_id => arizona_template:get_binding(child_stateful_id, Bindings)
            })}
        </div>

        <div class="nested">
            {arizona_template:render_stateful(arizona_nested_parent_stateful, #{
                id => arizona_template:get_binding(parent_stateful_id, Bindings),
                counter => 0
            })}
        </div>
    </div>
    """).

%% Stateless wrapper component
render_stateless_wrapper(Bindings) ->
    arizona_template:from_html(~"""
    <div class="component stateless">
        <div class="component-label">STATELESS (from View)</div>
        <div data-testid="stateless-parent-counter">
            Parent Counter: {arizona_template:get_binding(parent_counter, Bindings)}
        </div>

        <div class="nested">
            {arizona_template:render_stateful(arizona_nested_child_stateful, #{
                id => arizona_template:get_binding(child_stateful_id, Bindings),
                counter => 0,
                parent_id => arizona_template:get_binding(parent_stateful_id, Bindings)
            })}
        </div>
    </div>
    """).

%% Event handlers
handle_event(~"increment_view", _Params, View) ->
    State = arizona_view:get_state(View),
    ViewCounter = arizona_stateful:get_binding(view_counter, State),
    UpdatedState = arizona_stateful:put_binding(view_counter, ViewCounter + 1, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView};
handle_event(~"broadcast_from_view", _Params, View) ->
    State = arizona_view:get_state(View),
    ParentStatefulId = arizona_stateful:get_binding(parent_stateful_id, State),
    ChildStatefulId = arizona_stateful:get_binding(child_stateful_id, State),

    % Trigger propagation in parent stateful (which will propagate to its descendants)
    ok = arizona_live:handle_event(self(), ParentStatefulId, ~"propagate_down", #{}),

    % Also propagate to child stateful (sibling of parent)
    ok = arizona_live:handle_event(self(), ChildStatefulId, ~"receive_from_parent", #{
        ~"value" => 10
    }),

    {[], View};
% Handle notification from child component to update view counter
handle_event(~"child_notification", #{~"value" := Value}, View) ->
    State = arizona_view:get_state(View),
    ViewCounter = arizona_stateful:get_binding(view_counter, State),
    UpdatedState = arizona_stateful:put_binding(view_counter, ViewCounter + Value, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView}.
