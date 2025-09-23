-module(arizona_realtime_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(_Arg, Req) ->
    case arizona_live:is_connected(self()) of
        true ->
            arizona_pubsub:join(~"time_update", self());
        false ->
            ok
    end,
    Bindings = #{
        id => ~"realtime_view",
        current_time => ~"Loading..."
    },
    Layout =
        {arizona_realtime_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <div class="clock" data-testid="clock">
            {arizona_template:get_binding(current_time, Bindings)}
        </div>
    </div>
    """).

handle_event(~"time_update", Data, View) ->
    % Received time update from the clock process
    NewTime = maps:get(~"time", Data),
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(current_time, NewTime, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView}.
