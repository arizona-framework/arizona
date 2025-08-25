-module(arizona_counter_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

mount(Req) ->
    case arizona_live:is_connected(self()) of
        true ->
            arizona_pubsub:join(~"increment", self());
        false ->
            ok
    end,
    Bindings = #{
        id => ~"view",
        counter_id => ~"counter"
    },
    Layout =
        {arizona_counter_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        {arizona_template:render_stateful(arizona_counter_stateful, #{
            id => arizona_template:get_binding(counter_id, Bindings),
            count => 0
        })}
    </div>
    """).

handle_event(~"increment", Params, View) ->
    State = arizona_view:get_state(View),
    CounterId = arizona_stateful:get_binding(counter_id, State),
    ok = arizona_live:handle_event(self(), CounterId, ~"increment", Params),
    {noreply, View}.
