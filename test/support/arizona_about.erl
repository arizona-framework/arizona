-module(arizona_about).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

mount(Bindings0) ->
    Bindings = maps:merge(
        #{
            id => ~"page",
            title => ~"About",
            tick => 0,
            tags => [~"erlang", ~"otp", ~"arizona"]
        },
        Bindings0
    ),
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {span, [{az_hook, ~"Tick"}], [?get(tick)]},
            {button, [{az_click, arizona_js:push_event(~"add_tag", #{~"tag" => ~"new"})}], [
                ~"Add Tag"
            ]},
            {ul, [{id, ~"tags"}], [
                ?each(
                    fun(Tag) ->
                        {li, [], [Tag]}
                    end,
                    ?get(tags)
                )
            ]}
        ]}
    ).

handle_event(~"tick_started", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:dispatch_event(~"tick_ack", #{~"status" => ~"ok"})]};
handle_event(~"add_tag", #{~"tag" := Tag}, Bindings) ->
    Tags = maps:get(tags, Bindings),
    {Bindings#{tags => Tags ++ [Tag]}, #{}, []}.

handle_info(arizona_connected, Bindings) ->
    ?send_after(1000, tick),
    {Bindings, #{}, [arizona_js:set_title(maps:get(title, Bindings))]};
handle_info(tick, Bindings) ->
    ?send_after(1000, tick),
    {Bindings#{tick => maps:get(tick, Bindings) + 1}, #{}, []}.
