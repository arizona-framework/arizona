-module(arizona_about).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Init, _Req) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"About"),
        tick => 0,
        tags => [~"erlang", ~"otp", ~"arizona"]
    },
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
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

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"tick_started", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:dispatch_event(~"tick_ack", #{~"status" => ~"ok"})]};
handle_event(~"add_tag", #{~"tag" := Tag}, Bindings) ->
    Tags = maps:get(tags, Bindings),
    {Bindings#{tags => Tags ++ [Tag]}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(1000, tick),
    {Bindings, #{}, [arizona_js:set_title(maps:get(title, Bindings))]};
handle_info(tick, Bindings) ->
    ?send_after(1000, tick),
    {Bindings#{tick => maps:get(tick, Bindings) + 1}, #{}, []}.
