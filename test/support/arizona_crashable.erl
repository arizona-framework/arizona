-module(arizona_crashable).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    case maps:get(crash_on_mount, Bindings, false) of
        true ->
            error(crash_on_mount);
        false ->
            Params = maps:get(params, Bindings, []),
            Locale = proplists:get_value(~"locale", Params, ~"none"),
            {
                #{
                    id => ~"crashable",
                    status => maps:get(status, Bindings, ~"ok"),
                    <<"locale">> => Locale,
                    <<"item_id">> => maps:get(<<"item_id">>, Bindings, ~"none")
                },
                #{}
            }
    end.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [~"Status: ", ?get(status, ~"ok")]},
            {p, [], [~"Locale: ", ?get(<<"locale">>, ~"none")]},
            {p, [], [~"Item: ", ?get(<<"item_id">>, ~"none")]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"crash", _Payload, _Bindings) ->
    error(crash_on_event);
handle_event(~"crash_async", _Payload, Bindings) ->
    self() ! crash,
    {Bindings, #{}, []};
handle_event(~"set_status", #{~"value" := V}, Bindings) ->
    {Bindings#{status => V}, #{}, []};
handle_event(~"with_effect", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:dispatch_event(~"test_effect", #{~"ok" => true})]};
handle_event(~"flash_navigate", _Payload, Bindings) ->
    %% In-view Post/Redirect/Get: set a flash and navigate. The socket carries the
    %% flash in-process to the target route (arizona_js:navigate/2 `flash` opt).
    Effect = arizona_js:navigate(~"/show_flash", #{flash => #{~"error" => ~"from_handler"}}),
    {Bindings, #{}, [Effect]};
handle_event(~"flash_patch", _Payload, Bindings) ->
    %% Same as flash_navigate but via an in-place patch: the flash rides the socket
    %% to the patched route (arizona_js:patch/2 `flash` opt), stripped from the effect.
    Effect = arizona_js:patch(~"/show_flash", #{flash => #{~"error" => ~"from_patch"}}),
    {Bindings, #{}, [Effect]}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(crash, _Bindings) ->
    error(crash_on_info);
handle_info(_Info, Bindings) ->
    {Bindings, #{}, []}.
