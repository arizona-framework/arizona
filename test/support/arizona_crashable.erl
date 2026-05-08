-module(arizona_crashable).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, Req) ->
    case maps:get(crash_on_mount, Bindings, false) of
        true ->
            error(crash_on_mount);
        false ->
            {Params, _Req1} = arizona_req:params(Req),
            Locale = proplists:get_value(~"locale", Params, ~"none"),
            {
                #{
                    id => ~"crashable",
                    status => maps:get(status, Bindings, ~"ok"),
                    <<"locale">> => maps:get(<<"locale">>, Bindings, Locale),
                    item_id => maps:get(item_id, Bindings, ~"none")
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
            {p, [], [~"Item: ", ?get(item_id, ~"none")]}
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
    {Bindings, #{}, [arizona_js:dispatch_event(~"test_effect", #{~"ok" => true})]}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(crash, _Bindings) ->
    error(crash_on_info);
handle_info(_Info, Bindings) ->
    {Bindings, #{}, []}.
