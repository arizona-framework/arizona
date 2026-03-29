-module(arizona_crashable).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

mount(Bindings) ->
    case maps:get(crash_on_mount, Bindings, false) of
        true -> error(crash_on_mount);
        false -> {maps:merge(#{id => ~"crashable", status => ~"ok"}, Bindings), #{}}
    end.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [~"Status: ", ?get(status, ~"ok")]},
            {p, [], [~"Locale: ", ?get(<<"locale">>, ~"none")]}
        ]}
    ).

handle_event(~"crash", _Payload, _Bindings) ->
    error(crash_on_event);
handle_event(~"crash_async", _Payload, Bindings) ->
    self() ! crash,
    {Bindings, #{}, []};
handle_event(~"set_status", #{~"value" := V}, Bindings) ->
    {Bindings#{status => V}, #{}, []};
handle_event(~"with_effect", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:dispatch_event(~"test_effect", #{~"ok" => true})]}.

handle_info(crash, _Bindings) ->
    error(crash_on_info);
handle_info(_Info, Bindings) ->
    {Bindings, #{}, []}.
