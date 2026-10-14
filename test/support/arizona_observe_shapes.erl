-module(arizona_observe_shapes).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Shapes a snapshot must observe exactly as the document render does: a limited
%% stream whose hidden item carries a command (never evaluated, so never declared)
%% and a `?local` attribute whose init is a command (declared).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    Items = [
        #{id => ~"shown", cmd => ~"plain"},
        #{id => ~"hidden", cmd => arizona_js:push_event(~"hidden")}
    ],
    {
        #{
            id => ~"shapes",
            rows => arizona_stream:new(fun(#{id := Id}) -> Id end, Items, #{limit => 1})
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(Row, Key) ->
                        {li, [{az_key, Key}, {az_paste, maps:get(cmd, Row)}], [Key]}
                    end,
                    ?get(rows)
                )
            ]},
            {button, [{az_blur, ?local(~"k", arizona_js:push_event(~"blur"))}], [~"b"]}
        ]}
    ).
