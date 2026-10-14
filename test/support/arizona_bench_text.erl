-module(arizona_bench_text).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% Bench fixture: a page whose dynamic values are prose in a chosen script, so the
%% render and diff costs of non-ASCII text can be compared against ASCII text of the
%% same shape. `text` picks the base string; every slot derives its value from it.
%% Twelve content slots, four attribute slots and a ten-item list. `next` rewrites
%% four content values and one attribute (the live diff path).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Text = maps:get(text, Init, ~"Quick action, single price, heart"),
    {
        #{
            id => ~"bench_text",
            base => Text,
            n => 0,
            title => Text,
            lead => Text,
            body => [<<Text/binary, " ", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 8)],
            hint => Text,
            items => [<<Text/binary, " #", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10)]
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [{title, ?get(title)}], [?get(title)]},
            {p, [{class, ~"lead"}, {'aria-label', ?get(lead)}], [?get(lead)]},
            {section, [], [
                {p, [], [lists:nth(1, ?get(body))]},
                {p, [], [lists:nth(2, ?get(body))]},
                {p, [], [lists:nth(3, ?get(body))]},
                {p, [], [lists:nth(4, ?get(body))]},
                {p, [], [lists:nth(5, ?get(body))]},
                {p, [], [lists:nth(6, ?get(body))]},
                {p, [], [lists:nth(7, ?get(body))]},
                {p, [], [lists:nth(8, ?get(body))]}
            ]},
            {input, [{placeholder, ?get(hint)}, {alt, ?get(hint)}], []},
            {footer, [], [?get(hint)]},
            {ul, [], [?each(fun(I) -> {li, [], [I]} end, ?get(items))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"next", _Payload, #{base := Base, n := N0, body := Body} = Bindings) ->
    N = N0 + 1,
    Suffix = integer_to_binary(N),
    [_, _, _, _ | Rest] = Body,
    New = [<<Base/binary, " ", Suffix/binary, " ", (integer_to_binary(I))/binary>> || I <- [1, 2, 3, 4]],
    {
        Bindings#{
            n => N,
            title => <<Base/binary, " ", Suffix/binary>>,
            body => New ++ Rest
        },
        #{},
        []
    }.
