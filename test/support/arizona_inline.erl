-module(arizona_inline).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"inline",
        count => maps:get(count, Init, 0)
    },
    {Bindings, #{}}.

%% Every read is HOISTED into a body variable rather than written inline in
%% `?html` -- the binding-read inlining feature rewrites each interpolated
%% variable back into its slot so it still tracks `count` and diffs over the
%% wire. Exercises several inlining shapes at once: a hoisted root id, a plain
%% hoisted read, a derived chain (`Doubled`), and a statement-form `case` bind
%% (`Parity`, lifted by normalize_tail_binds). If any regressed to a frozen
%% slot, the corresponding `<span>` would not update on click.
-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    Id = ?get(id),
    Count = ?get(count),
    Doubled = Count * 2,
    case ?get(count) rem 2 of
        0 -> Parity = ~"even";
        _ -> Parity = ~"odd"
    end,
    ?html(
        {'div', [{id, Id}], [
            {button, [{az_click, arizona_js:push_event(~"inc")}], [~"+"]},
            {span, [{class, ~"count"}], [integer_to_binary(Count)]},
            {span, [{class, ~"doubled"}], [integer_to_binary(Doubled)]},
            {span, [{class, ~"parity"}], [Parity]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.
