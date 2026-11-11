-module(arizona_pip_parent).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% A parent whose re-render swaps the wrapper around a stateful child, so the
%% child's slot is rendered wholesale: a popped-out child's inline placeholder goes
%% with it and the child renders inline again.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"pip_parent"),
            wrap => section
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{id, ~"swap"}, {az_click, arizona_js:push_event(~"swap")}], [~"Swap"]},
            case ?get(wrap) of
                section -> {section, [], [?stateful(arizona_pip, #{id => ~"pip_child"})]};
                article -> {article, [], [?stateful(arizona_pip, #{id => ~"pip_child"})]}
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"swap", _Payload, Bindings) ->
    Wrap =
        case maps:get(wrap, Bindings) of
            section -> article;
            article -> section
        end,
    {Bindings#{wrap => Wrap}, #{}, []}.
