-module(arizona_doc_tail_page).
%% A live page that renders the whole document itself, `<body>` included.
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"doc_tail_page", count => maps:get(count, Bindings, 0)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {html, [{id, ?get(id)}], [
            {body, [], [
                {button, [{az_click, arizona_js:push_event(~"inc")}], [?get(count)]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) -> az:handle_event_ret().
handle_event(~"inc", _Params, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.
