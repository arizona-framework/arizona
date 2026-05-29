-module(arizona_term_demo).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3, handle_info/2]).

%% A terminal (?terminal) demo view exercising all three real-time paths:
%%
%%   * key-driven state -- the driver turns each keypress into a ~"key" event
%%     (`j`/`k` move the menu selection, `+`/`-` change a counter);
%%   * timer ticks -- a self-scheduled `?send_after` advances a server clock with
%%     no user input (the first tick is armed from `handle_info(arizona_connected)`
%%     so it only runs in a connected live process);
%%   * pubsub broadcasts -- the view subscribes to the `demo` channel at mount and
%%     prepends each delivered message, so any process broadcasting to `demo`
%%     repaints every subscribed terminal.
%%
%% Lists are pre-formatted into binaries in `format_menu/2` and `format_messages/1`
%% (the terminal target has no `?each`), rendered as dynamic children.

-define(TICK_MS, 1000).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Init, _Req) ->
    Bindings = #{
        id => ~"term_demo",
        selected => 0,
        items => [~"New Game", ~"Options", ~"Quit"],
        count => maps:get(count, Init, 0),
        clock => 0,
        messages => []
    },
    ok = arizona_pubsub:subscribe(demo, self()),
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?terminal(
        {col, [], [
            {line, [bold, green], [~"== Arizona Terminal Demo =="]},
            {text, [], [format_menu(?get(selected), ?get(items))]},
            {line, [], [~"Count: ", ?get(count)]},
            {line, [dim], [~"Server ticks: ", ?get(clock)]},
            {line, [bold], [~"Messages:"]},
            {text, [], [format_messages(?get(messages))]},
            {line, [dim], [~"[j/k] move  [+/-] count  [q] quit"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"key", #{~"key" := Key}, Bindings) ->
    {apply_key(Key, Bindings), #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(?TICK_MS, tick),
    {Bindings, #{}, []};
handle_info(tick, Bindings) ->
    ?send_after(?TICK_MS, tick),
    {Bindings#{clock => maps:get(clock, Bindings) + 1}, #{}, []};
handle_info({chat, Msg}, Bindings) ->
    Messages = maps:get(messages, Bindings),
    {Bindings#{messages => [Msg | Messages]}, #{}, []}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

apply_key(~"j", Bindings) -> move(Bindings, 1);
apply_key(~"down", Bindings) -> move(Bindings, 1);
apply_key(~"k", Bindings) -> move(Bindings, -1);
apply_key(~"up", Bindings) -> move(Bindings, -1);
apply_key(~"+", Bindings) -> Bindings#{count => maps:get(count, Bindings) + 1};
apply_key(~"-", Bindings) -> Bindings#{count => maps:get(count, Bindings) - 1};
apply_key(_Other, Bindings) -> Bindings.

move(Bindings, Delta) ->
    Selected = maps:get(selected, Bindings),
    Max = length(maps:get(items, Bindings)) - 1,
    Bindings#{selected => max(0, min(Max, Selected + Delta))}.

format_menu(Selected, Items) ->
    iolist_to_binary([menu_line(I, Selected, Item) || {I, Item} <- lists:enumerate(0, Items)]).

menu_line(Idx, Idx, Item) -> [~"> ", Item, ~"\n"];
menu_line(_Idx, _Selected, Item) -> [~"  ", Item, ~"\n"].

format_messages([]) ->
    ~"  (no messages yet)\n";
format_messages(Messages) ->
    iolist_to_binary([[~"  ", Msg, ~"\n"] || Msg <- Messages]).
