-module(arizona_term_demo).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

%% A terminal (?terminal) demo view exercising the three real-time paths:
%%
%%   * key-driven state -- the driver turns each keypress into a ~"key" event
%%     (`j`/`k` move the menu selection, `+`/`-` change a counter);
%%   * timer ticks -- a self-scheduled `?send_after` advances a server clock with
%%     no user input (the first tick is armed from `handle_info(arizona_connected)`
%%     so it only runs in a connected live process);
%%   * presence -- `arizona_pubsub:monitor/1` notifies the view the instant a
%%     terminal joins or leaves the `demo` channel, so the connected-client count
%%     updates live (event-driven, no polling);
%%   * pubsub broadcasts -- each message delivered on the `demo` channel is emitted
%%     as an `arizona_term_demo_effects:log/1` effect, which the runtime streams into the
%%     terminal's scrolling log (above the pinned status block). The "Send message"
%%     menu item opens a text input that broadcasts on `demo`, so a message typed in
%%     one terminal appears in every connected terminal's log (the sender's too).
%%
%% `render/1` is the *status block* that the runtime redraws in place. Broadcasts
%% are append-only log output (the user scrolls back through them), so they are
%% not part of the rendered frame.

-define(TICK_MS, 1000).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    %% Subscribe, then monitor membership for a live client count. Subscribe first
    %% so this client is in the monitor's initial member snapshot.
    ok = arizona_pubsub:subscribe(demo, self()),
    {_Ref, Members} = arizona_pubsub:monitor(demo),
    Bindings = #{
        id => ~"term_demo",
        selected => 0,
        items => [~"New Game", ~"Options", ~"Send message", ~"Quit"],
        %% menu = navigate the list; input = typing a message to broadcast.
        mode => menu,
        draft => <<>>,
        count => maps:get(count, Init, 0),
        clock => 0,
        clients => length(Members),
        %% Terminal size: a network transport (SSH) supplies it via pty-req /
        %% window-change; a local TTY can't read it, so it stays 0.
        cols => maps:get(term_cols, Init, 0),
        rows => maps:get(term_rows, Init, 0)
    },
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?terminal(
        {col, [], [
            {line, [bold, green], [~"== Arizona Terminal Demo =="]},
            ?each(
                fun({Marker, Label}) -> {line, [], [Marker, Label]} end,
                marked_items(?get(selected), ?get(items))
            ),
            {line, [], [
                ~"Count: ",
                ?get(count),
                ~"   Server ticks: ",
                ?get(clock),
                ~"   Clients: ",
                ?get(clients)
            ]},
            {line, [dim], [~"Terminal: ", ?get(cols), ~"x", ?get(rows)]},
            %% The message input line, shown only while typing. ?each over a
            %% 0-or-1 element list -- a bare element tuple returned from a case
            %% would reach the diff as a dynamic value and crash to_bin/1.
            ?each(
                fun(Draft) -> {line, [yellow], [~"Message: ", Draft]} end,
                input_rows(?get(mode), ?get(draft))
            ),
            {line, [dim], [
                ?each(
                    fun({Keys, Label, Sep}) ->
                        <<"[", Keys/binary, "] ", Label/binary, Sep/binary>>
                    end,
                    footer_keys(?get(mode))
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"key", #{~"key" := Key}, Bindings) ->
    on_key(maps:get(mode, Bindings), Key, Bindings).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(?TICK_MS, tick),
    {Bindings, #{}, []};
handle_info(tick, Bindings) ->
    ?send_after(?TICK_MS, tick),
    {Bindings#{clock => maps:get(clock, Bindings) + 1}, #{}, []};
handle_info({chat, Msg}, Bindings) ->
    %% Append-only: stream the message into the scrolling log, no status change.
    {Bindings, #{}, [arizona_term_demo_effects:log(Msg)]};
handle_info({term_resize, Rows, Cols}, Bindings) ->
    %% A network transport reported a terminal resize; reflect the new size.
    {Bindings#{rows => Rows, cols => Cols}, #{}, []};
handle_info({_Ref, Verb, demo, _Pids}, Bindings) when Verb =:= join; Verb =:= leave ->
    %% A pg membership change on the demo channel -- refresh the live client count.
    {Bindings#{clients => count_clients()}, #{}, []}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Menu mode navigates the list and acts on the selection; `q` quits (the session
%% no longer intercepts it). Input mode builds a draft message: a printable key
%% appends, backspace deletes, enter broadcasts, esc cancels.
on_key(menu, ~"enter", Bindings) ->
    activate(Bindings);
on_key(menu, ~"q", Bindings) ->
    {Bindings, #{}, [arizona_term_demo_effects:quit()]};
on_key(menu, Key, Bindings) ->
    {apply_key(Key, Bindings), #{}, []};
on_key(input, ~"enter", Bindings) ->
    send_message(Bindings);
on_key(input, ~"esc", Bindings) ->
    {Bindings#{mode => menu, draft => <<>>}, #{}, []};
on_key(input, ~"backspace", Bindings) ->
    {Bindings#{draft => drop_last(maps:get(draft, Bindings))}, #{}, []};
on_key(input, <<Char>>, Bindings) ->
    {Bindings#{draft => <<(maps:get(draft, Bindings))/binary, Char>>}, #{}, []};
on_key(input, _Key, Bindings) ->
    {Bindings, #{}, []}.

apply_key(~"j", Bindings) -> move(Bindings, 1);
apply_key(~"down", Bindings) -> move(Bindings, 1);
apply_key(~"k", Bindings) -> move(Bindings, -1);
apply_key(~"up", Bindings) -> move(Bindings, -1);
apply_key(~"+", Bindings) -> Bindings#{count => maps:get(count, Bindings) + 1};
apply_key(~"-", Bindings) -> Bindings#{count => maps:get(count, Bindings) - 1};
apply_key(_Other, Bindings) -> Bindings.

%% Confirm the highlighted menu item: "Quit" stops the runtime; the others log
%% the selection into the scrolling feed.
activate(Bindings) ->
    Items = maps:get(items, Bindings),
    Selected = maps:get(selected, Bindings),
    activate_item(lists:nth(Selected + 1, Items), Bindings).

activate_item(~"Send message", Bindings) ->
    {Bindings#{mode => input, draft => <<>>}, #{}, []};
activate_item(~"Quit", Bindings) ->
    {Bindings, #{}, [arizona_term_demo_effects:quit()]};
activate_item(Item, Bindings) ->
    {Bindings, #{}, [arizona_term_demo_effects:log(<<"selected ", Item/binary>>)]}.

%% Broadcast the typed message to every terminal subscribed to `demo` (including
%% this one, which receives it back via handle_info/2 and logs it), then return
%% to the menu. An empty draft is a no-op.
send_message(Bindings) ->
    ok = broadcast_draft(maps:get(draft, Bindings)),
    {Bindings#{mode => menu, draft => <<>>}, #{}, []}.

broadcast_draft(<<>>) ->
    ok;
broadcast_draft(Draft) ->
    arizona_pubsub:broadcast(demo, {chat, Draft}).

drop_last(<<>>) -> <<>>;
drop_last(Bin) -> binary:part(Bin, 0, byte_size(Bin) - 1).

%% Footer keybinding hints per mode (rendered via ?each, the data-driven footer).
footer_keys(menu) ->
    [
        {~"j/k", ~"move", ~"  "},
        {~"enter", ~"select", ~"  "},
        {~"+/-", ~"count", ~"  "},
        {~"q", ~"quit", ~""}
    ];
footer_keys(input) ->
    [
        {~"enter", ~"send", ~"  "},
        {~"esc", ~"cancel", ~""}
    ].

%% A 0-or-1 element list driving the ?each that renders the input line: present
%% (with the draft) only in input mode.
input_rows(input, Draft) -> [Draft];
input_rows(menu, _Draft) -> [].

%% How many terminals are connected: every connected view subscribes to `demo`,
%% so the subscriber count is the live client count.
count_clients() ->
    length(arizona_pubsub:subscribers(demo)).

move(Bindings, Delta) ->
    Selected = maps:get(selected, Bindings),
    Max = length(maps:get(items, Bindings)) - 1,
    Bindings#{selected => max(0, min(Max, Selected + Delta))}.

%% One {Marker, Label} per item, the selected row marked with "> ". Rendered with
%% ?each in the template; the marker is computed here (not inside the each fun) so
%% the fun does not read bindings.
marked_items(Selected, Items) ->
    [{marker(I, Selected), Item} || {I, Item} <- lists:enumerate(0, Items)].

marker(Idx, Idx) -> ~"> ";
marker(_Idx, _Selected) -> ~"  ".
