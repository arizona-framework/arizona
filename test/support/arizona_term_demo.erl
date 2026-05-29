-module(arizona_term_demo).
-include("arizona_view.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

%% A terminal (?terminal) demo view exercising the three real-time paths:
%%
%%   * key-driven state -- the driver turns each keypress into a ~"key" event
%%     (`j`/`k` move the menu selection, `+`/`-` change a counter);
%%   * timer ticks -- a self-scheduled `?send_after` advances a server clock with
%%     no user input (the first tick is armed from `handle_info(arizona_connected)`
%%     so it only runs in a connected live process);
%%   * pubsub broadcasts -- each message delivered on the `demo` channel is emitted
%%     as an `arizona_tty:log/1` effect, which the runtime streams into the
%%     terminal's scrolling log (above the pinned status block).
%%
%% `render/1` is the *status block* that the runtime redraws in place. Broadcasts
%% are append-only log output (the user scrolls back through them), so they are
%% not part of the rendered frame.

-define(TICK_MS, 1000).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"term_demo",
        selected => 0,
        items => [~"New Game", ~"Options", ~"Quit"],
        keybindings => [
            {~"j/k", ~"move", ~"  "},
            {~"enter", ~"select", ~"  "},
            {~"+/-", ~"count", ~"  "},
            {~"q", ~"quit", ~""}
        ],
        count => maps:get(count, Init, 0),
        clock => 0,
        %% Terminal size: a network transport (SSH) supplies it via pty-req /
        %% window-change; a local TTY can't read it, so it stays 0.
        cols => maps:get(term_cols, Init, 0),
        rows => maps:get(term_rows, Init, 0)
    },
    ok = arizona_pubsub:subscribe(demo, self()),
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
            {line, [], [~"Count: ", ?get(count), ~"   Server ticks: ", ?get(clock)]},
            {line, [dim], [~"Terminal: ", ?get(cols), ~"x", ?get(rows)]},
            {line, [dim], [
                ?each(
                    fun({Keys, Label, Sep}) ->
                        <<"[", Keys/binary, "] ", Label/binary, Sep/binary>>
                    end,
                    ?get(keybindings)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"key", #{~"key" := ~"enter"}, Bindings) ->
    activate(Bindings);
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
    %% Append-only: stream the message into the scrolling log, no status change.
    {Bindings, #{}, [arizona_tty:log(Msg)]};
handle_info({term_resize, Rows, Cols}, Bindings) ->
    %% A network transport reported a terminal resize; reflect the new size.
    {Bindings#{rows => Rows, cols => Cols}, #{}, []}.

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

%% Confirm the highlighted menu item: "Quit" stops the runtime; the others log
%% the selection into the scrolling feed.
activate(Bindings) ->
    Items = maps:get(items, Bindings),
    Selected = maps:get(selected, Bindings),
    activate_item(lists:nth(Selected + 1, Items), Bindings).

activate_item(~"Quit", Bindings) ->
    {Bindings, #{}, [arizona_tty:quit()]};
activate_item(Item, Bindings) ->
    {Bindings, #{}, [arizona_tty:log(<<"selected ", Item/binary>>)]}.

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
