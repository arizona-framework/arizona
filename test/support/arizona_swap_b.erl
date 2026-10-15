-module(arizona_swap_b).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_info/2]).

%% Embeddable child that arizona_swap_parent swaps in at the id arizona_swap_a
%% held. Reports every message it receives, and arms its own timer at mount: that
%% one is registered under the same id and must survive the swap.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    ?send_after(maps:get(id, Props), 50, b_tick),
    {#{id => maps:get(id, Props), notify => maps:get(notify, Props)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"B"]}).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(Msg, #{notify := Notify} = Bindings) ->
    Notify ! {b_got, Msg},
    {Bindings, #{}, []}.
