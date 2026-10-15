-module(arizona_swap_a).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_info/2]).
-export([unmount/1]).

%% Embeddable child that arizona_swap_parent swaps out while keeping its id. It
%% arms a timer at mount, which the swap must cancel rather than let it reach the
%% handler mounted in its place.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    ?send_after(maps:get(id, Props), 50, a_tick),
    {#{id => maps:get(id, Props), notify => maps:get(notify, Props)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"A"]}).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(Msg, #{notify := Notify} = Bindings) ->
    Notify ! {a_got, Msg},
    {Bindings, #{}, []}.

-spec unmount(az:bindings()) -> ok.
unmount(#{notify := Notify}) ->
    Notify ! {unmounted, arizona_swap_a},
    ok.
