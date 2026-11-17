-module(arizona_wizard).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_info/2]).

%% A wizard whose every step's handler navigates to the next one, under other
%% layouts: each step the user takes is answered with a page load. `toq` navigates
%% to a page under the same layouts, served live; `#tofrag` is a link to a fragment
%% of the next step, far down its page. With `auto`, the view does both
%% itself once connected, with no user input: the next step at once, then, 2 s
%% later, the page served live. With `hop`, it navigates to the next step on its own
%% once connected, and nothing else; `#tor` and `#to1` link to `/wizard-r` and the
%% first step; `#pop` pops the view out into a Document Picture-in-Picture window.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{id => ~"wizard", step => maps:get(step, Init), other => maps:get(other, Init)},
    Connected = ?connected,
    case
        {
            Connected andalso maps:get(auto, Init, false),
            Connected andalso maps:get(hop, Init, false)
        }
    of
        {true, _} ->
            {arizona_view, _, _} = ?send(next),
            _TRef = ?send_after(2000, toq),
            ok;
        {false, true} ->
            {arizona_view, _, _} = ?send(next),
            ok;
        {false, false} ->
            ok
    end,
    {Bindings, #{}}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(next, Bindings) ->
    {Bindings, #{}, [arizona_js:navigate(maps:get(other, Bindings))]};
handle_info(toq, Bindings) ->
    {Bindings, #{}, [arizona_js:navigate(~"/wizard-q")]}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"next", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:navigate(maps:get(other, Bindings))]};
handle_event(~"toq", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:navigate(~"/wizard-q")]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [{id, ~"step"}], [?get(step)]},
            {button, [{id, ~"next"}, {az_click, arizona_js:push_event(~"next")}], [~"next"]},
            {button, [{id, ~"toq"}, {az_click, arizona_js:push_event(~"toq")}], [~"toq"]},
            {a, [{id, ~"tofrag"}, {href, ~"/wizard-2#sec"}, az_navigate], [~"step 2, #sec"]},
            {a, [{id, ~"tor"}, {href, ~"/wizard-r"}, az_navigate], [~"hop"]},
            {button, [{id, ~"pop"}, {az_click, arizona_js:request_pip(~"wizard")}], [~"pop out"]},
            {a, [{id, ~"to1"}, {href, ~"/wizard-1"}, az_navigate], [~"step 1"]},
            {'div', [{style, ~"height:3000px"}], []},
            {h2, [{id, ~"sec"}], [~"sec"]}
        ]}
    ).
