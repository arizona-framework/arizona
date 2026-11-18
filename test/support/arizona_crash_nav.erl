-module(arizona_crash_nav).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_info/2]).
-export([unmount/1]).
-export([slow/2]).

%% One click raises an event that crashes the view (4500) and then navigates to
%% another view: the navigate, queued behind the crash, never runs. With `loop`,
%% the live view does it itself on connect, every time it mounts, navigating to
%% `other` (itself by default): a crash that recurs after every mount. With
%% `crash_on_leave`, the view crashes as a navigate unmounts it: the navigate
%% itself fails, and the server answers it with the load of its page. With
%% `bounce`, the live view navigates to `other` on connect, every time it mounts
%% (`ping` -- `connected` or `hooklater` -- makes the page's own code send an event
%% first, no person acting, and has the view wait 400 ms before navigating),
%% with no crash of its own. The `arm` event makes the view crash as it is left,
%% until it is remounted. With `tall`, the page scrolls; `slow/2` is a middleware
%% step that answers late, so a navigate to its route is answered well after it
%% is sent.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Loop = maps:get(loop, Init, false),
    Bindings = #{
        id => ~"crash_nav",
        loop => Loop,
        other => maps:get(other, Init, ~"/crash-nav-loop"),
        crash_on_leave => maps:get(crash_on_leave, Init, false),
        tall => maps:get(tall, Init, false),
        ping => maps:get(ping, Init, ~"none")
    },
    (Loop andalso ?connected) andalso ?send(go),
    case {maps:get(bounce, Init, false) andalso ?connected, maps:get(ping, Bindings)} of
        {false, _} ->
            ok;
        {true, ~"none"} ->
            {arizona_view, _, _} = ?send(bounce),
            ok;
        {true, _Ping} ->
            _TRef = ?send_after(400, bounce),
            ok
    end,
    {Bindings, #{}}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"hello", _Payload, Bindings) ->
    {Bindings, #{}, []};
handle_event(~"arm", _Payload, Bindings) ->
    {Bindings#{crash_on_leave => true}, #{}, []};
handle_event(~"crash", _Payload, _Bindings) ->
    error(crash_on_event).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(go, Bindings) ->
    {Bindings, #{}, [
        arizona_js:push_event(~"crash"), arizona_js:navigate(maps:get(other, Bindings))
    ]};
handle_info(bounce, Bindings) ->
    {Bindings, #{}, [arizona_js:navigate(maps:get(other, Bindings))]}.

-spec unmount(az:bindings()) -> az:unmount_ret().
unmount(#{crash_on_leave := true}) ->
    error(crash_on_leave);
unmount(_Bindings) ->
    ok.

-spec slow(arizona_req:request(), az:bindings()) -> arizona_middleware:middleware_result().
slow(Req, Bindings) ->
    ok = timer:sleep(1500),
    {cont, Req, Bindings}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}, {'data-ping', ?get(ping)}], [
            {h1, [], [~"Crash nav"]},
            case ?get(ping) of
                ~"hooklater" -> {span, [{id, ~"ping"}, {az_hook, ~"PingLater"}], []};
                _ -> <<>>
            end,
            {button,
                [
                    {id, ~"crash-about"},
                    {az_click, [arizona_js:push_event(~"crash"), arizona_js:navigate(~"/about")]}
                ],
                [~"crash, then /about"]},
            {button,
                [
                    {id, ~"crash-section"},
                    {az_click, [
                        arizona_js:push_event(~"crash"), arizona_js:navigate(~"/scroll-section")
                    ]}
                ],
                [~"crash, then /scroll-section"]},
            {button,
                [
                    {id, ~"crash-twin"},
                    {az_click, [
                        arizona_js:push_event(~"crash"), arizona_js:navigate(~"/crash-nav-twin")
                    ]}
                ],
                [~"crash, then this view under other layouts"]},
            {button,
                [
                    {id, ~"crash-twin-patch"},
                    {az_click, [
                        arizona_js:push_event(~"crash"), arizona_js:patch(~"/crash-nav-twin")
                    ]}
                ],
                [~"crash, then patch to this view under other layouts"]},
            {a, [{id, ~"leave-about"}, {href, ~"/about"}, az_navigate], [~"/about"]},
            {a, [{id, ~"twin-empty-query"}, {href, ~"/crash-nav-twin?"}, az_navigate], [
                ~"this view under other layouts, an empty query"
            ]},
            {a, [{id, ~"leave-slow"}, {href, ~"/crash-nav-slow"}, az_navigate], [~"slow"]},
            {a, [{id, ~"plain-slow"}, {href, ~"/crash-nav-slow"}], [~"slow, as a plain link"]},
            {button, [{id, ~"crash"}, {az_click, arizona_js:push_event(~"crash")}], [~"crash"]},
            {button, [{id, ~"arm"}, {az_click, arizona_js:push_event(~"arm")}], [
                case ?get(crash_on_leave) of
                    true -> ~"armed";
                    false -> ~"arm"
                end
            ]},
            case ?get(tall) of
                true -> {'div', [{id, ~"spacer"}, {style, ~"height:4000px"}], []};
                false -> <<>>
            end
        ]}
    ).
