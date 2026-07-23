-module(arizona_os_demo).
-moduledoc """
E2E fixture for the native-shell (OS) capability seam.

Exercises the full loop against a real browser running the real `arizona.js`
client with a fake `window.__arizona_os__` (the Electron-preload equivalent):

- **Capability negotiation + gated rendering.** `mount/1` seeds `can_window`
  `false` and self-casts on connect (the `?connected` pattern); `handle_info/2`
  flips `can_window` from `?capability(~"window_title")`, so the window-control
  button renders only when the shell advertised that capability -- and never in a
  plain browser.
- **Server-emitted, declarative OS command.** The same `handle_info/2` re-asserts
  the window title with `arizona_os:set_title/1`; safe to repeat on every
  (re)connect (a declarative/idempotent capability, unlike one-shot `notify`).
- **Client-triggered OS command.** The button's `az_click` issues
  `arizona_os:set_title/1` directly (a client effect, no round-trip).
- **Inbound OS event.** A shell-injected event arrives via `handle_event/3` like
  any other event and updates `last_event`.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_info/2]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings0) ->
    %% Construct a fresh map named `Bindings` so the ?get(id) inside ?send
    %% resolves against it (the input has no `id`); pull accepted overrides
    %% from the input explicitly.
    Bindings = #{
        id => ~"os_demo",
        title => maps:get(title, Bindings0, ~"OS"),
        connected => false,
        can_window => false,
        last_event => ~"none"
    },
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {span, [{id, ~"status"}], [
                case ?get(connected) of
                    true -> ~"Connected";
                    false -> ~"Connecting..."
                end
            ]},
            {span, [{id, ~"last-event"}], [?get(last_event)]},
            %% Capability-gated UI: driven from the `can_window` binding (the
            %% ?connected pattern), not raw ?capability in render -- so it
            %% repaints on connect and is simply absent in a plain browser.
            case ?get(can_window) of
                true ->
                    {'div', [{id, ~"os-actions"}], [
                        {button,
                            [
                                {id, ~"set-title"},
                                {az_click, arizona_os:set_title(~"Clicked title")}
                            ],
                            [~"Set title"]},
                        {button, [{id, ~"maximize"}, {az_click, arizona_os:maximize(true)}], [
                            ~"Maximize"
                        ]},
                        {button, [{id, ~"restore"}, {az_click, arizona_os:maximize(false)}], [
                            ~"Restore"
                        ]}
                    ]};
                false ->
                    <<>>
            end
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    {
        Bindings#{
            connected => true,
            can_window => ?capability(~"window_title")
        },
        #{},
        [arizona_os:set_title(~"OS demo (connected)")]
    }.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) -> az:handle_event_ret().
handle_event(~"window_state", #{~"state" := State}, Bindings) ->
    %% A native OS event the shell injected arrives as an ordinary event; its
    %% payload flows through to handle_event/3 like any other event payload.
    {Bindings#{last_event => State}, #{}, []}.
