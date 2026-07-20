-module(arizona_local_xss).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Fixture for the content-`?local` SSR-escaping regression: a `?local` init
%% seeded from a binding is a supported pattern, so user data can reach it. A
%% content local must be HTML-escaped at SSR (matching the client's text-node
%% semantics); an attribute local already is (via render_attr). Both slots here
%% are seeded from the same `payload` binding so a test can pass hostile markup.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"local_xss"),
            payload => maps:get(payload, Bindings, ~"")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [{class, ~"content"}], [?local(~"content", ?get(payload))]},
            {'div', [{class, ~"attr"}, {title, ?local(~"attr", ?get(payload))}], [~"x"]}
        ]}
    ).
