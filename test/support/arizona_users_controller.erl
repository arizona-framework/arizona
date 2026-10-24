-module(arizona_users_controller).
-moduledoc """
**TEST FIXTURE.** A resource-style controller with named actions (no `handle/1`),
to verify `arizona_controller:dispatch/2` dispatches `Handler:Action/1` from the
route's `action` option. Two same-path verb-tag routes dispatch to different
actions here (`GET` -> `index`, `POST` -> `create`).
""".

-export([index/1]).
-export([create/1]).

-spec index(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
index(Req) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"users#index"}, Req}.

-spec create(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
create(Req) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"users#create"}, Req}.
