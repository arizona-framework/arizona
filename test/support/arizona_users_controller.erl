-module(arizona_users_controller).
-moduledoc """
**TEST FIXTURE.** A resource-style controller with named actions (no `handle/1`),
to verify `arizona_controller:dispatch/2` dispatches `Handler:Action/2` from the
route's `action` option. Two same-path verb-tag routes dispatch to different
actions here (`GET` -> `index`, `POST` -> `create`).
""".

-export([index/2]).
-export([create/2]).

-spec index(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
index(Req, _Ctx) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"users#index"}, Req}.

-spec create(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
create(Req, _Ctx) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"users#create"}, Req}.
