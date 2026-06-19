-module(arizona_users_controller).
-moduledoc """
**TEST FIXTURE.** A resource-style controller with named actions (no `handle/1`),
to verify `arizona_roadrunner_controller` dispatches `Handler:Action/1` from the
route's `action` option. Two same-path verb-tag routes dispatch to different
actions here (`GET` -> `index`, `POST` -> `create`).
""".

-export([index/1]).
-export([create/1]).

-spec index(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
index(Req) ->
    {roadrunner_resp:text(200, ~"users#index"), Req}.

-spec create(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
create(Req) ->
    {roadrunner_resp:text(200, ~"users#create"), Req}.
