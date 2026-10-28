-module(arizona_reload_admin).
-moduledoc """
**TEST FIXTURE -- DO NOT EXPOSE IN PRODUCTION.** Unauthenticated handler that
swaps running code; lives in `test/support/` and is only wired into the e2e
fixture in `arizona_test_server`.

Drives a dev reload the way `arizona_reloader` does, so the e2e reload spec can
watch a native client remount. The test server runs without the dev reloader, so
`step=arm` enables it (`reloader` app env) for the connection the spec opens next,
and `step=fire` reloads `arizona_reload_fun_source` twice with a different `STEP`
(`code:purge/1` then `code:load_binary/3`, broadcasting after each, as a reload wave
does) and disables it again. The reloader env is node-wide, so every connection
opened between the two joins the reload topic and is closed `1001` -- not only the
spec's own. That is why the spec lives in the `sequential` Playwright project,
which runs after the others and alone.

Always replies `204 No Content`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req0, _Ctx) ->
    {Params, Req1} = arizona_req:params(Req0),
    ok =
        case proplists:get_value(~"step", Params) of
            ~"arm" -> arm();
            ~"fire" -> fire()
        end,
    {{204, [], <<>>}, Req1}.

arm() ->
    application:set_env(arizona, reloader, #{enabled => true, rules => []}).

fire() ->
    ok = reload(arizona_reload_fun_source, 2),
    ok = arizona_reloader:broadcast(),
    ok = reload(arizona_reload_fun_source, 3),
    ok = arizona_reloader:broadcast(),
    application:unset_env(arizona, reloader).

reload(Mod, Step) ->
    Src = proplists:get_value(source, Mod:module_info(compile)),
    {ok, Mod, Bin} = compile:file(Src, [binary, {d, 'STEP', Step}]),
    ok =
        case code:purge(Mod) of
            true -> ok;
            false -> ok
        end,
    {module, Mod} = code:load_binary(Mod, Src, Bin),
    ok.
