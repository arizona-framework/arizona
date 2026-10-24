-module(arizona_fake_watcher_init_only).
-moduledoc """
Not an `arizona_watcher`: it exports `init/2` but no `handle_info/2`, so the
reloader's config check must reject it.
""".

-export([init/2]).

init(_Dir, Opts) ->
    {ok, Opts}.
