-module(arizona_js_test_reloader).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(_Files, _Options) ->
    os:cmd("npm run build").
