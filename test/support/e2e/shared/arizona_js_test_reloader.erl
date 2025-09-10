-module(arizona_js_test_reloader).
-behaviour(arizona_reloader).
-export([reload/1]).

reload(_Files) ->
    os:cmd("npm run build").
