-module(arizona_remote_helper).
-moduledoc """
A template helper module, not a component: pages call its functions directly
and splice the returned templates into their own. Its literal builder call
folds into the statics, so no render ever re-proves the name -- only the
compile-time walk can deliver it.
""".
-include("arizona_stateless.hrl").

-export([button/0]).

-spec button() -> az:template().
button() ->
    ?html({button, [{az_dblclick, arizona_js:toggle(~"#x")}], [~"b"]}).
