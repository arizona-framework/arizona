-module(arizona_cfg).

-export([endpoint/0]).

-opaque endpoint() :: map().
-export_type([endpoint/0]).

-spec endpoint() -> endpoint().
endpoint() ->
  application:get_env(arizona, endpoint, #{}).

