-module(arizona_config).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([static_dir/0]).
-export([endpoint/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec static_dir() -> file:filename_all().
static_dir() ->
    get_env(static_dir, "static").

-spec endpoint() -> arizona_server:opts().
endpoint() ->
    get_env(endpoint, #{}).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

get_env(Key, Default) ->
    application:get_env(arizona, Key, Default).
