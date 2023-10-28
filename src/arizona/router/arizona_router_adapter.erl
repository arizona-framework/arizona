-module(arizona_router_adapter).

%% Types
-export_type([]).

-type method() :: arizona_server_adapter:method().
-type path() :: arizona_server_adapter:path().
-type handler() :: mfa() | {live, module(), term()}.
-type options() :: map().

%% Callbacks
-optional_callbacks([]).

-callback match(Method, Path) -> {Handler, Options}
    when Method :: method()
       , Path :: path()
       , Handler :: handler()
       , Options :: options()
       .
