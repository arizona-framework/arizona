-module(arizona_adapter).

-type path() :: binary().

-export_type([path/0]).

%% Resolve a client-side navigation path to handler module + route opts.
%% Called by arizona_socket during SPA-style navigation.
-callback resolve_route(Path :: path(), State :: term()) ->
    {module(), route_opts()}.

-type route_opts() :: #{
    bindings => map(),
    on_mount => arizona_live:on_mount(),
    layout => {module(), atom()},
    _ => term()
}.
-export_type([route_opts/0]).
