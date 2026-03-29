-module(arizona_cowboy_server).
-export([start/2, stop/1, recompile_routes/0]).
-ignore_xref([start/2, stop/1]).

-define(ROUTES_KEY, arizona_routes).

-spec start(atom(), map()) -> {ok, pid()} | {error, term()}.
start(Name, #{routes := Routes} = Opts) ->
    ok = arizona_cowboy_router:compile_routes(Routes),
    persistent_term:put({?ROUTES_KEY, Name}, Routes),
    ErrorPage = maps:get(error_page, Opts, {arizona_error_page, render}),
    persistent_term:put(arizona_error_page, ErrorPage),
    Scheme = maps:get(scheme, Opts, http),
    TransportOpts = maps:get(transport_opts, Opts, [{port, 4040}]),
    UserProtoOpts = maps:get(proto_opts, Opts, #{}),
    Env = maps:get(env, UserProtoOpts, #{}),
    ProtoOpts = UserProtoOpts#{env => Env#{dispatch => {persistent_term, arizona_dispatch}}},
    case Scheme of
        http -> cowboy:start_clear(Name, TransportOpts, ProtoOpts);
        https -> cowboy:start_tls(Name, TransportOpts, ProtoOpts)
    end.

-spec stop(atom()) -> ok.
stop(Name) ->
    persistent_term:erase({?ROUTES_KEY, Name}),
    persistent_term:erase(arizona_error_page),
    cowboy:stop_listener(Name).

-spec recompile_routes() -> ok.
recompile_routes() ->
    Terms = persistent_term:get(),
    lists:foreach(
        fun
            ({{?ROUTES_KEY, _}, Routes}) ->
                arizona_cowboy_router:compile_routes(Routes);
            (_) ->
                ok
        end,
        Terms
    ).
