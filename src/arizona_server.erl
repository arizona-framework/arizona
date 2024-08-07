-module(arizona_server).
-moduledoc false.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/0]).
-export([start/1]).
-export([route/1]).

%

-ignore_xref([start/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(LISTENER, arizona_http_listener).
-define(PERSIST_KEY, arizona_dispatch).
-elvis([{elvis_style, no_macros, #{allow => ['LISTENER', 'PERSIST_KEY']}}]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type opts() :: #{
    scheme => http | https,
    transport => ranch_tcp:opts(),
    proto => #{
        proto => cowboy:opts(),
        host => '_' | iodata(), % as per cowboy_router:route_match(),
        routes => list(),
        live_reload => boolean()
    }
}.
-export_type([opts/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start() -> pid().
start() ->
    start(arizona_cfg:endpoint()).

-spec start(Opts) -> pid()
    when Opts :: opts().
start(Opts) ->
    do_start(norm_opts(Opts)).

-spec route(Req) -> Routed
    when Req :: cowboy_req:req(),
         Routed :: {Req, Env},
         Env :: cowboy_middleware:env().
route(Req0) ->
    #{path := Path} = cowboy_req:match_qs([path], Req0),
    {ok, Req, Env} = cowboy_router:execute(Req0#{path => Path},
                                           #{dispatch => {persistent_term, ?PERSIST_KEY}}),
    {Req, Env}.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

do_start(#{scheme := http, transport := Transport, proto := Proto}) ->
    {ok, Pid} = cowboy:start_clear(?LISTENER, Transport, Proto),
    Pid;
do_start(#{scheme := https, transport := Transport, proto := Proto}) ->
    {ok, Pid} = cowboy:start_tls(?LISTENER, Transport, Proto),
    Pid.

norm_opts(Opts) ->
    #{
        scheme => maps:get(scheme, Opts, http),
        transport => norm_transport_opts(maps:get(transport, Opts, [])),
        proto => norm_proto_opts(maps:get(proto, Opts, #{}),
                                 maps:get(host, Opts, '_'),
                                 maps:get(routes, Opts, []),
                                 maps:get(live_reload, Opts, false))
    }.

norm_transport_opts([]) ->
    [{port, 8080}];
norm_transport_opts(Opts) ->
    Opts.

norm_proto_opts(Opts, Host, Routes, LiveReload) ->
    Dispatch = cowboy_router:compile([{Host, [
        {"/assets/js/morphdom.min.js", cowboy_static,
         {priv_file, arizona, "static/assets/js/morphdom.min.js"}},
        {"/assets/js/arizona.js", cowboy_static,
         {priv_file, arizona, "static/assets/js/arizona.js"}},
        {"/assets/js/arizona-worker.js", cowboy_static,
         {priv_file, arizona, "static/assets/js/arizona-worker.js"}},
        {"/websocket", arizona_websocket, []}
    ] ++ case LiveReload of
        true ->
            [{"/assets/js/arizona-live-reload.js", cowboy_static,
              {priv_file, arizona, "static/assets/js/arizona-live-reload.js"}}
             | Routes];
         false ->
            Routes
    end}]),
    persistent_term:put(?PERSIST_KEY, Dispatch),
    Opts#{env => #{dispatch => {persistent_term, ?PERSIST_KEY}}}.
