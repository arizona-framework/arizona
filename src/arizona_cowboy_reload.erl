-module(arizona_cowboy_reload).
-export([init/2, info/3]).
-ignore_xref([init/2, info/3]).

init(Req0, State) ->
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>
    },
    Req = cowboy_req:stream_reply(200, Headers, Req0),
    arizona_reloader:join(self()),
    {cowboy_loop, Req, State}.

info({arizona_reloader, reload}, Req, State) ->
    cowboy_req:stream_body(<<"event: reload\ndata: \n\n">>, nofin, Req),
    {ok, Req, State};
info({arizona_reloader, reload_css}, Req, State) ->
    cowboy_req:stream_body(<<"event: reload_css\ndata: \n\n">>, nofin, Req),
    {ok, Req, State};
info(_Info, Req, State) ->
    {ok, Req, State}.
